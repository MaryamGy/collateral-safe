;; -------------------------------------------------------------
;; Contract: collateral-safe.clar
;; Description: Simple collateralized lending vault (STX-based)
;; - Users deposit STX as collateral
;; - Owner funds liquidity pool used for lending
;; - Users borrow STX against collateral (enforced collateral ratio)
;; - Users repay loans by sending STX back
;; - Anyone can liquidate under-collateralized positions by repaying debt
;; Notes:
;; - All amounts are in micro-STX (1 STX = 1_000_000 micro-STX)
;; - Collateral ratio is represented as percentage (e.g., 150 = 150%)
;; -------------------------------------------------------------

(define-constant ERR-NOT-OWNER u100)
(define-constant ERR-ZERO-AMOUNT u101)
(define-constant ERR-NO-VAULT u102)
(define-constant ERR-INSUFFICIENT-COLLATERAL u103)
(define-constant ERR-INACTIVE-LIQUIDITY u104)
(define-constant ERR-INSUFFICIENT-LIQUIDITY u105)
(define-constant ERR-NO-DEBT u106)
(define-constant ERR-UNDER_COLLATERALIZED u107)
(define-constant ERR-ALREADY-INITIALIZED u108)
(define-constant ERR-INVALID_REPAYMENT u109)

;; -------------------------
;; State
;; -------------------------

(define-data-var owner (optional principal) none)
(define-data-var liquidity-pool uint u0)        ;; STX available to lend (micro-STX)
(define-data-var collateralization-rate uint u150) ;; e.g., 150 means 150%

;; Per-user vault: collateral (micro-STX) and debt (micro-STX)
(define-map vaults
  { user: principal }
  { collateral: uint, debt: uint })

;; -------------------------
;; Helpers
;; -------------------------

(define-private (get-vault-or-default (user principal))
  (match (map-get? vaults { user: user })
    some-v some-v
    { collateral: u0, debt: u0 }
  )
)

;; Check if collateral covers required ratio for given debt:
;; returns true if collateral * 100 >= debt * collateralization-rate
(define-private (is-collateral-sufficient (collateral uint) (debt uint))
  (let ((rate (var-get collateralization-rate)))
    (if (<= debt u0)
        (ok true) ;; no debt -> sufficient
        (ok (>= (* collateral u100) (* debt rate)))
    )
  )
)

;; -------------------------
;; Initialization
;; -------------------------

(define-public (initialize (admin principal))
  (if (is-eq admin tx-sender)
      (match (var-get owner)
        some-existing (err ERR-ALREADY-INITIALIZED)
        (begin
          (var-set owner (some admin))
          (ok admin)
        )
      )
      (err ERR-NOT-OWNER)
  )
)

(define-read-only (get-owner) (ok (var-get owner)))

;; -------------------------
;; Owner functions
;; -------------------------

;; Owner funds liquidity pool by sending STX with this call
(define-public (fund-liquidity (amount uint))
  (let ((caller tx-sender))
    (if (<= amount u0)
        (err ERR-ZERO-AMOUNT)
        (match (var-get owner)
          o (if (is-eq caller o)
                (begin
                  (var-set liquidity-pool (+ (var-get liquidity-pool) amount))
                  (ok (var-get liquidity-pool))
                )
                (err ERR-NOT-OWNER)
            )
          (err ERR-NOT-OWNER)
        )
    )
  )
)

;; Owner can withdraw excess liquidity (careful: don't withdraw funds backing outstanding debt)
(define-public (owner-withdraw (amount uint))
  (match (var-get owner)
    o (if (not (is-eq tx-sender o))
          (err ERR-NOT-OWNER)
          (let ((pool (var-get liquidity-pool)))
            (if (> amount pool)
                (err ERR-INSUFFICIENT-LIQUIDITY)
                (begin
                  (var-set liquidity-pool (- pool amount))
                  (stx-transfer? amount (as-contract tx-sender) o)
                )
            )
          )
      )
    (err ERR-NOT-OWNER)
  )
)

;; Owner can set collateralization rate (e.g., 150)
(define-public (set-collateralization-rate (new-rate uint))
  (if (> new-rate u0)
      (match (var-get owner)
        o (if (is-eq tx-sender o)
              (begin
                (var-set collateralization-rate new-rate)
                (ok new-rate)
              )
              (err ERR-NOT-OWNER)
          )
        (err ERR-NOT-OWNER)
      )
      (err ERR-ZERO-AMOUNT)
  )
)

;; -------------------------
;; User functions
;; -------------------------

;; Deposit STX as collateral (send STX with the call)
(define-public (deposit (amount uint))
  (let ((sender tx-sender))
    (if (<= amount u0)
        (err ERR-ZERO-AMOUNT)
        (let ((maybe-v (map-get? vaults { user: sender })))
          (let ((prev-c (match maybe-v v (get collateral v) u0))
                (prev-d (match maybe-v v (get debt v) u0)))
            (map-set vaults { user: sender } { collateral: (+ prev-c amount), debt: prev-d })
            (ok (+ prev-c amount))
          )
        )
    )
  )
)

;; Withdraw collateral (only allowed if collateral after withdrawal still covers debt)
(define-public (withdraw (amount uint))
  (let ((sender tx-sender))
    (if (<= amount u0)
        (err ERR-ZERO-AMOUNT)
        (let ((maybe-vault (map-get? vaults { user: sender })))
          (if (is-none maybe-vault)
              (err ERR-NO-VAULT)
              (let ((some-v (unwrap-panic maybe-vault)))
                (let ((coll (get collateral some-v))
                      (debt (get debt some-v)))
                  (if (< coll amount)
                      (err ERR-INSUFFICIENT-COLLATERAL)
                      (let ((new-coll (- coll amount)))
                        (let ((sufficient (unwrap-panic (is-collateral-sufficient new-coll debt))))
                          (if (not sufficient)
                              (err ERR-INSUFFICIENT-COLLATERAL)
                              (begin
                                (map-set vaults { user: sender } { collateral: new-coll, debt: debt })
                                (var-set liquidity-pool (+ (var-get liquidity-pool) u0))
                                (stx-transfer? amount (as-contract tx-sender) sender)
                              )
                          )
                        )
                      )
                  )
                )
              )
          )
        )
    )
  )
)

;; Borrow STX against collateral. Requires liquidity in pool.
;; Borrow amount is sent from contract liquidity to borrower.
(define-public (borrow (amount uint))
  (let ((sender tx-sender))
    (if (<= amount u0)
        (err ERR-ZERO-AMOUNT)
        (let ((pool (var-get liquidity-pool)))
          (if (< pool amount)
              (err ERR-INSUFFICIENT-LIQUIDITY)
              (let ((maybe-vault (map-get? vaults { user: sender })))
                (if (is-none maybe-vault)
                    (err ERR-NO-VAULT)
                    (let ((some-v (unwrap-panic maybe-vault)))
                      (let ((coll (get collateral some-v))
                            (debt (get debt some-v))
                            (new-debt (+ debt amount)))
                        (let ((sufficient (unwrap-panic (is-collateral-sufficient coll new-debt))))
                          (if (not sufficient)
                              (err ERR-INSUFFICIENT-COLLATERAL)
                              (begin
                                (var-set liquidity-pool (- pool amount))
                                (map-set vaults { user: sender } { collateral: coll, debt: new-debt })
                                (stx-transfer? amount (as-contract tx-sender) sender)
                              )
                          )
                        )
                      )
                    )
                )
              )
          )
        )
    )
  )
)

;; Repay debt by sending STX (amount = ctx-get-transfer-amount)
(define-public (repay (amount uint))
  (let ((sender tx-sender))
    (if (<= amount u0)
        (err ERR-ZERO-AMOUNT)
        (let ((maybe-vault (map-get? vaults { user: sender })))
          (if (is-none maybe-vault)
              (err ERR-NO-VAULT)
              (let ((some-v (unwrap-panic maybe-vault)))
                (let ((debt (get debt some-v)))
                  (if (<= debt u0)
                      (err ERR-NO-DEBT)
                      (let ((repay-amount (if (> amount debt) debt amount))
                            (excess (if (> amount debt) (- amount debt) u0)))
                        (var-set liquidity-pool (+ (var-get liquidity-pool) repay-amount))
                        (let ((new-debt (- debt repay-amount)))
                          (map-set vaults { user: sender } { collateral: (get collateral some-v), debt: new-debt })
                          (if (> excess u0)
                              (try! (stx-transfer? excess (as-contract tx-sender) sender))
                              true
                          )
                          (ok new-debt)
                        )
                      )
                  )
                )
              )
          )
        )
    )
  )
)

;; -------------------------
;; Liquidation
;; -------------------------
;; Anyone can liquidate an under-collateralized vault by sending a STX payment
;; equal to the target's debt. On success the liquidator receives the collateral.
;; (Simple model: require liquidator to fully repay debt.)

(define-public (liquidate (target principal) (repay-amount uint))
  (let ((liquidator tx-sender))
    (if (is-eq target liquidator)
        (err ERR-NOT-OWNER)
        (if (<= repay-amount u0)
            (err ERR-INVALID_REPAYMENT)
            (let ((maybe-vault (map-get? vaults { user: target })))
              (if (is-none maybe-vault)
                  (err ERR-NO-VAULT)
                  (let ((some-v (unwrap-panic maybe-vault)))
                    (let ((coll (get collateral some-v))
                          (debt (get debt some-v))
                          (rate (var-get collateralization-rate)))
                      (if (<= debt u0)
                          (err ERR-NO-DEBT)
                          (if (>= (* coll u100) (* debt rate))
                              (err ERR-UNDER_COLLATERALIZED)
                              (if (< repay-amount debt)
                                  (err ERR-INVALID_REPAYMENT)
                                  (begin
                                    (var-set liquidity-pool (+ (var-get liquidity-pool) debt))
                                    (map-delete vaults { user: target })
                                    (var-set liquidity-pool (+ (var-get liquidity-pool) u0))
                                    (stx-transfer? coll (as-contract tx-sender) liquidator)
                                  )
                              )
                          )
                      )
                    )
                  )
              )
            )
        )
    )
  )
)

;; -------------------------
;; Read-only helpers
;; -------------------------

(define-read-only (get-vault (user principal))
  (let ((maybe-vault (map-get? vaults { user: user })))
    (if (is-none maybe-vault)
        (ok { collateral: u0, debt: u0 })
        (ok (unwrap-panic maybe-vault))
    )
  )
)

(define-read-only (get-liquidity-pool) (ok (var-get liquidity-pool)))
(define-read-only (get-collateralization-rate) (ok (var-get collateralization-rate)))
