(define-map pools
  {pool-id: uint}
  {
    creator: principal,
    premium: uint,
    total-funds: uint,
    members: (list 100 principal)
  }
)

(define-map claims
  {claim-id: uint}
  {
    pool-id: uint,
    claimant: principal,
    description: (string-utf8 200),
    votes-for: uint,
    votes-against: uint,
    resolved: bool,
    approved: bool
  }
)

(define-map member-contributions
  {pool-id: uint, member: principal}
  uint
)

(define-data-var pool-counter uint u0)
(define-data-var claim-counter uint u0)

;; Create a new insurance pool
(define-public (create-pool (premium uint))
  (let ((pool-id (+ (var-get pool-counter) u1)))
    (map-set pools
      {pool-id: pool-id}
      {
        creator: tx-sender,
        premium: premium,
        total-funds: u0,
        members: (list tx-sender)
      }
    )
    (var-set pool-counter pool-id)
    (ok pool-id)
  )
)

;; Join an existing pool by paying the premium
(define-public (join-pool (pool-id uint))
  (let ((pool (map-get? pools {pool-id: pool-id})))
    (match pool
      pool-data
      (begin
        (let ((premium (get premium pool-data))
              (current-members (get members pool-data)))
          (asserts! (not (is-some (index-of current-members tx-sender))) (err "Already a member"))
          (match (stx-transfer? premium tx-sender (get creator pool-data))
            success
            (let ((updated-members (unwrap-panic (as-max-len? (concat current-members (list tx-sender)) u100))))
              (map-set pools
                {pool-id: pool-id}
                (merge pool-data
                  {
                    total-funds: (+ (get total-funds pool-data) premium),
                    members: updated-members
                  }
                )
              )
              (map-set member-contributions {pool-id: pool-id, member: tx-sender} premium)
              (ok "Joined pool"))
            error (err "Failed to transfer STX"))
        )
      )
      (err "Invalid pool")
    )
  )
)

;; Submit a claim to a pool
(define-public (submit-claim (pool-id uint) (description (string-utf8 200)))
  (let ((pool (map-get? pools {pool-id: pool-id})))
    (match pool
      pool-data
      (begin
        (let ((claim-id (+ (var-get claim-counter) u1)))
          (map-set claims
            {claim-id: claim-id}
            {
              pool-id: pool-id,
              claimant: tx-sender,
              description: description,
              votes-for: u0,
              votes-against: u0,
              resolved: false,
              approved: false
            }
          )
          (var-set claim-counter claim-id)
          (ok claim-id)
        )
      )
      (err "Invalid pool")
    )
  )
)

;; Vote on a claim (yes/no)
(define-public (vote-on-claim (claim-id uint) (approve bool))
  (let ((claim (map-get? claims {claim-id: claim-id})))
    (match claim
      claim-data
      (let ((pool (map-get? pools {pool-id: (get pool-id claim-data)})))
        (match pool
          pool-data
          (begin
            (asserts! (is-some (index-of (get members pool-data) tx-sender)) (err "Not a pool member"))
            (asserts! (not (get resolved claim-data)) (err "Claim already resolved"))
            (if approve
                (map-set claims
                  {claim-id: claim-id}
                  (merge claim-data {votes-for: (+ (get votes-for claim-data) u1)})
                )
                (map-set claims
                  {claim-id: claim-id}
                  (merge claim-data {votes-against: (+ (get votes-against claim-data) u1)})
                )
            )
            (ok "Vote submitted")
          )
          (err "Invalid pool")
        )
      )
      (err "Invalid claim")
    )
  )
)

;; Resolve and execute payout if approved
(define-public (execute-payout (claim-id uint))
  (let ((claim (map-get? claims {claim-id: claim-id})))
    (match claim
      claim-data
      (let ((pool (map-get? pools {pool-id: (get pool-id claim-data)})))
        (match pool
          pool-data
          (begin
            (if (is-eq (get resolved claim-data) true)
                (err "Claim already resolved")
                (let ((total-votes (+ (get votes-for claim-data) (get votes-against claim-data))))
                  (if (>= (get votes-for claim-data) (/ total-votes u2))
                      ;; Approve claim and payout
                      (begin
                        (match (stx-transfer? (/ (get total-funds pool-data) u10) (get creator pool-data) (get claimant claim-data))
                          success
                          (begin
                            (map-set claims
                              {claim-id: claim-id}
                              (merge claim-data {resolved: true, approved: true})
                            )
                            (ok "Claim approved & payout done")
                          )
                          error (err "Failed to transfer payout")
                        )
                      )
                      ;; Reject claim
                      (begin
                        (map-set claims
                          {claim-id: claim-id}
                          (merge claim-data {resolved: true, approved: false})
                        )
                        (ok "Claim rejected")
                      )
                  )
                )
            )
          )
          (err "Invalid pool")
        )
      )
      (err "Invalid claim")
    )
  )
)
