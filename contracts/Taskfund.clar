(define-constant ERR-NOT-AUTHORIZED (err u100))
(define-constant ERR-TASK-NOT-FOUND (err u101))
(define-constant ERR-INVALID-STATUS (err u102))
(define-constant ERR-INSUFFICIENT-FUNDS (err u103))
(define-constant ERR-ALREADY-FUNDED (err u104))
(define-constant ERR-NOT-CLIENT (err u105))
(define-constant ERR-NOT-FREELANCER (err u106))

(define-data-var dao-fee uint u50)
(define-data-var dao-address principal 'SP000000000000000000002Q6VF78)

(define-map tasks 
    { task-id: uint }
    {
        client: principal,
        freelancer: principal,
        amount: uint,
        status: (string-ascii 20),
        milestone-count: uint,
        current-milestone: uint,
        milestone-amount: uint
    }
)

(define-map task-milestones
    { task-id: uint, milestone: uint }
    {
        description: (string-ascii 100),
        amount: uint,
        status: (string-ascii 20)
    }
)

(define-map task-funds
    { task-id: uint }
    { balance: uint }
)

(define-data-var task-nonce uint u0)

(define-public (create-task (freelancer principal) (total-amount uint) (milestone-count uint))
    (let
        (
            (task-id (var-get task-nonce))
            (milestone-amount (/ total-amount milestone-count))
        )
        (try! (stx-transfer? total-amount tx-sender (as-contract tx-sender)))
        (map-set tasks 
            { task-id: task-id }
            {
                client: tx-sender,
                freelancer: freelancer,
                amount: total-amount,
                status: "active",
                milestone-count: milestone-count,
                current-milestone: u1,
                milestone-amount: milestone-amount
            }
        )
        (map-set task-funds
            { task-id: task-id }
            { balance: total-amount }
        )
        (var-set task-nonce (+ task-id u1))
        (ok task-id)
    )
)

(define-public (add-milestone (task-id uint) (milestone uint) (description (string-ascii 100)))
    (let
        ((task (unwrap! (map-get? tasks { task-id: task-id }) ERR-TASK-NOT-FOUND)))
        (asserts! (is-eq tx-sender (get client task)) ERR-NOT-CLIENT)
        (map-set task-milestones
            { task-id: task-id, milestone: milestone }
            {
                description: description,
                amount: (get milestone-amount task),
                status: "pending"
            }
        )
        (ok true)
    )
)

(define-public (complete-milestone (task-id uint) (milestone uint))
    (let
        (
            (task (unwrap! (map-get? tasks { task-id: task-id }) ERR-TASK-NOT-FOUND))
            (milestone-data (unwrap! (map-get? task-milestones { task-id: task-id, milestone: milestone }) ERR-TASK-NOT-FOUND))
        )
        (asserts! (is-eq tx-sender (get freelancer task)) ERR-NOT-FREELANCER)
        (asserts! (is-eq (get status milestone-data) "pending") ERR-INVALID-STATUS)
        (map-set task-milestones
            { task-id: task-id, milestone: milestone }
            (merge milestone-data { status: "completed" })
        )
        (ok true)
    )
)

(define-public (approve-milestone (task-id uint) (milestone uint))
    (let
        (
            (task (unwrap! (map-get? tasks { task-id: task-id }) ERR-TASK-NOT-FOUND))
            (milestone-data (unwrap! (map-get? task-milestones { task-id: task-id, milestone: milestone }) ERR-TASK-NOT-FOUND))
            (funds (unwrap! (map-get? task-funds { task-id: task-id }) ERR-TASK-NOT-FOUND))
            (fee-amount (/ (* (get amount milestone-data) (var-get dao-fee)) u1000))
            (payment-amount (- (get amount milestone-data) fee-amount))
        )
        (asserts! (is-eq tx-sender (get client task)) ERR-NOT-CLIENT)
        (asserts! (is-eq (get status milestone-data) "completed") ERR-INVALID-STATUS)
        (try! (as-contract (stx-transfer? fee-amount tx-sender (var-get dao-address))))
        (try! (as-contract (stx-transfer? payment-amount tx-sender (get freelancer task))))
        (map-set task-funds
            { task-id: task-id }
            { balance: (- (get balance funds) (get amount milestone-data)) }
        )
        (map-set task-milestones
            { task-id: task-id, milestone: milestone }
            (merge milestone-data { status: "paid" })
        )
        (if (is-eq milestone (get milestone-count task))
            (map-set tasks { task-id: task-id } (merge task { status: "completed" }))
            (map-set tasks { task-id: task-id } (merge task { current-milestone: (+ milestone u1) }))
        )
        (ok true)
    )
)

(define-read-only (get-task (task-id uint))
    (map-get? tasks { task-id: task-id })
)

(define-read-only (get-milestone (task-id uint) (milestone uint))
    (map-get? task-milestones { task-id: task-id, milestone: milestone })
)

(define-read-only (get-task-balance (task-id uint))
    (map-get? task-funds { task-id: task-id })
)