(define-constant ERR-NOT-AUTHORIZED (err u100))
(define-constant ERR-TASK-NOT-FOUND (err u101))
(define-constant ERR-INVALID-STATUS (err u102))
(define-constant ERR-INSUFFICIENT-FUNDS (err u103))
(define-constant ERR-ALREADY-FUNDED (err u104))
(define-constant ERR-NOT-CLIENT (err u105))
(define-constant ERR-NOT-FREELANCER (err u106))
(define-constant ERR-DISPUTE-NOT-FOUND (err u107))
(define-constant ERR-ALREADY-VOTED (err u108))
(define-constant ERR-NOT-ARBITRATOR (err u109))
(define-constant ERR-DISPUTE-RESOLVED (err u110))
(define-constant ERR-ARBITRATOR-EXISTS (err u111))
(define-constant ERR-INSUFFICIENT-STAKE (err u112))
(define-constant ERR-VOTING-PERIOD-ENDED (err u113))
(define-constant ERR-VOTING-PERIOD-ACTIVE (err u114))

(define-data-var dao-fee uint u50)
(define-data-var dao-address principal 'SP000000000000000000002Q6VF78)
(define-data-var arbitrator-stake-amount uint u1000000)
(define-data-var dispute-voting-period uint u144)
(define-data-var dispute-nonce uint u0)

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

(define-map arbitrators
    { arbitrator: principal }
    {
        stake: uint,
        reputation: uint,
        active: bool,
        cases-handled: uint
    }
)

(define-map disputes
    { dispute-id: uint }
    {
        task-id: uint,
        disputant: principal,
        dispute-type: (string-ascii 20),
        description: (string-ascii 200),
        status: (string-ascii 20),
        created-at: uint,
        voting-end: uint,
        selected-arbitrators: (list 3 principal),
        client-votes: uint,
        freelancer-votes: uint,
        total-votes: uint,
        resolution: (string-ascii 20)
    }
)

(define-map dispute-votes
    { dispute-id: uint, arbitrator: principal }
    {
        vote: (string-ascii 20),
        reasoning: (string-ascii 150)
    }
)

(define-map arbitrator-assignments
    { dispute-id: uint }
    {
        arbitrator-1: principal,
        arbitrator-2: principal,
        arbitrator-3: principal
    }
)

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

(define-public (register-arbitrator)
    (let
        ((stake-amount (var-get arbitrator-stake-amount)))
        (asserts! (is-none (map-get? arbitrators { arbitrator: tx-sender })) ERR-ARBITRATOR-EXISTS)
        (try! (stx-transfer? stake-amount tx-sender (as-contract tx-sender)))
        (map-set arbitrators
            { arbitrator: tx-sender }
            {
                stake: stake-amount,
                reputation: u0,
                active: true,
                cases-handled: u0
            }
        )
        (ok true)
    )
)

(define-public (deactivate-arbitrator)
    (let
        ((arbitrator-data (unwrap! (map-get? arbitrators { arbitrator: tx-sender }) ERR-NOT-ARBITRATOR)))
        (asserts! (get active arbitrator-data) ERR-NOT-ARBITRATOR)
        (try! (as-contract (stx-transfer? (get stake arbitrator-data) tx-sender tx-sender)))
        (map-set arbitrators
            { arbitrator: tx-sender }
            (merge arbitrator-data { active: false })
        )
        (ok true)
    )
)

(define-public (create-dispute (task-id uint) (dispute-type (string-ascii 20)) (description (string-ascii 200)))
    (let
        (
            (task (unwrap! (map-get? tasks { task-id: task-id }) ERR-TASK-NOT-FOUND))
            (dispute-id (var-get dispute-nonce))
            (voting-end (+ stacks-block-height (var-get dispute-voting-period)))
        )
        (asserts! (or (is-eq tx-sender (get client task)) (is-eq tx-sender (get freelancer task))) ERR-NOT-AUTHORIZED)
        (map-set disputes
            { dispute-id: dispute-id }
            {
                task-id: task-id,
                disputant: tx-sender,
                dispute-type: dispute-type,
                description: description,
                status: "active",
                created-at: stacks-block-height,
                voting-end: voting-end,
                selected-arbitrators: (list),
                client-votes: u0,
                freelancer-votes: u0,
                total-votes: u0,
                resolution: "pending"
            }
        )
        (var-set dispute-nonce (+ dispute-id u1))
        (ok dispute-id)
    )
)

(define-public (assign-arbitrators (dispute-id uint) (arbitrator-1 principal) (arbitrator-2 principal) (arbitrator-3 principal))
    (let
        ((dispute (unwrap! (map-get? disputes { dispute-id: dispute-id }) ERR-DISPUTE-NOT-FOUND)))
        (asserts! (is-eq tx-sender (var-get dao-address)) ERR-NOT-AUTHORIZED)
        (asserts! (is-eq (get status dispute) "active") ERR-DISPUTE-RESOLVED)
        (asserts! (get active (unwrap! (map-get? arbitrators { arbitrator: arbitrator-1 }) ERR-NOT-ARBITRATOR)) ERR-NOT-ARBITRATOR)
        (asserts! (get active (unwrap! (map-get? arbitrators { arbitrator: arbitrator-2 }) ERR-NOT-ARBITRATOR)) ERR-NOT-ARBITRATOR)
        (asserts! (get active (unwrap! (map-get? arbitrators { arbitrator: arbitrator-3 }) ERR-NOT-ARBITRATOR)) ERR-NOT-ARBITRATOR)
        (map-set arbitrator-assignments
            { dispute-id: dispute-id }
            {
                arbitrator-1: arbitrator-1,
                arbitrator-2: arbitrator-2,
                arbitrator-3: arbitrator-3
            }
        )
        (map-set disputes
            { dispute-id: dispute-id }
            (merge dispute { selected-arbitrators: (list arbitrator-1 arbitrator-2 arbitrator-3) })
        )
        (ok true)
    )
)

(define-public (vote-on-dispute (dispute-id uint) (vote (string-ascii 20)) (reasoning (string-ascii 150)))
    (let
        (
            (dispute (unwrap! (map-get? disputes { dispute-id: dispute-id }) ERR-DISPUTE-NOT-FOUND))
            (assignments (unwrap! (map-get? arbitrator-assignments { dispute-id: dispute-id }) ERR-DISPUTE-NOT-FOUND))
            (arbitrator-data (unwrap! (map-get? arbitrators { arbitrator: tx-sender }) ERR-NOT-ARBITRATOR))
        )
        (asserts! (is-eq (get status dispute) "active") ERR-DISPUTE-RESOLVED)
        (asserts! (< stacks-block-height (get voting-end dispute)) ERR-VOTING-PERIOD-ENDED)
        (asserts! (or
            (is-eq tx-sender (get arbitrator-1 assignments))
            (is-eq tx-sender (get arbitrator-2 assignments))
            (is-eq tx-sender (get arbitrator-3 assignments))
        ) ERR-NOT-ARBITRATOR)
        (asserts! (is-none (map-get? dispute-votes { dispute-id: dispute-id, arbitrator: tx-sender })) ERR-ALREADY-VOTED)
        (map-set dispute-votes
            { dispute-id: dispute-id, arbitrator: tx-sender }
            { vote: vote, reasoning: reasoning }
        )
        (let
            (
                (updated-client-votes (if (is-eq vote "client") (+ (get client-votes dispute) u1) (get client-votes dispute)))
                (updated-freelancer-votes (if (is-eq vote "freelancer") (+ (get freelancer-votes dispute) u1) (get freelancer-votes dispute)))
                (updated-total-votes (+ (get total-votes dispute) u1))
            )
            (map-set disputes
                { dispute-id: dispute-id }
                (merge dispute {
                    client-votes: updated-client-votes,
                    freelancer-votes: updated-freelancer-votes,
                    total-votes: updated-total-votes
                })
            )
            (map-set arbitrators
                { arbitrator: tx-sender }
                (merge arbitrator-data { cases-handled: (+ (get cases-handled arbitrator-data) u1) })
            )
        )
        (ok true)
    )
)

(define-public (resolve-dispute (dispute-id uint))
    (let
        (
            (dispute (unwrap! (map-get? disputes { dispute-id: dispute-id }) ERR-DISPUTE-NOT-FOUND))
            (task (unwrap! (map-get? tasks { task-id: (get task-id dispute) }) ERR-TASK-NOT-FOUND))
            (funds (unwrap! (map-get? task-funds { task-id: (get task-id dispute) }) ERR-TASK-NOT-FOUND))
        )
        (asserts! (is-eq (get status dispute) "active") ERR-DISPUTE-RESOLVED)
        (asserts! (>= stacks-block-height (get voting-end dispute)) ERR-VOTING-PERIOD-ACTIVE)
        (asserts! (>= (get total-votes dispute) u2) ERR-INSUFFICIENT-FUNDS)
        (let
            (
                (resolution (if (> (get client-votes dispute) (get freelancer-votes dispute)) "client" "freelancer"))
                (refund-amount (get balance funds))
            )
            (if (is-eq resolution "client")
                (try! (as-contract (stx-transfer? refund-amount tx-sender (get client task))))
                (try! (as-contract (stx-transfer? refund-amount tx-sender (get freelancer task))))
            )
            (map-set disputes
                { dispute-id: dispute-id }
                (merge dispute { status: "resolved", resolution: resolution })
            )
            (map-set tasks
                { task-id: (get task-id dispute) }
                (merge task { status: "disputed" })
            )
            (map-set task-funds
                { task-id: (get task-id dispute) }
                { balance: u0 }
            )
            (ok resolution)
        )
    )
)

(define-read-only (get-arbitrator (arbitrator principal))
    (map-get? arbitrators { arbitrator: arbitrator })
)

(define-read-only (get-dispute (dispute-id uint))
    (map-get? disputes { dispute-id: dispute-id })
)

(define-read-only (get-dispute-vote (dispute-id uint) (arbitrator principal))
    (map-get? dispute-votes { dispute-id: dispute-id, arbitrator: arbitrator })
)

(define-read-only (get-arbitrator-assignments (dispute-id uint))
    (map-get? arbitrator-assignments { dispute-id: dispute-id })
)

(define-read-only (is-arbitrator-active (arbitrator principal))
    (match (map-get? arbitrators { arbitrator: arbitrator })
        arbitrator-data (get active arbitrator-data)
        false
    )
)