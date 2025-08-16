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
(define-constant ERR-ALREADY-RATED (err u115))
(define-constant ERR-INVALID-RATING (err u116))
(define-constant ERR-TASK-NOT-COMPLETED (err u117))
(define-constant ERR-CANNOT-RATE-SELF (err u118))
(define-constant ERR-RATING-PERIOD-EXPIRED (err u119))

(define-data-var dao-fee uint u50)
(define-data-var dao-address principal 'SP000000000000000000002Q6VF78)
(define-data-var arbitrator-stake-amount uint u1000000)
(define-data-var dispute-voting-period uint u144)
(define-data-var dispute-nonce uint u0)
(define-data-var rating-window uint u1008)
(define-data-var rating-nonce uint u0)
(define-data-var min-rating uint u1)
(define-data-var max-rating uint u5)

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

(define-map user-profiles
    { user: principal }
    {
        total-tasks-completed: uint,
        total-tasks-created: uint,
        total-earnings: uint,
        total-spent: uint,
        average-rating: uint,
        total-ratings-received: uint,
        total-ratings-given: uint,
        reputation-score: uint,
        last-active: uint,
        profile-created: uint
    }
)

(define-map task-ratings
    { task-id: uint }
    {
        client-rating: uint,
        freelancer-rating: uint,
        client-review: (string-ascii 200),
        freelancer-review: (string-ascii 200),
        client-rated: bool,
        freelancer-rated: bool,
        rating-deadline: uint,
        completed-at: uint
    }
)

(define-map rating-history
    { rating-id: uint }
    {
        task-id: uint,
        rater: principal,
        ratee: principal,
        rating: uint,
        review: (string-ascii 200),
        rating-type: (string-ascii 10),
        created-at: uint
    }
)

(define-map performance-metrics
    { user: principal, metric-type: (string-ascii 20) }
    {
        value: uint,
        count: uint,
        last-updated: uint
    }
)

(define-map reputation-rewards
    { user: principal }
    {
        current-tier: (string-ascii 20),
        bonus-rate: uint,
        total-rewards: uint,
        tier-updated: uint
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

(define-public (initialize-user-profile)
    (let
        ((existing-profile (map-get? user-profiles { user: tx-sender })))
        (if (is-none existing-profile)
            (begin
                (map-set user-profiles
                    { user: tx-sender }
                    {
                        total-tasks-completed: u0,
                        total-tasks-created: u0,
                        total-earnings: u0,
                        total-spent: u0,
                        average-rating: u0,
                        total-ratings-received: u0,
                        total-ratings-given: u0,
                        reputation-score: u100,
                        last-active: stacks-block-height,
                        profile-created: stacks-block-height
                    }
                )
                (map-set reputation-rewards
                    { user: tx-sender }
                    {
                        current-tier: "bronze",
                        bonus-rate: u0,
                        total-rewards: u0,
                        tier-updated: stacks-block-height
                    }
                )
                (ok true)
            )
            (ok false)
        )
    )
)

(define-public (submit-task-rating (task-id uint) (rating uint) (review (string-ascii 200)))
    (let
        (
            (task (unwrap! (map-get? tasks { task-id: task-id }) ERR-TASK-NOT-FOUND))
            (task-rating-data (default-to 
                {
                    client-rating: u0,
                    freelancer-rating: u0,
                    client-review: "",
                    freelancer-review: "",
                    client-rated: false,
                    freelancer-rated: false,
                    rating-deadline: (+ stacks-block-height (var-get rating-window)),
                    completed-at: stacks-block-height
                }
                (map-get? task-ratings { task-id: task-id })
            ))
            (rating-id (var-get rating-nonce))
        )
        (asserts! (is-eq (get status task) "completed") ERR-TASK-NOT-COMPLETED)
        (asserts! (>= rating (var-get min-rating)) ERR-INVALID-RATING)
        (asserts! (<= rating (var-get max-rating)) ERR-INVALID-RATING)
        (asserts! (<= stacks-block-height (get rating-deadline task-rating-data)) ERR-RATING-PERIOD-EXPIRED)
        (asserts! (or (is-eq tx-sender (get client task)) (is-eq tx-sender (get freelancer task))) ERR-NOT-AUTHORIZED)
        
        (if (is-eq tx-sender (get client task))
            (begin
                (asserts! (not (get client-rated task-rating-data)) ERR-ALREADY-RATED)
                (map-set task-ratings
                    { task-id: task-id }
                    (merge task-rating-data {
                        client-rating: rating,
                        client-review: review,
                        client-rated: true
                    })
                )
                (map-set rating-history
                    { rating-id: rating-id }
                    {
                        task-id: task-id,
                        rater: tx-sender,
                        ratee: (get freelancer task),
                        rating: rating,
                        review: review,
                        rating-type: "client",
                        created-at: stacks-block-height
                    }
                )
                (unwrap-panic (update-user-rating (get freelancer task) rating))
            )
            (begin
                (asserts! (not (get freelancer-rated task-rating-data)) ERR-ALREADY-RATED)
                (map-set task-ratings
                    { task-id: task-id }
                    (merge task-rating-data {
                        freelancer-rating: rating,
                        freelancer-review: review,
                        freelancer-rated: true
                    })
                )
                (map-set rating-history
                    { rating-id: rating-id }
                    {
                        task-id: task-id,
                        rater: tx-sender,
                        ratee: (get client task),
                        rating: rating,
                        review: review,
                        rating-type: "freelancer",
                        created-at: stacks-block-height
                    }
                )
                (unwrap-panic (update-user-rating (get client task) rating))
            )
        )
        (var-set rating-nonce (+ rating-id u1))
        (unwrap-panic (update-user-profile-activity tx-sender))
        (ok rating-id)
    )
)

(define-private (update-user-rating (user principal) (new-rating uint))
    (let
        (
            (profile (default-to 
                {
                    total-tasks-completed: u0,
                    total-tasks-created: u0,
                    total-earnings: u0,
                    total-spent: u0,
                    average-rating: u0,
                    total-ratings-received: u0,
                    total-ratings-given: u0,
                    reputation-score: u100,
                    last-active: stacks-block-height,
                    profile-created: stacks-block-height
                }
                (map-get? user-profiles { user: user })
            ))
            (total-ratings (+ (get total-ratings-received profile) u1))
            (total-rating-points (+ (* (get average-rating profile) (get total-ratings-received profile)) new-rating))
            (new-average (/ total-rating-points total-ratings))
            (reputation-adjustment (if (> new-rating u3) u10 u0))
            (new-reputation (+ (get reputation-score profile) reputation-adjustment))
        )
        (map-set user-profiles
            { user: user }
            (merge profile {
                average-rating: new-average,
                total-ratings-received: total-ratings,
                reputation-score: new-reputation,
                last-active: stacks-block-height
            })
        )
        (let ((tier-result (update-reputation-tier user new-reputation)))
            (ok true)
        )
    )
)

(define-private (update-user-profile-activity (user principal))
    (let
        (
            (profile (unwrap! (map-get? user-profiles { user: user }) ERR-NOT-AUTHORIZED))
        )
        (map-set user-profiles
            { user: user }
            (merge profile {
                total-ratings-given: (+ (get total-ratings-given profile) u1),
                last-active: stacks-block-height
            })
        )
        (ok true)
    )
)

(define-private (update-reputation-tier (user principal) (reputation uint))
    (let
        (
            (current-rewards (default-to 
                {
                    current-tier: "bronze",
                    bonus-rate: u0,
                    total-rewards: u0,
                    tier-updated: stacks-block-height
                }
                (map-get? reputation-rewards { user: user })
            ))
            (new-tier (if (>= reputation u1000) "platinum"
                        (if (>= reputation u500) "gold"
                          (if (>= reputation u200) "silver" "bronze"))))
            (new-bonus-rate (if (is-eq new-tier "platinum") u20
                              (if (is-eq new-tier "gold") u15
                                (if (is-eq new-tier "silver") u10 u5))))
        )
        (map-set reputation-rewards
            { user: user }
            (merge current-rewards {
                current-tier: new-tier,
                bonus-rate: new-bonus-rate,
                tier-updated: stacks-block-height
            })
        )
        (ok true)
    )
)

(define-public (update-performance-metric (user principal) (metric-type (string-ascii 20)) (value uint))
    (let
        (
            (current-metric (default-to 
                {
                    value: u0,
                    count: u0,
                    last-updated: stacks-block-height
                }
                (map-get? performance-metrics { user: user, metric-type: metric-type })
            ))
        )
        (map-set performance-metrics
            { user: user, metric-type: metric-type }
            {
                value: (+ (get value current-metric) value),
                count: (+ (get count current-metric) u1),
                last-updated: stacks-block-height
            }
        )
        (ok true)
    )
)

(define-public (calculate-reputation-bonus (user principal) (base-amount uint))
    (let
        (
            (rewards (default-to 
                {
                    current-tier: "bronze",
                    bonus-rate: u0,
                    total-rewards: u0,
                    tier-updated: stacks-block-height
                }
                (map-get? reputation-rewards { user: user })
            ))
            (bonus-amount (/ (* base-amount (get bonus-rate rewards)) u100))
        )
        (map-set reputation-rewards
            { user: user }
            (merge rewards { total-rewards: (+ (get total-rewards rewards) bonus-amount) })
        )
        (ok bonus-amount)
    )
)

(define-read-only (get-user-profile (user principal))
    (map-get? user-profiles { user: user })
)

(define-read-only (get-task-rating (task-id uint))
    (map-get? task-ratings { task-id: task-id })
)

(define-read-only (get-rating-details (rating-id uint))
    (map-get? rating-history { rating-id: rating-id })
)

(define-read-only (get-performance-metric (user principal) (metric-type (string-ascii 20)))
    (map-get? performance-metrics { user: user, metric-type: metric-type })
)

(define-read-only (get-reputation-rewards (user principal))
    (map-get? reputation-rewards { user: user })
)

(define-read-only (calculate-trust-score (user principal))
    (let
        (
            (profile (map-get? user-profiles { user: user }))
        )
        (match profile
            user-data 
                (let
                    (
                        (rating-factor (if (> (get total-ratings-received user-data) u0) 
                                        (* (get average-rating user-data) u20) u0))
                        (activity-factor (if (> (* (get total-tasks-completed user-data) u5) u100) u100 (* (get total-tasks-completed user-data) u5)))
                        (reputation-factor (/ (get reputation-score user-data) u10))
                    )
                    (some (+ rating-factor (+ activity-factor reputation-factor)))
                )
            none
        )
    )
)

(define-read-only (get-user-reputation-summary (user principal))
    (let
        (
            (profile (map-get? user-profiles { user: user }))
            (rewards (map-get? reputation-rewards { user: user }))
            (trust-score (calculate-trust-score user))
        )
        {
            profile: profile,
            rewards: rewards,
            trust-score: trust-score
        }
    )
)


