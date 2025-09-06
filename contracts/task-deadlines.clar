;; task-deadlines.clar - Task Deadline Management & Automatic Enforcement

;; Error constants
(define-constant ERR-NOT-AUTHORIZED (err u200))
(define-constant ERR-TASK-NOT-FOUND (err u201))
(define-constant ERR-DEADLINE-ALREADY-SET (err u202))
(define-constant ERR-INVALID-DEADLINE (err u203))
(define-constant ERR-DEADLINE-PASSED (err u204))
(define-constant ERR-NO-DEADLINE-SET (err u205))
(define-constant ERR-MILESTONE-NOT-FOUND (err u206))
(define-constant ERR-DEADLINE-NOT-REACHED (err u207))

;; Constants for deadline management
(define-constant DEFAULT-PENALTY-RATE u10) ;; 10% penalty
(define-constant MIN-DEADLINE-BUFFER u144) ;; 1 day in blocks (assuming 10min blocks)
(define-constant MAX-EXTENSION-PERIOD u1008) ;; 7 days in blocks
(define-constant GRACE-PERIOD u72) ;; 12 hours in blocks

;; Task deadline tracking
(define-map task-deadlines
    { task-id: uint }
    {
        final-deadline: uint,
        penalty-rate: uint,
        extensions-used: uint,
        max-extensions: uint,
        set-by: principal,
        created-at: uint
    }
)

;; Milestone-specific deadlines
(define-map milestone-deadlines
    { task-id: uint, milestone: uint }
    {
        deadline: uint,
        buffer-time: uint,
        is-critical: bool,
        violation-count: uint
    }
)

;; Deadline violations and penalties
(define-map deadline-violations
    { task-id: uint }
    {
        total-violations: uint,
        penalty-amount: uint,
        last-violation: uint,
        auto-release-triggered: bool
    }
)

;; Auto-release settings
(define-map auto-release-settings
    { task-id: uint }
    {
        enabled: bool,
        release-after-violations: uint,
        release-percentage: uint,
        configured-by: principal
    }
)

;; Public function to set task deadline
(define-public (set-task-deadline (task-id uint) (deadline uint) (penalty-rate uint))
    (let
        ((task-info (unwrap! (contract-call? .Taskfund get-task task-id) ERR-TASK-NOT-FOUND))
         (current-time stacks-block-height))
        
        (asserts! (is-eq tx-sender (get client task-info)) ERR-NOT-AUTHORIZED)
        (asserts! (> deadline (+ current-time MIN-DEADLINE-BUFFER)) ERR-INVALID-DEADLINE)
        (asserts! (<= penalty-rate u50) ERR-INVALID-DEADLINE) ;; Max 50% penalty
        (asserts! (is-none (map-get? task-deadlines { task-id: task-id })) ERR-DEADLINE-ALREADY-SET)
        
        (map-set task-deadlines
            { task-id: task-id }
            {
                final-deadline: deadline,
                penalty-rate: penalty-rate,
                extensions-used: u0,
                max-extensions: u2,
                set-by: tx-sender,
                created-at: current-time
            }
        )
        (ok true)
    )
)

;; Set milestone deadline
(define-public (set-milestone-deadline (task-id uint) (milestone uint) (deadline uint) (is-critical bool))
    (let
        ((task-info (unwrap! (contract-call? .Taskfund get-task task-id) ERR-TASK-NOT-FOUND))
         (task-deadline (map-get? task-deadlines { task-id: task-id }))
         (current-time stacks-block-height))
        
        (asserts! (is-eq tx-sender (get client task-info)) ERR-NOT-AUTHORIZED)
        (asserts! (> deadline current-time) ERR-INVALID-DEADLINE)
        
        ;; Ensure milestone deadline doesn't exceed task deadline
        (match task-deadline
            deadline-data (asserts! (<= deadline (get final-deadline deadline-data)) ERR-INVALID-DEADLINE)
            true ;; No task deadline set, allow any milestone deadline
        )
        
        (map-set milestone-deadlines
            { task-id: task-id, milestone: milestone }
            {
                deadline: deadline,
                buffer-time: GRACE-PERIOD,
                is-critical: is-critical,
                violation-count: u0
            }
        )
        (ok true)
    )
)

;; Request deadline extension
(define-public (request-deadline-extension (task-id uint) (extension-blocks uint) (reason (string-ascii 150)))
    (let
        ((task-info (unwrap! (contract-call? .Taskfund get-task task-id) ERR-TASK-NOT-FOUND))
         (deadline-data (unwrap! (map-get? task-deadlines { task-id: task-id }) ERR-NO-DEADLINE-SET)))
        
        (asserts! (is-eq tx-sender (get freelancer task-info)) ERR-NOT-AUTHORIZED)
        (asserts! (< (get extensions-used deadline-data) (get max-extensions deadline-data)) ERR-NOT-AUTHORIZED)
        (asserts! (<= extension-blocks MAX-EXTENSION-PERIOD) ERR-INVALID-DEADLINE)
        (asserts! (< stacks-block-height (get final-deadline deadline-data)) ERR-DEADLINE-PASSED)
        
        (map-set task-deadlines
            { task-id: task-id }
            (merge deadline-data {
                final-deadline: (+ (get final-deadline deadline-data) extension-blocks),
                extensions-used: (+ (get extensions-used deadline-data) u1)
            })
        )
        (ok true)
    )
)

;; Check for deadline violations and apply penalties
(define-public (process-deadline-violation (task-id uint))
    (let
        ((task-info (unwrap! (contract-call? .Taskfund get-task task-id) ERR-TASK-NOT-FOUND))
         (deadline-data (unwrap! (map-get? task-deadlines { task-id: task-id }) ERR-NO-DEADLINE-SET))
         (task-balance (unwrap! (contract-call? .Taskfund get-task-balance task-id) ERR-TASK-NOT-FOUND))
         (violation-data (default-to 
             { total-violations: u0, penalty-amount: u0, last-violation: u0, auto-release-triggered: false }
             (map-get? deadline-violations { task-id: task-id })))
         (current-time stacks-block-height))
        
        (asserts! (> current-time (+ (get final-deadline deadline-data) GRACE-PERIOD)) ERR-DEADLINE-NOT-REACHED)
        
        (let
            ((penalty-amount (/ (* (get balance task-balance) (get penalty-rate deadline-data)) u100))
             (new-violation-count (+ (get total-violations violation-data) u1)))
            
            (map-set deadline-violations
                { task-id: task-id }
                {
                    total-violations: new-violation-count,
                    penalty-amount: (+ (get penalty-amount violation-data) penalty-amount),
                    last-violation: current-time,
                    auto-release-triggered: false
                }
            )
            (ok { violations: new-violation-count, penalty: penalty-amount })
        )
    )
)

;; Configure automatic fund release on deadline violations
(define-public (configure-auto-release (task-id uint) (violation-threshold uint) (release-percentage uint))
    (let
        ((task-info (unwrap! (contract-call? .Taskfund get-task task-id) ERR-TASK-NOT-FOUND)))
        
        (asserts! (is-eq tx-sender (get client task-info)) ERR-NOT-AUTHORIZED)
        (asserts! (<= release-percentage u100) ERR-INVALID-DEADLINE)
        (asserts! (> violation-threshold u0) ERR-INVALID-DEADLINE)
        
        (map-set auto-release-settings
            { task-id: task-id }
            {
                enabled: true,
                release-after-violations: violation-threshold,
                release-percentage: release-percentage,
                configured-by: tx-sender
            }
        )
        (ok true)
    )
)

;; Process automatic fund release based on violations
(define-public (trigger-auto-release (task-id uint))
    (let
        ((task-info (unwrap! (contract-call? .Taskfund get-task task-id) ERR-TASK-NOT-FOUND))
         (auto-settings (unwrap! (map-get? auto-release-settings { task-id: task-id }) ERR-NOT-AUTHORIZED))
         (violation-data (unwrap! (map-get? deadline-violations { task-id: task-id }) ERR-NOT-AUTHORIZED))
         (task-balance (unwrap! (contract-call? .Taskfund get-task-balance task-id) ERR-TASK-NOT-FOUND)))
        
        (asserts! (get enabled auto-settings) ERR-NOT-AUTHORIZED)
        (asserts! (>= (get total-violations violation-data) (get release-after-violations auto-settings)) ERR-NOT-AUTHORIZED)
        (asserts! (not (get auto-release-triggered violation-data)) ERR-NOT-AUTHORIZED)
        
        (let
            ((release-amount (/ (* (get balance task-balance) (get release-percentage auto-settings)) u100)))
            
            (try! (as-contract (stx-transfer? release-amount tx-sender (get client task-info))))
            (map-set deadline-violations
                { task-id: task-id }
                (merge violation-data { auto-release-triggered: true })
            )
            (ok release-amount)
        )
    )
)

;; Read-only functions for deadline information
(define-read-only (get-task-deadline (task-id uint))
    (map-get? task-deadlines { task-id: task-id })
)

(define-read-only (get-milestone-deadline (task-id uint) (milestone uint))
    (map-get? milestone-deadlines { task-id: task-id, milestone: milestone })
)

(define-read-only (get-deadline-violations (task-id uint))
    (map-get? deadline-violations { task-id: task-id })
)

(define-read-only (get-auto-release-settings (task-id uint))
    (map-get? auto-release-settings { task-id: task-id })
)

(define-read-only (is-deadline-violated (task-id uint))
    (match (map-get? task-deadlines { task-id: task-id })
        deadline-data (> stacks-block-height (+ (get final-deadline deadline-data) GRACE-PERIOD))
        false
    )
)

(define-read-only (calculate-time-remaining (task-id uint))
    (match (map-get? task-deadlines { task-id: task-id })
        deadline-data 
            (if (> (get final-deadline deadline-data) stacks-block-height)
                (some (- (get final-deadline deadline-data) stacks-block-height))
                (some u0))
        none
    )
)

(define-read-only (get-deadline-status (task-id uint))
    (let
        ((deadline-data (map-get? task-deadlines { task-id: task-id }))
         (violation-data (map-get? deadline-violations { task-id: task-id }))
         (current-time stacks-block-height))
        
        {
            has-deadline: (is-some deadline-data),
            is-violated: (is-deadline-violated task-id),
            time-remaining: (calculate-time-remaining task-id),
            violations: violation-data,
            current-block: current-time
        }
    )
)
