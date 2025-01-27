;; SavityPact - Decentralized Savings Smart Contract
;; Contract for managing personal and group savings goals with customizable unlock conditions

(define-constant contract-owner tx-sender)
(define-constant err-owner-only (err u100))
(define-constant err-invalid-goal (err u101))
(define-constant err-unauthorized (err u102))
(define-constant err-goal-exists (err u103))
(define-constant err-insufficient-funds (err u104))
(define-constant err-goal-locked (err u105))
(define-constant err-max-goals-reached (err u106))

;; Data structures
(define-map savings-goals
    { goal-id: uint }
    {
        owner: principal,
        target-amount: uint,
        current-amount: uint,
        unlock-height: uint,
        is-group-goal: bool,
        participants: (list 20 principal),
        threshold-percent: uint
    }
)

(define-map user-goals
    { user: principal }
    (list 20 uint)  ;; List of goal IDs
)

;; Counter for generating unique goal IDs
(define-data-var goal-counter uint u0)

;; Read-only functions
(define-read-only (get-goal (goal-id uint))
    (map-get? savings-goals { goal-id: goal-id })
)

(define-read-only (get-user-goals (user principal))
    (default-to (list) (map-get? user-goals { user: user }))
)

;; Helper function to safely append to a list with max size check
(define-private (safe-append (lst (list 20 uint)) (item uint))
    (let
        (
            (current-length (len lst))
        )
        (asserts! (< current-length u20) err-max-goals-reached)
        (ok (unwrap! (as-max-len? (append lst item) u20) err-max-goals-reached))
    )
)

;; Create a new personal savings goal
(define-public (create-personal-goal (target-amount uint) (unlock-height uint) (threshold-percent uint))
    (let
        (
            (new-goal-id (var-get goal-counter))
            (current-goals (get-user-goals tx-sender))
        )
        (asserts! (> target-amount u0) err-invalid-goal)
        (asserts! (>= unlock-height stacks-block-height) err-invalid-goal)
        (asserts! (<= threshold-percent u100) err-invalid-goal)
        (asserts! (< (len current-goals) u20) err-max-goals-reached)
        
        (map-set savings-goals
            { goal-id: new-goal-id }
            {
                owner: tx-sender,
                target-amount: target-amount,
                current-amount: u0,
                unlock-height: unlock-height,
                is-group-goal: false,
                participants: (list tx-sender),
                threshold-percent: threshold-percent
            }
        )
        
        ;; Update user's goal list with size check
        (let
            (
                (updated-goals (try! (safe-append current-goals new-goal-id)))
            )
            (map-set user-goals
                { user: tx-sender }
                updated-goals
            )
        )
        
        (var-set goal-counter (+ new-goal-id u1))
        (ok new-goal-id)
    )
)

;; Create a group savings goal
(define-public (create-group-goal (target-amount uint) (unlock-height uint) (participants (list 20 principal)))
    (let
        (
            (new-goal-id (var-get goal-counter))
        )
        (asserts! (> target-amount u0) err-invalid-goal)
        (asserts! (>= unlock-height stacks-block-height) err-invalid-goal)
        
        ;; Check if all participants can add more goals
        (try! (fold check-participant-capacity participants (ok true)))
        
        (map-set savings-goals
            { goal-id: new-goal-id }
            {
                owner: tx-sender,
                target-amount: target-amount,
                current-amount: u0,
                unlock-height: unlock-height,
                is-group-goal: true,
                participants: participants,
                threshold-percent: u100
            }
        )
        
        ;; Add goal to all participants' lists
        (try! (process-participants participants new-goal-id))
        
        (var-set goal-counter (+ new-goal-id u1))
        (ok new-goal-id)
    )
)

;; Helper function to check if a participant can add more goals
(define-private (check-participant-capacity (participant principal) (previous-result (response bool uint)))
    (match previous-result
        success (let
            (
                (current-goals (get-user-goals participant))
            )
            (if (< (len current-goals) u20)
                (ok true)
                err-max-goals-reached
            )
        )
        error (err error)
    )
)

;; Helper function to process all participants
(define-private (process-participants (participants (list 20 principal)) (goal-id uint))
    (let
        (
            (result (fold add-goal-to-participant participants (ok true)))
        )
        result
    )
)

;; Helper function to add goal to participant's list
(define-private (add-goal-to-participant (participant principal) (previous-result (response bool uint)))
    (match previous-result
        success (let
            (
                (current-goals (get-user-goals participant))
            )
            (asserts! (< (len current-goals) u20) err-max-goals-reached)
            (let
                (
                    (updated-goals (try! (safe-append current-goals (var-get goal-counter))))
                )
                (map-set user-goals
                    { user: participant }
                    updated-goals
                )
                (ok true)
            )
        )
        error (err error)
    )
)

;; Deposit funds to a goal
(define-public (deposit (goal-id uint) (amount uint))
    (let
        (
            (goal (unwrap! (get-goal goal-id) err-invalid-goal))
        )
        (asserts! (or
            (is-eq (get owner goal) tx-sender)
            (is-some (index-of (get participants goal) tx-sender))
        ) err-unauthorized)
        
        ;; Transfer STX from sender to contract
        (try! (stx-transfer? amount tx-sender (as-contract tx-sender)))
        
        ;; Update goal amount
        (map-set savings-goals
            { goal-id: goal-id }
            (merge goal {
                current-amount: (+ (get current-amount goal) amount)
            })
        )
        (ok true)
    )
)

;; Withdraw funds from a goal
(define-public (withdraw (goal-id uint) (amount uint))
    (let
        (
            (goal (unwrap! (get-goal goal-id) err-invalid-goal))
        )
        (asserts! (is-eq (get owner goal) tx-sender) err-unauthorized)
        (asserts! (<= amount (get current-amount goal)) err-insufficient-funds)
        
        ;; Check if withdrawal is allowed
        (asserts! (or
            (>= stacks-block-height (get unlock-height goal))
            (>= (* u100 amount) (* (get threshold-percent goal) (get current-amount goal)))
        ) err-goal-locked)
        
        ;; Transfer STX from contract to sender
        (try! (as-contract (stx-transfer? amount (as-contract tx-sender) tx-sender)))
        
        ;; Update goal amount
        (map-set savings-goals
            { goal-id: goal-id }
            (merge goal {
                current-amount: (- (get current-amount goal) amount)
            })
        )
        (ok true)
    )
)