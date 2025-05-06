;; fitnest-core
;; 
;; This contract serves as the central hub for the FitNest ecosystem, handling all aspects of workout 
;; verification, token distribution, and user profile management. It maintains records of user workout history, 
;; calculates appropriate rewards based on consistency and achievement, and manages the token economy 
;; that incentivizes continued fitness engagement.

;; ======================
;; Error Codes
;; ======================

(define-constant ERR-NOT-AUTHORIZED (err u100))
(define-constant ERR-USER-NOT-FOUND (err u101))
(define-constant ERR-UNAUTHORIZED-VERIFIER (err u102))
(define-constant ERR-INVALID-WORKOUT-DURATION (err u103))
(define-constant ERR-WORKOUT-ALREADY-VERIFIED (err u104))
(define-constant ERR-CHALLENGE-NOT-FOUND (err u105))
(define-constant ERR-CHALLENGE-EXPIRED (err u106))
(define-constant ERR-INSUFFICIENT-TOKENS (err u107))
(define-constant ERR-ALREADY-JOINED-CHALLENGE (err u108))
(define-constant ERR-CHALLENGE-NOT-ACTIVE (err u109))
(define-constant ERR-MILESTONE-ALREADY-CLAIMED (err u110))

;; ======================
;; Constants
;; ======================

(define-constant CONTRACT-OWNER tx-sender)
(define-constant BASE-REWARD-AMOUNT u10) ;; Base tokens for completing a workout
(define-constant STREAK-MULTIPLIER u1)   ;; Additional tokens per day of streak
(define-constant MAX-STREAK-BONUS u50)   ;; Maximum bonus from streaks
(define-constant MIN-WORKOUT-DURATION u15) ;; Minimum workout duration in minutes

;; Milestone definitions (workout counts and rewards)
(define-constant MILESTONE-LEVELS (list 
  {count: u10, reward: u50}
  {count: u50, reward: u300}
  {count: u100, reward: u700}
  {count: u250, reward: u2000}
  {count: u500, reward: u5000}
))

;; ======================
;; Data Maps & Variables
;; ======================

;; Map of authorized workout verifiers (oracles)
(define-map authorized-verifiers principal bool)

;; Total token supply and admin account
(define-data-var token-supply uint u0)

;; User profiles storing basic fitness information
(define-map user-profiles
  principal
  {
    total-workouts: uint,
    current-streak: uint,
    longest-streak: uint,
    last-workout-time: uint,
    total-tokens-earned: uint,
    total-duration: uint
  }
)

;; User token balances
(define-map token-balances principal uint)

;; Workout history for each user
(define-map workout-history
  {user: principal, workout-id: uint}
  {
    timestamp: uint,
    duration: uint, 
    workout-type: (string-ascii 20),
    verified: bool,
    tokens-earned: uint
  }
)

;; Track next workout ID for each user
(define-map user-workout-counter principal uint)

;; Track which milestones users have claimed
(define-map claimed-milestones
  {user: principal, milestone-level: uint}
  bool
)

;; Community fitness challenges
(define-map challenges
  uint
  {
    name: (string-ascii 50),
    description: (string-utf8 200),
    start-time: uint,
    end-time: uint,
    stake-amount: uint,
    reward-pool: uint,
    participant-count: uint,
    is-active: bool
  }
)

;; Track challenge participants
(define-map challenge-participants
  {challenge-id: uint, user: principal}
  {
    stake-amount: uint,
    workouts-completed: uint,
    reward-claimed: bool
  }
)

;; Challenge counter
(define-data-var next-challenge-id uint u1)

;; ======================
;; Private Functions
;; ======================

(define-private (min (a uint) (b uint))
  (if (< a b)
    a
    b
  )
)

;; Initialize a new user profile if it doesn't exist
(define-private (initialize-user (user principal))
  (if (is-some (map-get? user-profiles user))
    true ;; User already exists, do nothing
    (begin ;; User doesn't exist, initialize
      (map-set user-profiles user {
        total-workouts: u0,
        current-streak: u0,
        longest-streak: u0,
        last-workout-time: u0,
        total-tokens-earned: u0,
        total-duration: u0
      })
      (map-set token-balances user u0)
      (map-set user-workout-counter user u1)
      true ;; Return true after initialization
    )
  )
)

;; Mint tokens to a user's balance
(define-private (mint-tokens (user principal) (amount uint))
  (let ((current-balance (default-to u0 (map-get? token-balances user))))
    (map-set token-balances user (+ current-balance amount))
    (var-set token-supply (+ (var-get token-supply) amount))
    amount
  )
)

;; Calculate token rewards based on workout duration and user streak
(define-private (calculate-rewards (user principal) (duration uint))
  (let (
    (user-profile (default-to 
      {
        total-workouts: u0,
        current-streak: u0,
        longest-streak: u0,
        last-workout-time: u0,
        total-tokens-earned: u0,
        total-duration: u0
      } 
      (map-get? user-profiles user)))
    (streak-bonus (min MAX-STREAK-BONUS (* STREAK-MULTIPLIER (get current-streak user-profile))))
    (duration-factor (/ duration u60)) ;; Factor based on hours of workout
  )
    (+ BASE-REWARD-AMOUNT (* streak-bonus duration-factor))
  )
)

;; Update user streak based on last workout time
(define-private (update-streak (user principal))
  (let (
    (user-profile (unwrap! (map-get? user-profiles user) ERR-USER-NOT-FOUND))
    (current-time block-height)
    (last-workout (get last-workout-time user-profile))
    (current-streak (get current-streak user-profile))
    (longest-streak (get longest-streak user-profile))
    (one-day-blocks u144) ;; Approximately blocks in a day
    
    ;; Calculate time difference in blocks
    (time-diff (if (> last-workout u0)
                 (- current-time last-workout)
                 u0))
                 
    ;; Determine new streak value
    (new-streak (if (or (is-eq last-workout u0) (> time-diff (* u2 one-day-blocks)))
                  ;; First workout or more than 2 days passed - reset streak to 1
                  u1
                  ;; Check if this is a new day's workout (between 20-28 hours)
                  (if (and (> time-diff one-day-blocks) (< time-diff (* u2 one-day-blocks)))
                    (+ current-streak u1)
                    ;; Same day workout, maintain current streak
                    current-streak)))
                    
    ;; Calculate new longest streak
    (new-longest (if (> new-streak longest-streak)
                   new-streak
                   longest-streak))
  )
    ;; Return the updated streak values wrapped in ok
    (ok {current-streak: new-streak, longest-streak: new-longest})
  )
)

;; Check if user has reached a milestone
(define-private (check-milestone (user principal))
  (let (
    (user-profile (unwrap! (map-get? user-profiles user) ERR-USER-NOT-FOUND))
    (workout-count (get total-workouts user-profile))
  )
    (ok (filter check-unclaimed-milestone MILESTONE-LEVELS))
  )
)

;; Helper for milestone checking
(define-private (check-unclaimed-milestone (milestone {count: uint, reward: uint}))
  (match (map-get? user-profiles tx-sender)
    ;; If user profile exists, perform the check
    user-profile 
      (let (
        (workout-count (get total-workouts user-profile))
        (milestone-count (get count milestone))
        (milestone-claimed (default-to false (map-get? claimed-milestones {user: tx-sender, milestone-level: milestone-count})))
      )
        (and (>= workout-count milestone-count) (not milestone-claimed))
      )
    ;; If user profile doesn't exist, they can't have claimed the milestone
    false
  )
)

;; Fold helper to find the reward for a specific milestone level during fold
(define-private (find-milestone-reward-fold 
    (milestone {count: uint, reward: uint}) 
    (accumulator {target-level: uint, current-found: uint})
  )
  (if (is-eq (get count milestone) (get target-level accumulator))
      ;; If count matches target level, update current-found with the reward
      (merge accumulator {current-found: (get reward milestone)})
      ;; Otherwise, return the accumulator unchanged
      accumulator
  )
)

;; ======================
;; Read-Only Functions
;; ======================

;; Get user profile information
(define-read-only (get-user-profile (user principal))
  (default-to
    {
      total-workouts: u0,
      current-streak: u0,
      longest-streak: u0,
      last-workout-time: u0,
      total-tokens-earned: u0,
      total-duration: u0
    }
    (map-get? user-profiles user)
  )
)

;; Get user token balance
(define-read-only (get-token-balance (user principal))
  (default-to u0 (map-get? token-balances user))
)

;; Get total token supply
(define-read-only (get-token-supply)
  (var-get token-supply)
)

;; Get workout details
(define-read-only (get-workout (user principal) (workout-id uint))
  (map-get? workout-history {user: user, workout-id: workout-id})
)

;; Get challenge details
(define-read-only (get-challenge (challenge-id uint))
  (map-get? challenges challenge-id)
)

;; Check if user has claimed a specific milestone
(define-read-only (is-milestone-claimed (user principal) (milestone-level uint))
  (default-to false (map-get? claimed-milestones {user: user, milestone-level: milestone-level}))
)

;; Get user's current workout streak
(define-read-only (get-user-streak (user principal))
  (let ((profile (get-user-profile user)))
    (get current-streak profile)
  )
)

;; Get user's participation in a challenge
(define-read-only (get-challenge-participation (challenge-id uint) (user principal))
  (map-get? challenge-participants {challenge-id: challenge-id, user: user})
)

;; Check if a principal is an authorized verifier
(define-read-only (is-authorized-verifier (verifier principal))
  (default-to false (map-get? authorized-verifiers verifier))
)

;; ======================
;; Public Functions
;; ======================

;; Register a new user
(define-public (register-user)
  (begin
    (initialize-user tx-sender)
    (ok true)
  )
)

;; Record a new workout (self-reported)
;; Note: This requires verification by an authorized verifier before rewards are given
(define-public (record-workout (duration uint) (workout-type (string-ascii 20)))
  (let (
    (user tx-sender)
    (workout-id (default-to u1 (map-get? user-workout-counter user)))
  )
    ;; Validate input
    (asserts! (>= duration MIN-WORKOUT-DURATION) ERR-INVALID-WORKOUT-DURATION)
    
    ;; Initialize user if needed
    (initialize-user user)
    
    ;; Record the workout
    (map-set workout-history 
      {user: user, workout-id: workout-id}
      {
        timestamp: block-height,
        duration: duration,
        workout-type: workout-type,
        verified: false,
        tokens-earned: u0
      }
    )
    
    ;; Increment workout counter
    (map-set user-workout-counter user (+ workout-id u1))
    
    (ok workout-id)
  )
)

;; Verify a workout (callable by authorized verifiers only)
(define-public (verify-workout (user principal) (workout-id uint))
  (let (
    (verifier tx-sender)
    (workout (unwrap! (map-get? workout-history {user: user, workout-id: workout-id}) ERR-USER-NOT-FOUND))
    (user-profile (unwrap! (map-get? user-profiles user) ERR-USER-NOT-FOUND))
    (streak-info (try! (update-streak user)))
    (verified (get verified workout))
  )
    ;; Check authorization
    (asserts! (is-authorized-verifier verifier) ERR-UNAUTHORIZED-VERIFIER)
    
    ;; Ensure workout hasn't already been verified
    (asserts! (not verified) ERR-WORKOUT-ALREADY-VERIFIED)
    
    ;; Calculate rewards
    (let (
      (duration (get duration workout))
      (rewards (calculate-rewards user duration))
      (current-streak (get current-streak streak-info))
      (longest-streak (get longest-streak streak-info))
      (total-workouts (+ (get total-workouts user-profile) u1))
      (total-duration (+ (get total-duration user-profile) duration))
      (total-tokens-earned (+ (get total-tokens-earned user-profile) rewards))
    )
      ;; Update workout record
      (map-set workout-history 
        {user: user, workout-id: workout-id}
        (merge workout {verified: true, tokens-earned: rewards})
      )
      
      ;; Update user profile
      (map-set user-profiles user
        {
          total-workouts: total-workouts,
          current-streak: current-streak,
          longest-streak: longest-streak,
          last-workout-time: block-height,
          total-tokens-earned: total-tokens-earned,
          total-duration: total-duration
        }
      )
      
      ;; Grant tokens
      (mint-tokens user rewards)
      
      ;; Update challenge progress if user is in any active challenges
      (ok rewards)
    )
  )
)

;; Create a new community challenge
(define-public (create-challenge (name (string-ascii 50)) (description (string-utf8 200)) 
                               (start-time uint) (end-time uint) (stake-amount uint))
  (let (
    (creator tx-sender)
    (challenge-id (var-get next-challenge-id))
  )
    ;; Validate permissions
    (asserts! (is-authorized-verifier creator) ERR-NOT-AUTHORIZED)
    
    ;; Create the challenge
    (map-set challenges challenge-id
      {
        name: name,
        description: description,
        start-time: start-time,
        end-time: end-time,
        stake-amount: stake-amount,
        reward-pool: u0,
        participant-count: u0,
        is-active: true
      }
    )
    
    ;; Increment challenge ID
    (var-set next-challenge-id (+ challenge-id u1))
    
    (ok challenge-id)
  )
)

;; Join a community challenge
(define-public (join-challenge (challenge-id uint))
  (let (
    (user tx-sender)
    (challenge (unwrap! (map-get? challenges challenge-id) ERR-CHALLENGE-NOT-FOUND))
    (stake-amount (get stake-amount challenge))
    (user-balance (default-to u0 (map-get? token-balances user)))
    (is-active (get is-active challenge))
    (current-time block-height)
    (start-time (get start-time challenge))
    (end-time (get end-time challenge))
    ;; Check if the user participation record exists
    (already-joined (is-some 
                     (map-get? challenge-participants {challenge-id: challenge-id, user: user})))
  )
    ;; Validate conditions
    (asserts! is-active ERR-CHALLENGE-NOT-ACTIVE)
    (asserts! (>= current-time start-time) ERR-CHALLENGE-NOT-ACTIVE)
    (asserts! (< current-time end-time) ERR-CHALLENGE-EXPIRED)
    (asserts! (>= user-balance stake-amount) ERR-INSUFFICIENT-TOKENS)
    (asserts! (not already-joined) ERR-ALREADY-JOINED-CHALLENGE)
    
    ;; Update user balance
    (map-set token-balances user (- user-balance stake-amount))
    
    ;; Update challenge info
    (map-set challenges challenge-id 
      (merge challenge {
        reward-pool: (+ (get reward-pool challenge) stake-amount),
        participant-count: (+ (get participant-count challenge) u1)
      })
    )
    
    ;; Add user to participants
    (map-set challenge-participants 
      {challenge-id: challenge-id, user: user}
      {
        stake-amount: stake-amount,
        workouts-completed: u0,
        reward-claimed: false
      }
    )
    
    (ok true)
  )
)

;; Record workout for a challenge
(define-public (record-challenge-workout (challenge-id uint) (workout-id uint))
  (let (
    (user tx-sender)
    (challenge (unwrap! (map-get? challenges challenge-id) ERR-CHALLENGE-NOT-FOUND))
    (participation (unwrap! (map-get? challenge-participants {challenge-id: challenge-id, user: user}) ERR-USER-NOT-FOUND))
    (workout (unwrap! (map-get? workout-history {user: user, workout-id: workout-id}) ERR-USER-NOT-FOUND))
    (verified (get verified workout))
    (current-time block-height)
    (end-time (get end-time challenge))
    (is-active (get is-active challenge))
  )
    ;; Validate conditions
    (asserts! is-active ERR-CHALLENGE-NOT-ACTIVE)
    (asserts! (< current-time end-time) ERR-CHALLENGE-EXPIRED)
    (asserts! verified ERR-WORKOUT-ALREADY-VERIFIED)
    
    ;; Update participation info
    (map-set challenge-participants 
      {challenge-id: challenge-id, user: user}
      (merge participation {
        workouts-completed: (+ (get workouts-completed participation) u1)
      })
    )
    
    (ok true)
  )
)

;; Add an authorized verifier (only callable by contract owner)
(define-public (add-verifier (verifier principal))
  (begin
    (asserts! (is-eq tx-sender CONTRACT-OWNER) ERR-NOT-AUTHORIZED)
    (map-set authorized-verifiers verifier true)
    (ok true)
  )
)

;; Remove an authorized verifier (only callable by contract owner)
(define-public (remove-verifier (verifier principal))
  (begin
    (asserts! (is-eq tx-sender CONTRACT-OWNER) ERR-NOT-AUTHORIZED)
    (map-delete authorized-verifiers verifier)
    (ok true)
  )
)

;; Transfer tokens between users
(define-public (transfer-tokens (recipient principal) (amount uint))
  (let (
    (sender tx-sender)
    (sender-balance (default-to u0 (map-get? token-balances sender)))
  )
    ;; Validate conditions
    (asserts! (>= sender-balance amount) ERR-INSUFFICIENT-TOKENS)
    
    ;; Update balances
    (map-set token-balances sender (- sender-balance amount))
    (let ((recipient-balance (default-to u0 (map-get? token-balances recipient))))
      (map-set token-balances recipient (+ recipient-balance amount))
    )
    
    (ok true)
  )
)

;; End a challenge (only callable by contract owner or authorized verifiers)
(define-public (end-challenge (challenge-id uint))
  (let (
    (caller tx-sender)
    (challenge (unwrap! (map-get? challenges challenge-id) ERR-CHALLENGE-NOT-FOUND))
  )
    ;; Validate permissions
    (asserts! (or (is-eq caller CONTRACT-OWNER) (is-authorized-verifier caller)) ERR-NOT-AUTHORIZED)
    
    ;; Update challenge status
    (map-set challenges challenge-id
      (merge challenge {is-active: false})
    )
    
    (ok true)
  )
)