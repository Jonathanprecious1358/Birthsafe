;; Prenatal Care Rewards System
;; Incentivizes regular prenatal checkups and health screenings during pregnancy

(define-constant ERR_UNAUTHORIZED (err u100))
(define-constant ERR_NOT_FOUND (err u101))
(define-constant ERR_ALREADY_EXISTS (err u102))
(define-constant ERR_INVALID_DATA (err u200))
(define-constant ERR_PREGNANCY_NOT_ACTIVE (err u201))
(define-constant ERR_APPOINTMENT_EXPIRED (err u202))
(define-constant ERR_REWARD_ALREADY_CLAIMED (err u203))
(define-constant ERR_INSUFFICIENT_CHECKUPS (err u204))

;; Prenatal care milestone constants
(define-constant FIRST_TRIMESTER_REWARD u200000)   ;; 0.2 SBT
(define-constant SECOND_TRIMESTER_REWARD u300000)  ;; 0.3 SBT
(define-constant THIRD_TRIMESTER_REWARD u400000)   ;; 0.4 SBT
(define-constant COMPLETION_BONUS u500000)         ;; 0.5 SBT

;; Checkup type constants
(define-constant CHECKUP_ROUTINE u0)
(define-constant CHECKUP_ULTRASOUND u1)
(define-constant CHECKUP_BLOODWORK u2)
(define-constant CHECKUP_SCREENING u3)
(define-constant CHECKUP_SPECIALIST u4)

(define-data-var pregnancy-counter uint u0)
(define-data-var checkup-counter uint u0)

;; Track pregnancies and prenatal care
(define-map pregnancies
  uint
  {
    mother: principal,
    healthcare-provider: principal,
    estimated-due-date: (string-ascii 16),
    pregnancy-start-block: uint,
    active: bool,
    risk-level: (string-ascii 16),
    total-checkups: uint,
    first-trimester-complete: bool,
    second-trimester-complete: bool,
    third-trimester-complete: bool,
    completion-bonus-claimed: bool
  }
)

;; Track individual prenatal appointments
(define-map prenatal-checkups
  uint
  {
    pregnancy-id: uint,
    mother: principal,
    healthcare-provider: principal,
    checkup-type: uint,
    checkup-date: (string-ascii 16),
    pregnancy-week: uint,
    checkup-block: uint,
    vitals-recorded: bool,
    findings-hash: (string-ascii 64),
    follow-up-required: bool,
    reward-claimed: bool,
    reward-amount: uint
  }
)

;; Map pregnancies to mothers
(define-map mother-pregnancies
  principal
  (list 10 uint)
)

;; Track maternal health records
(define-map maternal-health-records
  { pregnancy-id: uint }
  {
    pre-pregnancy-weight: uint,
    current-weight: uint,
    blood-pressure: (string-ascii 16),
    blood-sugar: uint,
    hemoglobin: uint,
    complications: (string-ascii 128),
    medications: (string-ascii 256),
    last-update-block: uint
  }
)

;; Prenatal care milestones
(define-map care-milestones
  { pregnancy-id: uint, milestone: uint }
  {
    milestone-name: (string-ascii 64),
    target-week: uint,
    completed: bool,
    completion-block: uint,
    reward-amount: uint,
    reward-claimed: bool
  }
)

;; Register new pregnancy for tracking
(define-public (register-pregnancy (mother principal) (estimated-due-date (string-ascii 16)) (risk-level (string-ascii 16)))
  (let
    (
      (pregnancy-id (+ (var-get pregnancy-counter) u1))
      (hospital-data (unwrap! (contract-call? .Birthsafe get-hospital tx-sender) ERR_UNAUTHORIZED))
    )
    (asserts! (get verified hospital-data) ERR_UNAUTHORIZED)
    (asserts! (> (len estimated-due-date) u0) ERR_INVALID_DATA)
    
    ;; Create pregnancy record
    (map-set pregnancies
      pregnancy-id
      {
        mother: mother,
        healthcare-provider: tx-sender,
        estimated-due-date: estimated-due-date,
        pregnancy-start-block: stacks-block-height,
        active: true,
        risk-level: risk-level,
        total-checkups: u0,
        first-trimester-complete: false,
        second-trimester-complete: false,
        third-trimester-complete: false,
        completion-bonus-claimed: false
      }
    )
    
    ;; Initialize care milestones
    (let 
      (
        (milestone-result (setup-pregnancy-milestones pregnancy-id))
        (current-pregnancies (default-to (list) (map-get? mother-pregnancies mother)))
      )
      (map-set mother-pregnancies mother (unwrap! (as-max-len? (append current-pregnancies pregnancy-id) u10) ERR_INVALID_DATA))
    )
    
    (var-set pregnancy-counter pregnancy-id)
    (ok pregnancy-id)
  )
)

;; Record prenatal checkup
(define-public (record-prenatal-checkup (pregnancy-id uint) (checkup-type uint) (checkup-date (string-ascii 16)) (pregnancy-week uint) (vitals-recorded bool) (findings-hash (string-ascii 64)) (follow-up-required bool))
  (let
    (
      (pregnancy-data (unwrap! (map-get? pregnancies pregnancy-id) ERR_NOT_FOUND))
      (checkup-id (+ (var-get checkup-counter) u1))
      (reward-amount (calculate-checkup-reward checkup-type pregnancy-week (get risk-level pregnancy-data)))
    )
    (asserts! (is-eq tx-sender (get healthcare-provider pregnancy-data)) ERR_UNAUTHORIZED)
    (asserts! (get active pregnancy-data) ERR_PREGNANCY_NOT_ACTIVE)
    (asserts! (<= checkup-type CHECKUP_SPECIALIST) ERR_INVALID_DATA)
    (asserts! (> pregnancy-week u0) ERR_INVALID_DATA)
    (asserts! (<= pregnancy-week u42) ERR_INVALID_DATA)
    
    ;; Record checkup
    (map-set prenatal-checkups
      checkup-id
      {
        pregnancy-id: pregnancy-id,
        mother: (get mother pregnancy-data),
        healthcare-provider: tx-sender,
        checkup-type: checkup-type,
        checkup-date: checkup-date,
        pregnancy-week: pregnancy-week,
        checkup-block: stacks-block-height,
        vitals-recorded: vitals-recorded,
        findings-hash: findings-hash,
        follow-up-required: follow-up-required,
        reward-claimed: false,
        reward-amount: reward-amount
      }
    )
    
    ;; Update pregnancy statistics
    (map-set pregnancies
      pregnancy-id
      (merge pregnancy-data { total-checkups: (+ (get total-checkups pregnancy-data) u1) })
    )
    
    ;; Check and update trimester milestones
    (try! (check-trimester-milestones pregnancy-id pregnancy-week))
    
    (var-set checkup-counter checkup-id)
    (ok checkup-id)
  )
)

;; Claim reward for completed checkup
(define-public (claim-checkup-reward (checkup-id uint))
  (let
    (
      (checkup-data (unwrap! (map-get? prenatal-checkups checkup-id) ERR_NOT_FOUND))
      (reward-amount (get reward-amount checkup-data))
    )
    (asserts! (is-eq tx-sender (get mother checkup-data)) ERR_UNAUTHORIZED)
    (asserts! (not (get reward-claimed checkup-data)) ERR_REWARD_ALREADY_CLAIMED)
    (asserts! (> reward-amount u0) ERR_INVALID_DATA)
    
    ;; Mint reward tokens to mother
    (try! (contract-call? .Birthsafe transfer reward-amount (as-contract tx-sender) tx-sender none))
    
    ;; Mark reward as claimed
    (map-set prenatal-checkups
      checkup-id
      (merge checkup-data { reward-claimed: true })
    )
    
    (ok reward-amount)
  )
)

;; Claim milestone reward
(define-public (claim-milestone-reward (pregnancy-id uint) (milestone uint))
  (let
    (
      (pregnancy-data (unwrap! (map-get? pregnancies pregnancy-id) ERR_NOT_FOUND))
      (milestone-data (unwrap! (map-get? care-milestones { pregnancy-id: pregnancy-id, milestone: milestone }) ERR_NOT_FOUND))
      (reward-amount (get reward-amount milestone-data))
    )
    (asserts! (is-eq tx-sender (get mother pregnancy-data)) ERR_UNAUTHORIZED)
    (asserts! (get completed milestone-data) ERR_INSUFFICIENT_CHECKUPS)
    (asserts! (not (get reward-claimed milestone-data)) ERR_REWARD_ALREADY_CLAIMED)
    
    ;; Mint milestone reward
    (try! (contract-call? .Birthsafe transfer reward-amount (as-contract tx-sender) tx-sender none))
    
    ;; Mark reward as claimed
    (map-set care-milestones
      { pregnancy-id: pregnancy-id, milestone: milestone }
      (merge milestone-data { reward-claimed: true })
    )
    
    (ok reward-amount)
  )
)

;; Update maternal health record
(define-public (update-health-record (pregnancy-id uint) (current-weight uint) (blood-pressure (string-ascii 16)) (blood-sugar uint) (hemoglobin uint) (complications (string-ascii 128)) (medications (string-ascii 256)))
  (let
    (
      (pregnancy-data (unwrap! (map-get? pregnancies pregnancy-id) ERR_NOT_FOUND))
      (current-record (default-to
        { pre-pregnancy-weight: u0, current-weight: u0, blood-pressure: "", blood-sugar: u0, hemoglobin: u0, complications: "", medications: "", last-update-block: u0 }
        (map-get? maternal-health-records { pregnancy-id: pregnancy-id })
      ))
    )
    (asserts! (is-eq tx-sender (get healthcare-provider pregnancy-data)) ERR_UNAUTHORIZED)
    (asserts! (get active pregnancy-data) ERR_PREGNANCY_NOT_ACTIVE)
    
    (map-set maternal-health-records
      { pregnancy-id: pregnancy-id }
      (merge current-record {
        current-weight: current-weight,
        blood-pressure: blood-pressure,
        blood-sugar: blood-sugar,
        hemoglobin: hemoglobin,
        complications: complications,
        medications: medications,
        last-update-block: stacks-block-height
      })
    )
    (ok true)
  )
)

;; Calculate checkup reward based on type, timing, and risk level
(define-private (calculate-checkup-reward (checkup-type uint) (pregnancy-week uint) (risk-level (string-ascii 16)))
  (let
    (
      (base-reward (if (is-eq checkup-type CHECKUP_ROUTINE) u100000
                     (if (is-eq checkup-type CHECKUP_ULTRASOUND) u150000
                       (if (is-eq checkup-type CHECKUP_BLOODWORK) u120000
                         (if (is-eq checkup-type CHECKUP_SCREENING) u180000
                           u200000))))) ;; specialist
      (risk-multiplier (if (is-eq risk-level "high") u150
                         (if (is-eq risk-level "medium") u125
                           u100)))
    )
    (/ (* base-reward risk-multiplier) u100)
  )
)

;; Setup initial milestones for pregnancy
(define-private (setup-pregnancy-milestones (pregnancy-id uint))
  (begin
    ;; First trimester milestone (by week 12)
    (map-set care-milestones
      { pregnancy-id: pregnancy-id, milestone: u1 }
      {
        milestone-name: "First Trimester Care",
        target-week: u12,
        completed: false,
        completion-block: u0,
        reward-amount: FIRST_TRIMESTER_REWARD,
        reward-claimed: false
      }
    )
    
    ;; Second trimester milestone (by week 28)
    (map-set care-milestones
      { pregnancy-id: pregnancy-id, milestone: u2 }
      {
        milestone-name: "Second Trimester Care",
        target-week: u28,
        completed: false,
        completion-block: u0,
        reward-amount: SECOND_TRIMESTER_REWARD,
        reward-claimed: false
      }
    )
    
    ;; Third trimester milestone (by week 36)
    (map-set care-milestones
      { pregnancy-id: pregnancy-id, milestone: u3 }
      {
        milestone-name: "Third Trimester Care",
        target-week: u36,
        completed: false,
        completion-block: u0,
        reward-amount: THIRD_TRIMESTER_REWARD,
        reward-claimed: false
      }
    )
    
    (ok true)
  )
)

;; Check and update trimester milestones based on checkup completion
(define-private (check-trimester-milestones (pregnancy-id uint) (current-week uint))
  (let
    (
      (pregnancy-data (unwrap! (map-get? pregnancies pregnancy-id) ERR_NOT_FOUND))
      (checkup-count (get total-checkups pregnancy-data))
    )
    ;; First trimester - need at least 3 checkups by week 12
    (if (and (>= current-week u12) (>= checkup-count u3) (not (get first-trimester-complete pregnancy-data)))
      (begin
        (map-set pregnancies pregnancy-id (merge pregnancy-data { first-trimester-complete: true }))
        (map-set care-milestones
          { pregnancy-id: pregnancy-id, milestone: u1 }
          (merge (unwrap-panic (map-get? care-milestones { pregnancy-id: pregnancy-id, milestone: u1 })) 
                 { completed: true, completion-block: stacks-block-height })
        )
      )
      true
    )
    
    ;; Second trimester - need at least 6 checkups by week 28
    (if (and (>= current-week u28) (>= checkup-count u6) (not (get second-trimester-complete pregnancy-data)))
      (begin
        (map-set pregnancies pregnancy-id (merge pregnancy-data { second-trimester-complete: true }))
        (map-set care-milestones
          { pregnancy-id: pregnancy-id, milestone: u2 }
          (merge (unwrap-panic (map-get? care-milestones { pregnancy-id: pregnancy-id, milestone: u2 })) 
                 { completed: true, completion-block: stacks-block-height })
        )
      )
      true
    )
    
    ;; Third trimester - need at least 10 checkups by week 36
    (if (and (>= current-week u36) (>= checkup-count u10) (not (get third-trimester-complete pregnancy-data)))
      (begin
        (map-set pregnancies pregnancy-id (merge pregnancy-data { third-trimester-complete: true }))
        (map-set care-milestones
          { pregnancy-id: pregnancy-id, milestone: u3 }
          (merge (unwrap-panic (map-get? care-milestones { pregnancy-id: pregnancy-id, milestone: u3 })) 
                 { completed: true, completion-block: stacks-block-height })
        )
      )
      true
    )
    (ok true)
  )
)

;; Get pregnancy details
(define-read-only (get-pregnancy (pregnancy-id uint))
  (map-get? pregnancies pregnancy-id)
)

;; Get checkup details
(define-read-only (get-prenatal-checkup (checkup-id uint))
  (map-get? prenatal-checkups checkup-id)
)

;; Get maternal health record
(define-read-only (get-maternal-health-record (pregnancy-id uint))
  (map-get? maternal-health-records { pregnancy-id: pregnancy-id })
)

;; Get milestone status
(define-read-only (get-milestone-status (pregnancy-id uint) (milestone uint))
  (map-get? care-milestones { pregnancy-id: pregnancy-id, milestone: milestone })
)

;; Get mother's pregnancies
(define-read-only (get-mother-pregnancies (mother principal))
  (map-get? mother-pregnancies mother)
)

;; Get checkup type name
(define-read-only (get-checkup-type-name (checkup-type uint))
  (if (is-eq checkup-type CHECKUP_ROUTINE) "Routine"
    (if (is-eq checkup-type CHECKUP_ULTRASOUND) "Ultrasound"
      (if (is-eq checkup-type CHECKUP_BLOODWORK) "Blood Work"
        (if (is-eq checkup-type CHECKUP_SCREENING) "Screening"
          "Specialist"))))
)

;; Get prenatal care statistics
(define-read-only (get-prenatal-statistics)
  (ok {
    total-pregnancies: (var-get pregnancy-counter),
    total-checkups: (var-get checkup-counter),
    current-block: stacks-block-height
  })
)

;; Calculate total potential rewards for pregnancy
(define-read-only (calculate-total-potential-rewards (pregnancy-id uint))
  (let
    (
      (pregnancy-data (map-get? pregnancies pregnancy-id))
    )
    (match pregnancy-data
      data (ok (+ FIRST_TRIMESTER_REWARD SECOND_TRIMESTER_REWARD THIRD_TRIMESTER_REWARD COMPLETION_BONUS))
      (err ERR_NOT_FOUND)
    )
  )
)
