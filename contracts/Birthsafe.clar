(define-fungible-token birthsafe-token)

(define-constant contract-owner tx-sender)
(define-constant err-owner-only (err u100))
(define-constant err-not-found (err u101))
(define-constant err-already-exists (err u102))
(define-constant err-invalid-hospital (err u103))
(define-constant err-invalid-birth (err u104))
(define-constant err-reward-claimed (err u105))
(define-constant err-insufficient-funds (err u106))
(define-constant err-time-expired (err u107))

(define-data-var token-name (string-ascii 32) "Safe Birth Token")
(define-data-var token-symbol (string-ascii 10) "SBT")
(define-data-var token-decimals uint u6)
(define-data-var total-supply uint u0)
(define-data-var reward-amount uint u1000000)
(define-data-var reward-expiry-blocks uint u144000)

(define-map hospitals principal {
    name: (string-ascii 64),
    location: (string-ascii 128),
    verified: bool,
    registration-block: uint
})

(define-map births (string-ascii 32) {
    mother: principal,
    hospital: principal,
    birth-block: uint,
    verified: bool,
    reward-claimed: bool,
    birth-weight: uint,
    birth-date: (string-ascii 16)
})

(define-map maternal-records principal {
    total-births: uint,
    institutional-births: uint,
    total-rewards: uint,
    last-birth-block: uint
})

(define-map hospital-stats principal {
    total-deliveries: uint,
    successful-deliveries: uint,
    total-rewards-distributed: uint
})

(define-read-only (get-name)
    (ok (var-get token-name))
)

(define-read-only (get-symbol)
    (ok (var-get token-symbol))
)

(define-read-only (get-decimals)
    (ok (var-get token-decimals))
)

(define-read-only (get-total-supply)
    (ok (var-get total-supply))
)

(define-read-only (get-balance (who principal))
    (ok (ft-get-balance birthsafe-token who))
)

(define-read-only (get-hospital (hospital-principal principal))
    (map-get? hospitals hospital-principal)
)

(define-read-only (get-birth (birth-id (string-ascii 32)))
    (map-get? births birth-id)
)

(define-read-only (get-maternal-record (mother principal))
    (map-get? maternal-records mother)
)

(define-read-only (get-hospital-stats (hospital principal))
    (map-get? hospital-stats hospital)
)

(define-read-only (get-reward-amount)
    (ok (var-get reward-amount))
)

(define-read-only (can-claim-reward (birth-id (string-ascii 32)))
    (match (map-get? births birth-id)
        birth-data (let (
            (current-block stacks-block-height)
            (birth-block (get birth-block birth-data))
            (reward-claimed (get reward-claimed birth-data))
            (verified (get verified birth-data))
        )
        (ok (and 
            verified
            (not reward-claimed)
            (<= (- current-block birth-block) (var-get reward-expiry-blocks))
        )))
        (err err-not-found)
    )
)

(define-public (register-hospital (name (string-ascii 64)) (location (string-ascii 128)))
    (let (
        (current-block stacks-block-height)
    )
    (asserts! (is-eq tx-sender contract-owner) err-owner-only)
    (asserts! (is-none (map-get? hospitals tx-sender)) err-already-exists)
    (map-set hospitals tx-sender {
        name: name,
        location: location,
        verified: true,
        registration-block: current-block
    })
    (map-set hospital-stats tx-sender {
        total-deliveries: u0,
        successful-deliveries: u0,
        total-rewards-distributed: u0
    })
    (ok true))
)

(define-public (register-birth 
    (birth-id (string-ascii 32))
    (mother principal)
    (birth-weight uint)
    (birth-date (string-ascii 16))
)
    (let (
        (current-block stacks-block-height)
        (hospital-data (unwrap! (map-get? hospitals tx-sender) err-invalid-hospital))
    )
    (asserts! (get verified hospital-data) err-invalid-hospital)
    (asserts! (is-none (map-get? births birth-id)) err-already-exists)
    (asserts! (> birth-weight u0) err-invalid-birth)
    
    (map-set births birth-id {
        mother: mother,
        hospital: tx-sender,
        birth-block: current-block,
        verified: true,
        reward-claimed: false,
        birth-weight: birth-weight,
        birth-date: birth-date
    })
    
    (map-set hospital-stats tx-sender 
        (merge (default-to {
            total-deliveries: u0,
            successful-deliveries: u0,
            total-rewards-distributed: u0
        } (map-get? hospital-stats tx-sender))
        {total-deliveries: (+ (get total-deliveries (default-to {
            total-deliveries: u0,
            successful-deliveries: u0,
            total-rewards-distributed: u0
        } (map-get? hospital-stats tx-sender))) u1)}
        )
    )
    
    (map-set maternal-records mother
        (merge (default-to {
            total-births: u0,
            institutional-births: u0,
            total-rewards: u0,
            last-birth-block: u0
        } (map-get? maternal-records mother))
        {
            total-births: (+ (get total-births (default-to {
                total-births: u0,
                institutional-births: u0,
                total-rewards: u0,
                last-birth-block: u0
            } (map-get? maternal-records mother))) u1),
            institutional-births: (+ (get institutional-births (default-to {
                total-births: u0,
                institutional-births: u0,
                total-rewards: u0,
                last-birth-block: u0
            } (map-get? maternal-records mother))) u1),
            last-birth-block: current-block
        })
    )
    (ok birth-id))
)

(define-public (claim-birth-reward (birth-id (string-ascii 32)))
    (let (
        (birth-data (unwrap! (map-get? births birth-id) err-not-found))
        (mother (get mother birth-data))
        (hospital (get hospital birth-data))
        (current-block stacks-block-height)
        (birth-block (get birth-block birth-data))
        (reward-amount-val (var-get reward-amount))
    )
    (asserts! (is-eq tx-sender mother) err-owner-only)
    (asserts! (get verified birth-data) err-invalid-birth)
    (asserts! (not (get reward-claimed birth-data)) err-reward-claimed)
    (asserts! (<= (- current-block birth-block) (var-get reward-expiry-blocks)) err-time-expired)
    
    (try! (ft-mint? birthsafe-token reward-amount-val mother))
    (var-set total-supply (+ (var-get total-supply) reward-amount-val))
    
    (map-set births birth-id (merge birth-data {reward-claimed: true}))
    
    (map-set maternal-records mother
        (merge (unwrap! (map-get? maternal-records mother) err-not-found)
        {total-rewards: (+ (get total-rewards (unwrap! (map-get? maternal-records mother) err-not-found)) reward-amount-val)}
        )
    )
    
    (map-set hospital-stats hospital
        (merge (unwrap! (map-get? hospital-stats hospital) err-not-found)
        {
            successful-deliveries: (+ (get successful-deliveries (unwrap! (map-get? hospital-stats hospital) err-not-found)) u1),
            total-rewards-distributed: (+ (get total-rewards-distributed (unwrap! (map-get? hospital-stats hospital) err-not-found)) reward-amount-val)
        })
    )
    (ok reward-amount-val))
)

(define-public (transfer (amount uint) (from principal) (to principal) (memo (optional (buff 34))))
    (begin
        (asserts! (is-eq from tx-sender) err-owner-only)
        (ft-transfer? birthsafe-token amount from to)
    )
)

(define-public (set-reward-amount (new-amount uint))
    (begin
        (asserts! (is-eq tx-sender contract-owner) err-owner-only)
        (asserts! (> new-amount u0) err-invalid-birth)
        (var-set reward-amount new-amount)
        (ok true)
    )
)

(define-public (set-reward-expiry (new-expiry uint))
    (begin
        (asserts! (is-eq tx-sender contract-owner) err-owner-only)
        (asserts! (> new-expiry u0) err-invalid-birth)
        (var-set reward-expiry-blocks new-expiry)
        (ok true)
    )
)

(define-public (verify-hospital (hospital-principal principal) (verified bool))
    (let (
        (hospital-data (unwrap! (map-get? hospitals hospital-principal) err-not-found))
    )
    (asserts! (is-eq tx-sender contract-owner) err-owner-only)
    (map-set hospitals hospital-principal (merge hospital-data {verified: verified}))
    (ok true))
)

(define-public (emergency-withdraw (amount uint) (recipient principal))
    (begin
        (asserts! (is-eq tx-sender contract-owner) err-owner-only)
        (asserts! (>= (ft-get-balance birthsafe-token contract-owner) amount) err-insufficient-funds)
        (ft-transfer? birthsafe-token amount contract-owner recipient)
    )
)

(define-read-only (get-contract-balance)
    (ft-get-balance birthsafe-token (as-contract tx-sender))
)

(define-read-only (get-birth-statistics)
    (ok {
        total-supply: (var-get total-supply),
        reward-amount: (var-get reward-amount),
        reward-expiry-blocks: (var-get reward-expiry-blocks),
        current-block: stacks-block-height
    })
)
