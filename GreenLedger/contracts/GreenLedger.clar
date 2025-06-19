;; Fixed Carbon Credits Smart Contract
;; This is a corrected version of the original Clarity contract

;; constants
(define-constant contract-owner tx-sender)
(define-constant err-owner-only (err u100))
(define-constant err-not-found (err u101))
(define-constant err-insufficient-balance (err u102))
(define-constant err-invalid-amount (err u103))
(define-constant err-not-verified (err u104))
(define-constant err-already-verified (err u105))
(define-constant err-expired (err u106))
(define-constant err-unauthorized (err u107))
(define-constant err-invalid-oracle (err u108))

;; data maps and vars
(define-map carbon-credits
  { credit-id: uint }
  {
    issuer: principal,
    project-id: (string-ascii 64),
    amount: uint,
    price-per-credit: uint,
    verification-status: (string-ascii 20),
    ai-confidence-score: uint,
    expiry-block: uint,
    metadata-hash: (string-ascii 64),
    created-at: uint
  }
)

(define-map user-balances
  { user: principal, credit-id: uint }
  { balance: uint }
)

(define-map ai-oracles
  { oracle-id: uint }
  {
    oracle-address: principal,
    reputation-score: uint,
    total-verifications: uint,
    accuracy-rate: uint,
    is-active: bool
  }
)

(define-map project-registry
  { project-id: (string-ascii 64) }
  {
    owner: principal,
    location: (string-ascii 100),
    project-type: (string-ascii 50),
    estimated-credits: uint,
    verification-threshold: uint,
    is-active: bool
  }
)

(define-data-var next-credit-id uint u1)
(define-data-var next-oracle-id uint u1)
(define-data-var total-credits-issued uint u0)
(define-data-var verification-fee uint u1000000) ;; 1 STX in microSTX

;; private functions
(define-private (is-contract-owner)
  (is-eq tx-sender contract-owner)
)

(define-private (calculate-ai-weighted-score (scores (list 10 uint)) (weights (list 10 uint)))
  (let ((total-weighted (fold + (map * scores weights) u0))
        (total-weights (fold + weights u0)))
    (if (> total-weights u0)
        (/ total-weighted total-weights)
        u0)
  )
)

(define-private (validate-credit-parameters (amount uint) (price uint) (expiry uint))
  (and (> amount u0)
       (> price u0)
       (> expiry block-height))
)

;; Helper functions for list operations
(define-private (list-length-10 (lst (list 10 uint)))
  u10
)

(define-private (list-length-8 (lst (list 8 uint)))
  u8
)

(define-private (list-length-5 (lst (list 5 uint)))
  u5
)

(define-private (list-length-3 (lst (list 3 uint)))
  u3
)

;; Helper function to calculate absolute difference
(define-private (abs-diff (a uint) (b uint))
  (if (> a b) (- a b) (- b a))
)

;; Helper function to get minimum of two values
(define-private (min-uint (a uint) (b uint))
  (if (< a b) a b)
)

;; Simplified market volatility calculation
(define-private (calculate-market-volatility (market-data (list 10 uint)))
  (let ((base-values (list u50 u50 u50 u50 u50 u50 u50 u50 u50 u50)))
    (fold + (map abs-diff market-data base-values) u0)
  )
)

;; public functions
(define-public (register-ai-oracle (oracle-address principal) (initial-reputation uint))
  (let ((oracle-id (var-get next-oracle-id)))
    (asserts! (is-contract-owner) err-owner-only)
    (map-set ai-oracles
      { oracle-id: oracle-id }
      {
        oracle-address: oracle-address,
        reputation-score: initial-reputation,
        total-verifications: u0,
        accuracy-rate: u100,
        is-active: true
      }
    )
    (var-set next-oracle-id (+ oracle-id u1))
    (ok oracle-id)
  )
)

(define-public (register-project (project-id (string-ascii 64)) 
                                (location (string-ascii 100))
                                (project-type (string-ascii 50))
                                (estimated-credits uint))
  (begin
    (asserts! (> estimated-credits u0) err-invalid-amount)
    (map-set project-registry
      { project-id: project-id }
      {
        owner: tx-sender,
        location: location,
        project-type: project-type,
        estimated-credits: estimated-credits,
        verification-threshold: u75, ;; 75% confidence threshold
        is-active: true
      }
    )
    (ok true)
  )
)

(define-public (issue-carbon-credits (project-id (string-ascii 64))
                                   (amount uint)
                                   (price-per-credit uint)
                                   (expiry-blocks uint)
                                   (metadata-hash (string-ascii 64)))
  (let ((credit-id (var-get next-credit-id))
        (expiry-block (+ block-height expiry-blocks)))
    (asserts! (validate-credit-parameters amount price-per-credit expiry-block) err-invalid-amount)
    (asserts! (is-some (map-get? project-registry { project-id: project-id })) err-not-found)
    
    (map-set carbon-credits
      { credit-id: credit-id }
      {
        issuer: tx-sender,
        project-id: project-id,
        amount: amount,
        price-per-credit: price-per-credit,
        verification-status: "pending",
        ai-confidence-score: u0,
        expiry-block: expiry-block,
        metadata-hash: metadata-hash,
        created-at: block-height
      }
    )
    
    (map-set user-balances
      { user: tx-sender, credit-id: credit-id }
      { balance: amount }
    )
    
    (var-set next-credit-id (+ credit-id u1))
    (var-set total-credits-issued (+ (var-get total-credits-issued) amount))
    (ok credit-id)
  )
)

(define-public (ai-verify-credits (credit-id uint) 
                                 (oracle-id uint)
                                 (verification-data (list 5 uint))
                                 (confidence-score uint))
  (let ((credit (unwrap! (map-get? carbon-credits { credit-id: credit-id }) err-not-found))
        (oracle (unwrap! (map-get? ai-oracles { oracle-id: oracle-id }) err-not-found)))
    
    (asserts! (is-eq tx-sender (get oracle-address oracle)) err-unauthorized)
    (asserts! (get is-active oracle) err-unauthorized)
    (asserts! (is-eq (get verification-status credit) "pending") err-already-verified)
    (asserts! (< block-height (get expiry-block credit)) err-expired)
    
    (let ((weighted-score (calculate-ai-weighted-score verification-data (list u20 u25 u20 u20 u15)))
          (final-confidence (/ (+ confidence-score weighted-score) u2))
          (verification-status (if (>= final-confidence u75) "verified" "rejected")))
      
      (map-set carbon-credits
        { credit-id: credit-id }
        (merge credit {
          verification-status: verification-status,
          ai-confidence-score: final-confidence
        })
      )
      
      ;; Update oracle statistics
      (map-set ai-oracles
        { oracle-id: oracle-id }
        (merge oracle {
          total-verifications: (+ (get total-verifications oracle) u1)
        })
      )
      
      (ok final-confidence)
    )
  )
)

(define-public (transfer-credits (credit-id uint) (recipient principal) (amount uint))
  (let ((sender-balance (default-to u0 (get balance (map-get? user-balances { user: tx-sender, credit-id: credit-id }))))
        (credit (unwrap! (map-get? carbon-credits { credit-id: credit-id }) err-not-found)))
    
    (asserts! (is-eq (get verification-status credit) "verified") err-not-verified)
    (asserts! (>= sender-balance amount) err-insufficient-balance)
    (asserts! (> amount u0) err-invalid-amount)
    
    ;; Update sender balance
    (map-set user-balances
      { user: tx-sender, credit-id: credit-id }
      { balance: (- sender-balance amount) }
    )
    
    ;; Update recipient balance
    (let ((recipient-balance (default-to u0 (get balance (map-get? user-balances { user: recipient, credit-id: credit-id })))))
      (map-set user-balances
        { user: recipient, credit-id: credit-id }
        { balance: (+ recipient-balance amount) }
      )
    )
    
    (ok true)
  )
)

;; Fixed AI-powered automated compliance and market analysis function
(define-public (ai-automated-compliance-analysis (credit-id uint)
                                                (market-data (list 10 uint))
                                                (environmental-metrics (list 8 uint))
                                                (regulatory-scores (list 5 uint)))
  (let ((credit (unwrap! (map-get? carbon-credits { credit-id: credit-id }) err-not-found))
        (project (unwrap! (map-get? project-registry { project-id: (get project-id credit) }) err-not-found))
        
        ;; Calculate market volatility score (fixed)
        (market-volatility (calculate-market-volatility market-data))
        (avg-market-score (/ (fold + market-data u0) (list-length-10 market-data)))
        
        ;; Calculate environmental impact score
        (env-impact-score (calculate-ai-weighted-score 
                          environmental-metrics 
                          (list u15 u20 u15 u10 u15 u10 u10 u5)))
        
        ;; Calculate regulatory compliance score (fixed)
        (regulatory-compliance (/ (fold + regulatory-scores u0) (list-length-5 regulatory-scores)))
        
        ;; AI-driven risk assessment (fixed)
        (risk-factors (list market-volatility env-impact-score regulatory-compliance))
        (overall-risk (/ (fold + risk-factors u0) (list-length-3 risk-factors)))
        
        ;; Dynamic pricing adjustment based on AI analysis
        (current-price (get price-per-credit credit))
        (market-adjustment (if (> avg-market-score u70) u110 u90)) ;; +10% or -10%
        (risk-adjustment (if (< overall-risk u30) u105 u95)) ;; +5% or -5%
        (suggested-price (/ (* (* current-price market-adjustment) risk-adjustment) u10000))
        
        ;; Compliance status determination
        (compliance-threshold u80)
        (is-compliant (and (> env-impact-score compliance-threshold)
                         (> regulatory-compliance compliance-threshold)
                         (< overall-risk u40)))
        
        ;; Generate AI recommendations
        (recommendation-score (+ (* env-impact-score u3) 
                             (* regulatory-compliance u2) 
                             (- u100 overall-risk))))
    
    ;; Update credit with AI analysis results
    (map-set carbon-credits
      { credit-id: credit-id }
      (merge credit {
        price-per-credit: suggested-price,
        ai-confidence-score: (min-uint u100 recommendation-score)
      })
    )
    
    ;; Return comprehensive analysis results
    (ok {
      market-score: avg-market-score,
      environmental-score: env-impact-score,
      regulatory-score: regulatory-compliance,
      overall-risk: overall-risk,
      suggested-price: suggested-price,
      is-compliant: is-compliant,
      recommendation: recommendation-score
    })
  )
)

