(impl-trait .sip009-nft-trait.sip009-nft-trait)
;; SIP009 NFT trait on
;;
;; mainnet
;;(impl-trait 'SP2PABAF9FTAJYNFZH93XENAJ8FVY99RRM50D2JG9.nft-trait.nft-trait)
;; testnet
;;(impl-trait 'ST1CSHTKVHMMQJ7PRQRFYW6SB4QAW6SR3XZ54PKG7.nft-trait.nft-trait)

(define-constant contract-owner tx-sender)
(define-constant err-owner-only (err u100))
(define-constant err-not-token-owner (err u101))
;;(define-constant err-no-value (err u103))
(define-constant err-no-value (err u0))

;; use the SIP009 interface (testnet)
;; trait deployed by deployer address from ./settings/Devnet.toml
;;(impl-trait 'ST1PQHQKV0RJXZFY1DGX8MNSNYVE3VGZJSRTPGZGM.nft-trait.nft-trait)
;;(impl-trait .nft-trait.nft-trait)


;; Non-Fungible Token, modeled after ERC-721 via nft-trait
;; NFT are identified by nft-index (uint) which is tied via a reverse lookup to a real world
;; asset hash - SHA 256 32 byte value. The Asset Hash is used to tie arbitrary real world
;; data to the NFT
(define-non-fungible-token MyParcel uint)
;; Store the last issues token ID
(define-data-var last-id uint u0)
(define-data-var last-contract_id uint u0)
(define-data-var amt-due-utility uint u0)
;;(define-data-var electric-utility-name (string-ascii 25) "Duke")
;;(define-data-var water-utility-name (string-ascii 25) "OUC")
;; contract variables
(define-constant IPFS_ROOT "https://ipfs.io/ipfs/")
;;(define-data-var base-token-uri (string-ascii 256) "https://ruma.risidio.com/mesh/v2/asset/")

;; data structures
;;(define-map nft-data {nft-index: uint} {asset-hash: (buff 32), meta-data-url: (buff 200), max-editions: uint, edition: uint, edition-cost: uint, mint-block-height: uint, series-original: uint})
;;(define-map nft-data {nft-index: uint} {asset-hash: (buff 32), meta-data-url: (buff 200), max-editions: uint, edition: uint, edition-cost: uint, mint-block-height: uint, series-original: uint})
(define-map nft-data uint (string-ascii 256))
(define-map parcel-numbers uint (string-ascii 256))
(define-map situs-addresses uint (string-ascii 256))
(define-map legal-descriptions uint (string-ascii 256))
(define-map GIS-coordinates uint (string-ascii 256))
(define-map KYC-Owner-Names uint (string-ascii 256))
(define-map nft-data2 {nft-index: uint} {asset-hash: (buff 32), PID: (buff 32), legal-description: (buff 200), situs_address: (buff 200), meta-data-url: (buff 200), max-editions: uint, edition: uint, edition-cost: uint, mint-block-height: uint, series-original: uint})

;;these maps hold name of a jurisdiction for each tax type and the wallet this type of tax is to be deposited to
;;(define-map sales-tax-info  (string-ascii 256) {jurisdiction-address: principal, tax-rate-percent: uint})
;; (define-map gross-receipts-tax-info uint (string-ascii 256) {jurisdiction-address: principal, tax-rate-percent: uint})
;; (define-map discretionary-surtax-info uint (string-ascii 256) {jurisdiction-address: principal, tax-rate-percent: uint})
;; (define-map municipal_utility-tax-info uint (string-ascii 256) {jurisdiction-address: principal, tax-rate-percent: uint})
;; (define-map tourist-development-tax-info uint (string-ascii 256) {jurisdiction-address: principal, tax-rate-percent: uint})
(define-map payees uint  {wallet-owner-name: (string-ascii 256), jurisdiction-address: principal, tax-rate-percent: uint})
(define-data-var last-payee-id uint u0)
(define-data-var data int 0)
;;these maps create a linked list accross contracts of token specific to a single parcel
(define-map parent-token-addresses uint {contract_address: principal, contract_name: (string-ascii 256), token_ID: uint})
(define-map child-token-addresses uint {contract_address: principal, contract_name: (string-ascii 256), token_ID: uint})

;;these are the total amounts due that must be paid in full
;;(define-map utility-bills uint {amt-due-utility: uint, sales-tax: uint, GRT: uint, MUT: uint, surtax: uint, franchise: uint })
(define-map utility-bills
  uint
  {amt-due-utility: uint, sales-tax: uint, GRT: uint, MUT: uint, surtax: uint, franchise: uint })
;;track the currency on deposit by token number
(define-map deposited-funds uint uint)

;;*****************************************************************************
;;     Jurisdictional Assignement to Tokens
;;     Each token may have a jurisdiction assigned indicating to whom a specific
;;        tax it remitted.  For instance, sales tax is usually paid to the State
;;        of Florida.  So the SofFL index of the 'sales-tax-info' map is assinged
;;        to the parcel-token.  The first group of maps below must consist of unique 
;;        set of wallet address and name.  There is a one-to-many relationship
;;        with the second group which assigns the jurisdiction for each tax type to each token         
;;        
;;these maps are the jurisdictions assigned to each parcel token based on tax type
(define-map sales-tax-jurisdictions uint (string-ascii 256))
(define-map MUT-tax-jurisdictions uint (string-ascii 256))
(define-map GRT-tax-jurisdictions uint (string-ascii 256))
(define-map TDT-tax-jurisdictions uint (string-ascii 256))
(define-map francshise-tax-jurisdictions uint (string-ascii 256))
(define-map surtax-tax-jurisdictions uint (string-ascii 256))


;; Claim a new NFT
(define-public (claim)
  (mint tx-sender))

;; Mint a new NFT, set its URI and return the token #
(define-public (claim_and_set_uri (meta-data-url (string-ascii 256)))
    (begin
      (try! (claim))   
      (map-insert nft-data (var-get last-id) meta-data-url) 
      (ok (var-get last-id))
    )
)

;; Mint a new NFT, set its URI and return the token #
(define-public (claim-and-save-metadata 
   (situs-address (string-ascii 256))
   (parcel-number (string-ascii 256))
   (legal-description (string-ascii 256))
   (GIS-coordinate (string-ascii 256))
   (meta-data-url (string-ascii 256))
   (parent-contract-address principal)
   (parent-contract-name (string-ascii 256))
   (parent_token_num uint)
  )
    (begin
      (try! (claim))      
      (map-insert nft-data (var-get last-id) meta-data-url) 
      (map-insert parcel-numbers (var-get last-id) parcel-number) 
      (map-insert situs-addresses (var-get last-id) situs-address) 
      (map-insert legal-descriptions (var-get last-id) legal-description) 
      (map-insert GIS-coordinates (var-get last-id) GIS-coordinate) 
      (map-insert parent-token-addresses (var-get last-id) {contract_address: parent-contract-address, contract_name: parent-contract-name, token_ID: parent_token_num})
      (ok (var-get last-id))
    )
)

;; Mint a new NFT, set its URI and return the token #
(define-public (claim-and-save-all-data 
   (situs-address (string-ascii 256))
   (parcel-number (string-ascii 256))
   (legal-description (string-ascii 256))
   (GIS-coordinate (string-ascii 256))
   (meta-data-url (string-ascii 256))
   (parent-contract-address principal)
   (parent-contract-name (string-ascii 256))
   (parent_token_num uint)
   (electric-utility-name (string-ascii 25))
   (water-utility-name (string-ascii 25))
  )
    (begin
      (try! (claim))      
      (map-insert nft-data (var-get last-id) meta-data-url) 
      (map-insert parcel-numbers (var-get last-id) parcel-number) 
      (map-insert situs-addresses (var-get last-id) situs-address) 
      (map-insert legal-descriptions (var-get last-id) legal-description) 
      (map-insert GIS-coordinates (var-get last-id) GIS-coordinate) 
      (map-insert parent-token-addresses (var-get last-id) {contract_address: parent-contract-address, contract_name: parent-contract-name, token_ID: parent_token_num})
      (ok (var-get last-id))
    )
)
;; (contract-call? .parcels claim-and-save-metadata
;;    "2665 Hempel Ave, Windermere FL 34786",
;;    "292303018290011",
;;    "tdd",
;;    "28.78547748 -81.60528943",
;;    "//ipfs.io/ipfs/QmfVyamTcLr8WU1AcnGSK5iUshfksRB8woG3ZbTrHLuQVn?filename=322215233601403.json",
;;    "0xd91b00Dd1AEBc0fC3093e8a0556aa36D0AFd86C8",
;;    "ORANGE_COUNTY_6",
;;    u24
;;   )

;; Mint a new NFT, set its URI and return the token #
(define-public (claim-and-save-all-data2 
   (situs-address (string-ascii 256)))
  
    (begin
      (try! (claim))      
      (map-insert situs-addresses (var-get last-id) situs-address) 
      (ok true)
    )
)

;; (contract-call? .parcels claim-and-save-metadata
;;    "2665 Hempel Ave, Windermere FL 34786",
;;    "292303018290011",
;;    "tdd",
;;    "28.78547748 -81.60528943",
;;    "//ipfs.io/ipfs/QmfVyamTcLr8WU1AcnGSK5iUshfksRB8woG3ZbTrHLuQVn?filename=322215233601403.json",
;;    "0xd91b00Dd1AEBc0fC3093e8a0556aa36D0AFd86C8",
;;    "ORANGE_COUNTY_6",
;;    u24
;;   )


;; SIP009: Transfer token to a specified principal
(define-public (transfer (token-id uint) (sender principal) (recipient principal))
  (begin
     (asserts! (is-eq tx-sender sender) (err u403))
     (nft-transfer? MyParcel token-id sender recipient)))

(define-public (transfer-memo (token-id uint) (sender principal) (recipient principal) (memo (buff 34)))
  (begin 
    (try! (transfer token-id sender recipient))
    (print memo)
    (ok true)))


;; Internal - Mint new NFT
(define-private (mint (new-owner principal))
    (let ((next-id (+ u1 (var-get last-id))))
      (var-set last-id next-id)
      (nft-mint? MyParcel next-id new-owner)))

;;*********************************************************************
;;
;;        Setters
;;
;;*********************************************************************

(define-public (set-token-uri (nftIndex uint) (meta-data-url (string-ascii 256)))
   (begin
      (map-insert nft-data nftIndex meta-data-url)
      ;; see place-bid for the payment - no need for this (unwrap! (stx-transfer? bidAmount tx-sender (as-contract tx-sender)) failed-to-stx-transfer)
     ;; (map-insert nft-data {nft-index: nftIndex, meta-data-url: meta-data-url}) 
   ;;      (map-set nft-high-bid-counter {nft-index: nftIndex} {high-bid-counter: (+ bidCounter u1), sale-cycle: saleCycle})
   ;;       (print {evt: "record-bid", nftIndex: nftIndex, txSender: tx-sender, bidAmount: bidAmount, bidCounter: bidCounter, appTimestamp: appTimestamp, saleCycle: saleCycle})
      (ok true)   
   )
)

(define-public (set-token-uri-for-last-tokenID (meta-data-url (string-ascii 256)))
   (begin
      (map-insert nft-data (var-get last-id) meta-data-url)
      (ok true)   
   )
)



(define-public (set-child-token-address (contract_address principal) (contract_name (string-ascii 256)) (token_ID_Child uint) (token_ID uint))
   (begin
      ;;(map-set orders u0 {maker: tx-sender, amount: u50})
      (map-set child-token-addresses token_ID_Child {contract_address: contract_address, contract_name: contract_name, token_ID: token_ID})
      (ok true)   
   )
)


       
;;*********************************************************************
;;
;;        Getters
;;
;;*********************************************************************


;; SIP009: Get the owner of the specified token ID
(define-read-only (get-owner (token-id uint))
  (ok (nft-get-owner? MyParcel token-id)))

;; SIP009: Get the last token ID
(define-read-only (get-last-token-id)
  (ok (var-get last-id)))

;; SIP009: Get the token URI. You can set it to any other URI
(define-read-only (get-token-uri (token-id uint))
   (ok (map-get? nft-data token-id))
 ;;   (ok (as-max-len? (concat (concat IPFS_ROOT (map-get? nft-data token-id)) "_metadata.json") u256))
  ;; (get nft-data2 { token-id: token-id })
  ;; (ok (as-max-len? (concat (concat IPFS_ROOT ((map-get? nft-data token-id))) "_metadata.json") u256))
)

(define-read-only (get-parcel-number (token-id uint))
   (ok (map-get? parcel-numbers token-id))
)

(define-read-only (get-situs-address (token-id uint))
   (ok (map-get? situs-addresses token-id))
)

(define-read-only (get-legal-description (token-id uint))
   (ok (map-get? legal-descriptions token-id))
)

(define-read-only (get-GIS-coordinate (token-id uint))
   (ok (map-get? GIS-coordinates token-id))
)

(define-read-only (get-parent-token-address (token-id uint))
   (ok (map-get? parent-token-addresses token-id))
)

(define-read-only (get-child-token-address (token-id uint))
   (ok (map-get? child-token-addresses token-id))
)

;; RANDOM CODE END
;;see https://explorer.stacks.co/txid/0xc4af2d5663f10ef3eb32f9196db550990a25d5067e55e8c12cc7224b41e8eeee?chain=mainnet
(define-read-only (get-token-uri2 (token-id uint))
    (ok (as-max-len? (concat (concat IPFS_ROOT (concat "guinea_" (uint-to-string token-id))) "_metadata.json") u256))
)

;; CODE FOR UINT -> STRING
(define-constant FOLDS_3 (list true true true))

(define-constant NUM_TO_CHAR (list
    "0" "1" "2" "3" "4" "5" "6" "7" "8" "9"
))

(define-private (concat-uint (ignore bool) (input { dec: uint, data: (string-ascii 3) }))
    (let (
            (last-val (get dec input))
        )
        (if (is-eq last-val u0)
            {
                dec: last-val,
                data: (get data input)
            }
            (if (< last-val u10)
                {
                    dec: u0,
                    data: (concat-num-to-string last-val (get data input))
                }
                {
                    dec: (/ last-val u10),
                    data: (concat-num-to-string (mod last-val u10) (get data input))
                }
            )
        )
    )
)

(define-private (concat-num-to-string (num uint) (right (string-ascii 3)))
    (unwrap-panic (as-max-len? (concat (unwrap-panic (element-at NUM_TO_CHAR num)) right) u3))
)

(define-private (uint-to-string (num uint))
    (if (is-eq num u0)
        (unwrap-panic (as-max-len? "0" u3))
        (get data (fold concat-uint FOLDS_3 { dec: num, data: ""}))
    )
)

(define-read-only (utility-bill-amount-due (token-number uint)) 
    (let ((temp1 (map-get? utility-bills token-number)))
        (+ 
          (default-to u0 (get amt-due-utility temp1))
          (default-to u0 (get sales-tax temp1)) 
          (default-to u0 (get GRT temp1))
          (default-to u0 (get MUT temp1))
          (default-to u0 (get franchise temp1))
          (default-to u0 (get surtax temp1))
        )
    )
)

(define-private (utility-bill-amount-dueOLD (token-number uint)) 
    (let ((temp1
        (map-get? utility-bills token-number))
        (amt-due-utility3 (default-to u0 (get amt-due-utility temp1)))
        (sales-tax (default-to u0 (get sales-tax temp1)))
        (GRT (default-to u0 (get GRT temp1)))
        (MUT (default-to u0 (get MUT temp1)))
        (franchise (default-to u0 (get franchise temp1)))
        (surtax (default-to u0 (get surtax temp1)))
        (balance-due (+ amt-due-utility3 sales-tax GRT MUT franchise surtax))
        )            
    (ok true)
    )
)    


;;recieve payment to contract and record to which token it applies
(define-public (recieve-payment  (token-number uint) (amount uint))
    (begin
        (asserts! (> amount u0) err-no-value)
        (try! (stx-transfer? amount tx-sender (as-contract tx-sender))) 
        (let (
            (current-balance (default-to u0 ( map-get? deposited-funds token-number ))) 
            (new-balance (+ current-balance amount))                     
            ) 
            (map-set deposited-funds token-number new-balance)  
        )       
        (ok true)
    )
)

;;decrease balance for this token by amount used to pay current bill
(define-public (disburse-funds  (token-number uint) (amount uint))
    (begin
        (asserts! (> amount u0) err-no-value) 
        (let (
            (current-balance (default-to u0 ( map-get? deposited-funds token-number ))) 
            (new-balance (- current-balance amount))                     
            ) 
            (map-set deposited-funds token-number new-balance)  
        )       
        (ok true)
    )
)

(define-private (pay (payment (optional uint)) (receiver principal))  
  (match payment
    amount (as-contract 
             (stx-transfer? amount tx-sender receiver))
    (ok false)))

;; (define-private (reset-utility-bill (token-number uint))
;;   (if (((map-set utility-bills token-number {amt-due-utility: u0, sales-tax: u0, GRT: u0, MUT: u0, surtax: u0, franchise: u0 })
;;   (ok old-value)
;;       (err u666))))
;; )

;; (define-public (reset-utility-bill (token-number uint))
;;   ((map-set utility-bills token-number {amt-due-utility: u0, sales-tax: u0, GRT: u0, MUT: u0, surtax: u0, franchise: u0 })
;;       (ok old-value)
;;       (err u666)))

(define-public (reset-utility-bill  (token-number uint) )
    (begin
        (asserts! (> token-number u0) err-no-value) 
        (map-set utility-bills u1 {amt-due-utility: u0, sales-tax: u0, GRT: u0, MUT: u0, surtax: u0, franchise: u0 })         
        (ok true)
    )
)

(define-public (change (value int))
  (let ((old-value (var-get data)))
    (if (var-set data value)
      (ok old-value)
      (err u666))))

(define-public (pay-utility-bill) 
;;ability to change wallet address restricted to wallet holder.  ORG cant change once set.
;;wallet 1: Duke Energy
;;Wallet 2: Florida Department of Revenue Sales Tax
;;Wallet 3: Florida Department of Revenue Gross Reciepts Tax
;;Wallet 4: Orange County Comptroller
;;Wallet 5: Florida Department of Revenue Discretionary Surtax
;;Wallet 6: City of Orlando
  (let ((temp1
        (map-get? utility-bills u1))
        (amt-due-utility2 (get amt-due-utility temp1))
        (sales-tax (get sales-tax temp1))
        (GRT (get GRT temp1))
        (MUT (get MUT temp1))
        (franchise (get franchise temp1))
        (surtax (get surtax temp1))) 
    (print (map-get? utility-bills u1))
    (asserts! (is-some amt-due-utility2) err-no-value)
    (try! (pay amt-due-utility2 'ST1SJ3DTE5DN7X54YDH5D64R3BCB6A2AG2ZQ8YPD5))
    (try! (pay sales-tax 'ST2CY5V39NHDPWSXMW9QDT3HC3GD6Q6XX4CFRK9AG))
    (try! (pay GRT 'ST2JHG361ZXG51QTKY2NQCVBPPRRE2KZB1HR05NNC))
    (try! (pay MUT 'ST2NEB84ASENDXKYGJPQW86YXQCEFEX2ZQPG87ND))
   ;; (map-set utility-bills u1 {amt-due-utility: u0, sales-tax: u0, GRT: u0, MUT: u0, surtax: u0, franchise: u0 })
  
    ;; (if (is-some franchise)   
    ;;     (try! (pay franchise 'ST2REHHS5J3CERCRBEPMGH7921Q6PYKAADT7JP2VB))
    ;;     true)
    
    ;; (if (is-some franchise)
    ;;     (try! (pay (unwrap-panic franchise) 'ST2REHHS5J3CERCRBEPMGH7921Q6PYKAADT7JP2VB))
    ;;     true)
    
    ;; (match franchise franchise_
    ;; (try! (pay franchise_ 'ST2REHHS5J3CERCRBEPMGH7921Q6PYKAADT7JP2VB)
    ;; true
    ;; ))

    (try! (pay surtax 'ST2REHHS5J3CERCRBEPMGH7921Q6PYKAADT7JP2VB))
    ;;(try! (disburse-funds  u1 u110)) ;;decrease token balances by amount paid
    (ok true)))



    (define-private (payOLD_WAY (payment (optional uint)) (receiver principal))     
        (let
            (
                (amt-to-pay-utility (default-to u0 ( get amt-due-utility (map-get? utility-bills u1 )))) 
                (amt-to-pay-sales-tax-payee (default-to u0 ( get sales-tax (map-get? utility-bills u1 )))) 
                (amt-to-pay-GRT-payee (default-to u0 ( get GRT (map-get? utility-bills u1 ))))
                (amt-to-pay-MUT-payee (default-to u0 ( get MUT (map-get? utility-bills u1 ))))
                (amt-to-pay-franchise-tax-payee (default-to u0 ( get franchise (map-get? utility-bills u1 ))))
                (amt-to-pay-surtax-payee (default-to u0 ( get surtax (map-get? utility-bills u1 ))))
            )
        (asserts! (not (is-eq amt-to-pay-utility u0)) err-no-value)
        (print "made it here 3")
        ;;wallet 1: Duke Energy
        ;;Wallet 2: Florida Department of Revenue Sales Tax
        ;;Wallet 3: Florida Department of Revenue Gross Reciepts Tax
        ;;Wallet 4: Orange County Comptroller
        ;;Wallet 5: Florida Department of Revenue Discretionary Surtax
        ;;Wallet 6: City of Orlando
        (try! (as-contract (stx-transfer? amt-to-pay-utility tx-sender 'ST1SJ3DTE5DN7X54YDH5D64R3BCB6A2AG2ZQ8YPD5)))
        (if (not (is-eq amt-to-pay-sales-tax-payee u0))            
            (try! (as-contract (stx-transfer? amt-to-pay-sales-tax-payee tx-sender 'ST2CY5V39NHDPWSXMW9QDT3HC3GD6Q6XX4CFRK9AG)))
            true
        )
        (if (not (is-eq amt-to-pay-GRT-payee u0))            
            (try! (as-contract (stx-transfer? amt-to-pay-GRT-payee tx-sender 'ST2JHG361ZXG51QTKY2NQCVBPPRRE2KZB1HR05NNC)))
            true
        )
        (if (not (is-eq amt-to-pay-MUT-payee u0))            
            (try! (as-contract (stx-transfer? amt-to-pay-MUT-payee tx-sender 'ST2NEB84ASENDXKYGJPQW86YXQCEFEX2ZQPG87ND)))
            true
        )
        (if (not (is-eq amt-to-pay-surtax-payee u0))            
            (try! (as-contract (stx-transfer? amt-to-pay-surtax-payee tx-sender 'ST2REHHS5J3CERCRBEPMGH7921Q6PYKAADT7JP2VB)))
            true
        )
        (if (not (is-eq amt-to-pay-franchise-tax-payee u0))            
            (try! (as-contract (stx-transfer? amt-to-pay-franchise-tax-payee tx-sender 'ST3AM1A56AK2C1XAFJ4115ZSV26EB49BVQ10MGCS0)))
            true
        )
        (try! (disburse-funds  u1 u110))
        (ok true)
        )
    )


;; (define-read-only (get-amt-due-utility (idx uint))
;;     (let ((utility-bills-info (try! (map-get? utility-bills u1))))
;;         (if (match (default-to u0 (get amt-due-utility utility-bills-info))
;;             amt-due-utility (> amt-due-utility u0)
;;             (try! (as-contract (stx-transfer? amt tx-sender 'ST1SJ3DTE5DN7X54YDH5D64R3BCB6A2AG2ZQ8YPD5))))
;;           ;; it expired, return none
;;           none
;;           ;; delegation is active
;;           (some utility-bills-info))))

(define-public (test)
    (begin
    ;;create list of jurisdictions & thier wallets where tax will be recieved
    (map-insert payees u1 {wallet-owner-name: "Duke Energy", jurisdiction-address:  'ST1SJ3DTE5DN7X54YDH5D64R3BCB6A2AG2ZQ8YPD5, tax-rate-percent : u1000 } )
    (map-insert payees u1 {wallet-owner-name: "Florida Department of Revenue Sales Tax", jurisdiction-address:  'ST2CY5V39NHDPWSXMW9QDT3HC3GD6Q6XX4CFRK9AG, tax-rate-percent : u1000 } )
    (map-insert payees u1 {wallet-owner-name: "Florida Department of Revenue Gross Reciepts Tax", jurisdiction-address:  'ST2JHG361ZXG51QTKY2NQCVBPPRRE2KZB1HR05NNC, tax-rate-percent : u1000 } )
    (map-insert payees u1 {wallet-owner-name: "Orange County Comptroller", jurisdiction-address:  'ST2NEB84ASENDXKYGJPQW86YXQCEFEX2ZQPG87ND, tax-rate-percent : u1000 } )
    (map-insert payees u1 {wallet-owner-name: "Florida Department of Revenue Discretionary Surtax", jurisdiction-address:  'ST2REHHS5J3CERCRBEPMGH7921Q6PYKAADT7JP2VB, tax-rate-percent : u1000 } )
    (map-insert payees u1 {wallet-owner-name: "City of Orlando", jurisdiction-address:  'ST2REHHS5J3CERCRBEPMGH7921Q6PYKAADT7JP2VB, tax-rate-percent : u1000 } )
    ;;mint a token for a parcel
    (try! (claim-and-save-all-data "2665 Hempel Ave, Windermere FL 34786"
            "292303018290011"
            "tdd"
            "28.78547748 -81.60528943"
            "//ipfs.io/ipfs/QmfVyamTcLr8WU1AcnGSK5iUshfksRB8woG3ZbTrHLuQVn?filename=322215233601403.json"
            'ST1SJ3DTE5DN7X54YDH5D64R3BCB6A2AG2ZQ8YPD5
            "ORANGE_COUNTY_6"
            u24
            "Duke"
            "OUC"))     
    ;;post utility bill for the newly created token
    ;;  this is the interface for the utility company where they will enter amounts due for the current 
    ;;  month or for unpaid billing periods
    (map-set utility-bills u1 {amt-due-utility: u100, sales-tax: u6, GRT: u1, MUT: u2, surtax: u1, franchise: u0 })
    ;;(print (map-get? utility-bills u1 ))
    ;;get amt due for token ***********************
    ;;customer pay utility bill
   ;; (try! (recieve-payment  u1 u110))  
   (try! (test2))  
   ;;(try! (reset-utility-bill u1))
    (ok true)
    )
)

(define-public (test2)
    (begin
        (let (
            (balance-due (utility-bill-amount-due u1)) 
            )
            (print "The balance-due on this account is:")
            (print balance-due)
            (try! (recieve-payment  u1 balance-due)) 
            (try! (pay-utility-bill))             
            (try! (disburse-funds  u1 balance-due)) ;;decrease token balances by amount paid
            (try! (reset-utility-bill u1 ))
            (ok balance-due)
        )
    )
)
