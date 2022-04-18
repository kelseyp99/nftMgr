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
(define-constant err-no-value (err u0))

;; use the SIP009 interface (testnet)
;; trait deployed by deployer address from ./settings/Devnet.toml
;;(impl-trait 'ST1PQHQKV0RJXZFY1DGX8MNSNYVE3VGZJSRTPGZGM.nft-trait.nft-trait)
;;(impl-trait .nft-trait.nft-trait)

(define-non-fungible-token MyParcel uint)
(define-data-var last-id uint u0)
(define-data-var last-contract_id uint u0)
;; contract variables
(define-constant IPFS_ROOT "https://ipfs.io/ipfs/")
;;(define-data-var base-token-uri (string-ascii 256) "https://ruma.risidio.com/mesh/v2/asset/")

;; data structures
(define-map nft-data uint (string-ascii 256))
(define-map parcel-numbers uint (string-ascii 256))
(define-map situs-addresses uint (string-ascii 256))
(define-map legal-descriptions uint (string-ascii 256))
(define-map GIS-coordinates uint (string-ascii 256))
(define-map KYC-Owner-Names uint (string-ascii 256))
(define-map nft-data2 {nft-index: uint} {asset-hash: (buff 32), PID: (buff 32), legal-description: (buff 200), situs_address: (buff 200), meta-data-url: (buff 200), max-editions: uint, edition: uint, edition-cost: uint, mint-block-height: uint, series-original: uint})

;;these maps hold name of a jurisdiction for each tax type and the wallet this type of tax is to be deposited to
;;(define-map sales-tax-info  (string-ascii 256) {wallet-address: principal, tax-rate-percent: uint})
;; (define-map gross-receipts-tax-info uint (string-ascii 256) {wallet-address: principal, tax-rate-percent: uint})
;; (define-map discretionary-surtax-info uint (string-ascii 256) {wallet-address: principal, tax-rate-percent: uint})
;; (define-map municipal_utility-tax-info uint (string-ascii 256) {wallet-address: principal, tax-rate-percent: uint})
;; (define-map tourist-development-tax-info uint (string-ascii 256) {wallet-address: principal, tax-rate-percent: uint})
(define-map payees 
  uint  
  {wallet-owner-name: (string-ascii 256), wallet-address: principal, tax-rate-percent: uint})
(define-map payee-token 
  {token-number: uint, payee-type: (string-ascii 256)}
  uint) 
;;these maps create a linked list accross contracts of token specific to a single parcel
(define-map parent-token-addresses uint {contract_address: principal, contract_name: (string-ascii 256), token_ID: uint})
(define-map child-token-addresses uint {contract_address: principal, contract_name: (string-ascii 256), token_ID: uint})

;;these are the total amounts due that must be paid in full
(define-map utility-bills
  uint
  {amt-due-utility: uint, sales-tax: uint, GRT: uint, MUT: uint, surtax: uint, franchise: uint})
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

;;list of global parameters ppaakk
(define-map parameters (string-ascii 256) {val-string: (string-ascii 256), val-number: uint})

;; Claim a new NFT
(define-public (claim)
  (mint tx-sender))

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
   (State (string-ascii 25))
   (County (string-ascii 25))
   (taxing-jurisdiction (string-ascii 25))
  )
    (begin
      (try! (claim))      
      (map-insert nft-data (var-get last-id) meta-data-url) 
      (map-insert parcel-numbers (var-get last-id) parcel-number) 
      (map-insert situs-addresses (var-get last-id) situs-address) 
      (map-insert legal-descriptions (var-get last-id) legal-description) 
      (map-insert GIS-coordinates (var-get last-id) GIS-coordinate) 
      (map-insert parent-token-addresses (var-get last-id) {contract_address: parent-contract-address, contract_name: parent-contract-name, token_ID: parent_token_num})
    ;;   (assign-payee (var-get last-id) electric-utility-name "electric") ;;ppaakk
    ;;   (assign-payee (var-get last-id) water-utility-name "water")
    ;;   (assign-payee (var-get last-id) State "State")
    ;;   (assign-payee (var-get last-id) County "County")
    ;;   (assign-payee (var-get last-id) taxing-jurisdiction "taxing-jurisdiction")
      (ok (var-get last-id))
    )
)

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
;;Sets parameter value in parameter table
(define-private (set-parameter (param-name (string-ascii 256)) (val-string  (string-ascii 256)) (val-number uint) ) 
   (begin
        (ok (map-insert parameters param-name {val-string: val-string, val-number: val-number }))
   )
)

(define-public (set-token-uri (nftIndex uint) (meta-data-url (string-ascii 256)))
   (begin
      (map-insert nft-data nftIndex meta-data-url)
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

;;gets a specifically named parameter from the parameter table ppaakk
(define-private (get-parameter (param-name (string-ascii 256)) ) 
   (begin
        (unwrap! (map-get? parameters param-name) (err err-no-value))         
        (ok false))
)

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

(define-read-only (get-token-uri2 (token-id uint))
    (ok (as-max-len? (concat (concat IPFS_ROOT (concat "guinea_" (uint-to-string token-id))) "_metadata.json") u256))
)
      
;;*********************************************************************
;;
;;        Helpful funcitons
;;
;;*********************************************************************

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

;;*********************************************************************
;;
;;        Tax and Utiltiy Bill Related Section
;;
;;*********************************************************************


;;***************************************************************************************
;;          Assignment of Payee to Parcel Token
;;          Function Name:  assign-payee
;;
;;          This cluster of functions is called during the mint process
;;          or as need to maitain a list of the payees to assign to each
;;          parcel token.  For nstance, if Duke Energy is the utility provider
;;          teh key to the map of payee for the row associated with Duke Energy
;;          is saved to the payee-token map.  There is a one-to-many relatioship
;;          with the NFT and the payee-token map.  When a bill is paid,
;;          the wallet addresses or each payee are gathered and the proceeds
;;          distributed to these wallets
;;
;;          loop values in the payee table to find utility names ppaakk
;;            insert pk's of the payee to a map with key{token#,payee-type} value {fk-payee}
;;              payee-type (water, electric, sales tax, MUT etc)
;;          loop values in the payee table to find jurisdiction name and tax type(uincorporated Orange/MUT)
;;              insert pk's of the payee to a map with key{token#,payee-type} value {fk-payee}
;;**************************************************************************************



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
(define-public (decrease-customer-funds  (token-number uint) (amount uint))
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

;; (define-private (pay (payment (optional uint)) (receiver principal)) 
;;     (begin
;;         (match payment
;;             amount 
;;                (if (> amount u0)
;;                     (as-contract (stx-transfer? amount tx-sender receiver))
;;                     (ok false)
;;                )
;;         (ok false))
;;     )
;; )


(define-public (reset-utility-bill  (token-number uint) )
    (begin
        (asserts! (> token-number u0) err-no-value) 
        (map-set utility-bills u1 {amt-due-utility: u0, sales-tax: u0, GRT: u0, MUT: u0, surtax: u0, franchise: u0 })         
        (ok true)
    )
)


;; (define-read-only (get-wallet_address? (token-number uint) (payee-type (string-ascii 256)))
;;     (let (
;;         (payee-id (unwrap!
;;             (map-get? payee-token { token-number:  token-number, payee-type: payee-type})
;;             (err err-no-value))) ;;the payee must have been set up for this token 
;;         (payee (unwrap!
;;             (map-get? payees payee-id)
;;             (err err-no-value))) ;;must have a wallet address for this payee
;;     )
;;        (ok (get wallet-address payee))
;;     )
;; )

;;ability to change wallet address restricted to wallet holder.  ORG cant change once set.
;;wallet 1: Duke Energy
;;Wallet 2: Florida Department of Revenue Sales Tax
;;Wallet 3: Florida Department of Revenue Gross Reciepts Tax
;;Wallet 4: Orange County Comptroller
;;Wallet 5: Florida Department of Revenue Discretionary Surtax
;; ;;Wallet 6: City of Orlando
;; (define-public (distribute-funds-received (token-number uint)) 
;;     (let 
;;         (
;;             (temp1 (map-get? utility-bills token-number))
;;             (amt-due-utility1 (get amt-due-utility temp1))
;;            ;; (amt-due-utility2 (get amt-due-utility temp1))
;;             (sales-tax (get sales-tax temp1))
;;             (GRT (get GRT temp1))
;;             (MUT (get MUT temp1))
;;             (franchise (get franchise temp1))
;;             (surtax (get surtax temp1))
;;             (wallet_address (unwrap! (get-wallet_address? token-number "electric") (err err-no-value)))
;;         )
;;         (asserts! (is-some amt-due-utility1) err-no-value)
;;         ;;(asserts! (is-some amt-due-utility2) err-no-value)
;;         (try! (pay amt-due-utility1 wallet_address)) 
;;         (ok true)
;;     )
;; )

(define-read-only (get-wallet_address? 
                    (token-number uint) 
                    (payee-type (string-ascii 256))) 
  (let ((payee-id (unwrap!
                    (map-get? payee-token 
                      {token-number: token-number, 
                       payee-type: payee-type})
                    (err err-no-value))) ;;the payee must have been set up for this token 
        (payee (unwrap!
                 (map-get? payees payee-id)
                 (err err-no-value))))  ;;must have a wallet address for this payee
  
     (ok (get wallet-address payee))))

(define-private (pay 
                  (payment (optional uint)) 
                  (receiver principal)) 
  (match payment
    amount (if (> amount u0)
              (as-contract (stx-transfer? amount tx-sender receiver))
              (ok false))
    (ok false)))

(define-public (pay-wallet (token-number uint) (payment (optional uint)) (payee-type (string-ascii 100)))
    (let (
        (wallet_address (unwrap! 
                (get-wallet_address? token-number payee-type)
                (err err-no-value)))
        (try! (pay payment wallet_address))
    )
    (ok true))) 

(define-public (distribute-funds-received 
                 (token-number uint))  
  (let ((temp1 (map-get? utility-bills token-number)))         
  (asserts! (is-some (get amt-due-utility temp1)) (err err-no-value))
  (begin 
    (and (is-some (get amt-due-utility temp1)) (try! (pay-wallet token-number (get amt-due-utility temp1) "electric")))
    (and (is-some (get amt-due-utility temp1)) (try! (pay-wallet token-number (get amt-due-utility temp1) "water")))
    (and (is-some (get sales-tax temp1)) (try! (pay-wallet token-number (get sales-tax temp1) "sales-tax")))
    (and (is-some (get GRT temp1)) (try! (pay-wallet token-number (get GRT temp1) "GRT")))
    (and (is-some (get MUT temp1)) (try! (pay-wallet token-number (get MUT temp1) "MUT")))
    (and (is-some (get franchise temp1)) (try! (pay-wallet token-number (get franchise temp1) "franchise")))
    (and (is-some (get surtax temp1)) (try! (pay-wallet token-number (get surtax temp1) "surtax")))
    (ok true))))
        ;;HERE YOU NEED TO GET THE APPLICABLE WALLET ADDRESS DYNAMICLLY
        ;;parameter table with descripton as key and value as param value
        ;;payee map has uint as pk with both utiltiy & jurisdiction names in a single row
        ;;payee/token table is this: key{token#,payee-type} value {fk-payee}
        ;;token is minted passing utility names for electric and water and local & state jurisdictions
        ;;  loop values in the payee table to find utility names
        ;;  insert pk's of the payee to a map with key{token#,payee-type} value {fk-payee}
        ;;      payee-type (water, electric, sales tax, MUT etc)
        ;;  loop values in the payee table to find jurisdiction name and tax type(uincorporated Orange/MUT)
        ;;  insert pk's of the payee to a map with key{token#,payee-type} value {fk-payee}
        ;;on payment, use the token# & payee type composit key in the payee table
        ;; to get fk to payee and use it to get wallet of utilities & jurisdictions to be paid
        ;;  
        ;;
        ;;set up a payee such as a utiltiy company or jurisdicton and save thier wallet address and other info ppaakk
(define-public (add-payee (payee-name (string-ascii 256)) (wallet-address principal))
        (let (
            (temp1 (map-get? parameters "payee-next-id"))
            (current-id (get val-number temp1)) 
            (match current-id
                id 
                (if (>= id u0)
                        (next-id (+ u1 current-id))
                        ;;(map-set parameters "payee-next-id" {val-string: "na", val-number: next-id})
                        (set-parameter "payee-next-id" {val-string: "na", val-number: next-id})
                        (ok next-id)
                )
                (   
                    (next-id (u1))
                   ;; (map-insert parameters "payee-next-id" {val-string: "na", val-number: u1})
                    (set-parameter "payee-next-id" {val-string: "na", val-number: u1})
                    (ok next-id)
                )
            )
            (map-insert payees next-id {wallet-owner-name: payee-name, wallet-address:  wallet-address, tax-rate-percent: u1000 } )
            )            
    (ok false))
)

