(ns ofx-clj.core
  (import [java.io FileInputStream]
          [java.util Date]
          [net.sf.ofx4j.io AggregateUnmarshaller]
          [net.sf.ofx4j.domain.data MessageSetType ResponseEnvelope ResponseMessageSet]
          [net.sf.ofx4j.domain.data.signon SignonResponse FinancialInstitution]
          [net.sf.ofx4j.domain.data.common Status StatusCode TransactionList Transaction
           Currency Payee BalanceInfo]
          [net.sf.ofx4j.domain.data.banking BankStatementResponseTransaction
           BankStatementResponse BankAccountDetails]
          [net.sf.ofx4j.domain.data.creditcard CreditCardAccountDetails 
           CreditCardResponseMessageSet CreditCardStatementResponseTransaction
           CreditCardStatementResponse]))

(defmacro obj-to-map
  "A macro which converts an object into a map, using user-defined
  bindings.

  The macro can be used in the following way:
  (def c (java.util.Calendar/getInstance))
  (obj-to-map c
              :time .getTimeInMillis
              :alt-time [.getTime .getTime])

  The first binding returns the milliseconds
  directly from the calendar, while the second
  form uses the getTime() function to first
  get a java.util.Date, and calls getTime()
  on that date to get the milliseconds."
  [obj & bindings]
  (let [lobj (gensym "object")]
    `(let [~lobj ~obj]
       ~(into {} (for [[key value] (partition 2 bindings)]
                   (if (vector? value)
                     [key (loop [funcs value
                                 result lobj]
                            (if (nil? funcs)
                              result
                              (recur (next funcs)
                                     (list (first funcs) result))))]
                     [key (list value lobj)]))))))

(defn- read-file
  "Read an OFX file using the OFX4J library."
  [ofx-file]
  (.unmarshal
    (AggregateUnmarshaller. ResponseEnvelope)
    (FileInputStream. ofx-file)))

(defn- parse-amount
  "Parse the given string into a BigDecimal.
  Returns nil if input string in nil."
  [amount]
  (if (nil? amount)
    nil
    (BigDecimal. (str amount))))

;;; PARSE DATA ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmulti parse-data
  "Parse OFX data from an OFX4J unmarshalled object"
  class)

(defmethod parse-data Date
  [date]
  (.getTime date))

(defmethod parse-data Currency
  [currency]
  (obj-to-map currency
              :code .getCode
              :exchange-rate .getExchangeRate))

(defmethod parse-data FinancialInstitution
  [institution]
  (obj-to-map institution
              :id .getId
              :organization .getOrganization))

(defmethod parse-data StatusCode
  [code]
  (obj-to-map code
              :code .getCode
              :default-severity [.getDefaultSeverity str]
              :message .getMessage))

(defmethod parse-data Status
  [status]
  (obj-to-map status
              :code [.getCode parse-data]
              :severity [.getSeverity str]
              :message .getMessage))

(defmethod parse-data BalanceInfo
  [balance]
  (obj-to-map balance
              :amount [.getAmount parse-amount]
              :as-of-date [.getAsOfDate parse-data]))

(defmethod parse-data CreditCardAccountDetails
  [account]
  (obj-to-map account
              :account-key .getAccountKey
              :account-number .getAccountNumber))

(defmethod parse-data BankAccountDetails
  [account]
  (obj-to-map account
              :account-key .getAccountKey
              :account-number .getAccountNumber
              :account-type [.getAccountType str]
              :bank-id .getBankId
              :branch-id .getBranchId))

(defmethod parse-data Payee
  [payee]
  (obj-to-map payee
              :address1 .getAddress1
              :address2 .getAddress2
              :address3 .getAddress3
              :city .getCity
              :country .getCountry
              :name .getName
              :phone .getPhone
              :state .getState
              :zip .getZip))

(defmethod parse-data Transaction
  [transaction]
  (obj-to-map transaction
              :amount [.getAmount parse-amount]
              :bank-account-to [.getBankAccountTo parse-data]
              :check-number .getCheckNumber
              :correction-action [.getCorrectionAction str]
              :correction-id .getCorrectionId
              :creditcard-account-to [.getCreditCardAccountTo parse-data]
              :currency [.getCurrency parse-data]
              :date-available [.getDateAvailable parse-data]
              :date-initiated [.getDateInitiated parse-data]
              :date-posted [.getDatePosted parse-data]
              :id .getId
              :memo .getMemo
              :name .getName
              :original-currency [.getOriginalCurrency parse-data]
              :payee [.getPayee parse-data]
              :payee-id .getPayeeId
              :reference-number .getReferenceNumber
              :standard-industrial-code .getStandardIndustrialCode
              :temp-id .getTempId
              :transaction-type [.getTransactionType str]))

(defmethod parse-data TransactionList
  [transactions]
  (obj-to-map transactions
              :end [.getEnd parse-data]
              :start [.getStart parse-data]
              :transactions [.getTransactions #(map parse-data %)]))

(defmethod parse-data BankStatementResponse
  [response]
  (obj-to-map response
              :account [.getAccount parse-data]
              :available-balance [.getAvailableBalance parse-data]
              :currency-code .getCurrencyCode
              :ledger-balance [.getLedgerBalance parse-data]
              :marketing-info .getMarketingInfo
              :response-message-name .getResponseMessageName
              :transaction-list [.getTransactionList parse-data]))

(defmethod parse-data BankStatementResponseTransaction
  [transaction]
  (obj-to-map transaction
              :message [.getMessage parse-data]
              :wrapped-message [.getWrappedMessage parse-data]))

(defmethod parse-data SignonResponse
  [response]
  (obj-to-map response
              :access-key .getAccessKey
              :account-last-updated .getAccountLastUpdated
              :financial-institution [.getFinancialInstitution parse-data]
              :language .getLanguage
              :profile-last-updated .getProfileLastUpdated
              :response-message-name .getResponseMessageName
              :session-id .getSessionId
              :status [.getStatus parse-data]
              :status-holder-name .getStatusHolderName
              :timestamp .getTimestamp
              :user-key .getUserKey
              :user-key-expiration .getUserKeyExpiration))

(defmethod parse-data ResponseMessageSet
  [message-set]
  (obj-to-map message-set
              :type [.getType str]
              :version .getVersion
              :messages [.getResponseMessages #(map parse-data %)]))

(defmethod parse-data ResponseEnvelope
  [envelope]
  (map parse-data (.getMessageSets envelope)))

(defmethod parse-data CreditCardResponseMessageSet
  [message-set]
  (obj-to-map message-set
              :type [.getType str]
              :version .getVersion
              :messages [.getResponseMessages #(map parse-data %)]))

(defmethod parse-data CreditCardStatementResponseTransaction
  [transaction]
  (obj-to-map transaction
              :message [.getMessage parse-data]
              :wrapped-message [.getWrappedMessage parse-data]))

(defmethod parse-data CreditCardStatementResponse
  [response]
  (obj-to-map response
              :account [.getAccount parse-data]
              :available-balance [.getAvailableBalance parse-data]
              :currency-code .getCurrencyCode
              :ledger-balance [.getLedgerBalance parse-data]
              :marketing-info .getMarketingInfo
              :response-message-name .getResponseMessageName
              :transaction-list [.getTransactionList parse-data]))

(defmethod parse-data :default
  [data]
  (if (nil? data)
    nil
    {:class (str "----- " (class data) " -----")}))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn- find-message-set
  "Find messages relating to the given type from a
  map returned by the parse function."
  [data type]
  (filter #(= (:type %) type) data))

(defn- find-responses
  "Find responses in the given message set."
  [message-set]
  (apply #(:messages %) message-set))

(defn- find-messages
  "Find all the messages in the given list of responses."
  [responses]
  (map #(:message %) responses))

(defn- find-transactions
  "Find all the transactions in a given list of messages."
  [messages]
  (apply #(-> % :transaction-list :transactions) messages))

(defn find-all-transactions
  "Find all the transactions of a certain type in a
  map returned by the parse function.

  For example,
  (find-all-transactions (parse \"/path/to/file\") \"banking\".)"
  [data type]
  (find-transactions (find-messages (find-responses (find-message-set data type)))))

(defn parse
  "Parse the given OFX file using the OFX4J parser."
  [file]
  (map parse-data (.getMessageSets (read-file file))))
