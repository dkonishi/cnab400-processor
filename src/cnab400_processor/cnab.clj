(ns cnab400-processor.cnab
  (:require [clj-time.format :as f]
            [clojure.string :as s]))

(defn- to-string [x] x)

(defn- to-string-trim [x] (.trim x))

(defn- to-int [x] (Integer. x))

(defn- to-bigdec [x] (/ (bigdec x) 100M))

(def formatter-2 (f/formatter "ddMMyy"))

(def formatter-4 (f/formatter "ddMMyyyy"))

(defn- to-date [x formatter]
  (try
    (if (s/blank? x)
      x
      (f/parse formatter x))
    (catch org.joda.time.IllegalFieldValueException e
      "")))

(defn- to-date-2 [x] (to-date x formatter-2))

(defn- to-date-4 [x] (to-date x formatter-4))

(def cnab400-header
  [:register-type           1      1       to-string
   :return-code             2      2       to-string
   :literal-return          3      9       to-string-trim
   :service-code            10     11      to-string
   :literal-service         12     26      to-string-trim
   :agency                  27     30      to-string
   :zeros                   31     32      to-string
   :account                 33     37      to-string
   :digit                   38     38      to-string
   :whites                  39     46      to-string
   :client-name             47     76      to-string-trim
   :bank-code               77     79      to-string
   :bank-name               80     94      to-string-trim
   :date-writing            95     100     to-date-2
   :density                 101    105     to-string
   :density-unit            106    108     to-string
   :sequential-return       109    113     to-string
   :credit-date             114    119     to-date-2
   :whites-2                120    394     to-string
   :sequential-number       395    400     to-int])

(def cnab400-entry
  [:register-type           1     1        to-string
   :inscription-code        2     3        to-string
   :inscription-number      4     17       to-string
   :agency                  18    21       to-string
   :zeros                   22    23       to-string
   :account                 24    28       to-string
   :digit                   29    29       to-string
   :whites                  30    37       to-string
   :company-use             38    62       to-string
   :our-number              63    70       to-string
   :whites-2                71    82       to-string
   :wallet                  83    85       to-string
   :our-number-2            86    93       to-string
   :our-number-digit        94    94       to-string
   :whites-3                95    107      to-string
   :wallet                  108   108      to-string
   :occurrence-code         109   110      to-string
   :occurrence-date         111   116      to-date-2
   :document-number         117   126      to-string
   :our-number-3            127   134      to-string
   :whites-4                135   146      to-string
   :due-date                147   152      to-date-2
   :value                   153   165      to-bigdec
   :bank-code               166   168      to-string
   :bank-agency             169   172      to-string
   :bank-agency-digit       173   173      to-string
   :species                 174   175      to-string
   :billing-fare            176   188      to-bigdec
   :whites-5                189   214      to-string
   :iof                     215   227      to-bigdec
   :reduction-value         228   240      to-bigdec
   :discount                241   253      to-bigdec
   :main-value              254   266      to-bigdec
   :interest                267   279      to-bigdec
   :other-credits           280   292      to-bigdec
   :invoice-digit           293   293      to-string
   :whites-6                294   295      to-string
   :credit-date             296   301      to-date-2
   :cancelled-instruction   302   305      to-string
   :whites-7                306   311      to-string
   :zeros-2                 312   324      to-string
   :name                    325   354      to-string-trim
   :whites-8                355   377      to-string
   :errors                  378   385      to-string
   :whites-9                386   392      to-string
   :liquidation-code        393   394      to-string
   :sequential-number       395   400      to-int])

(def cnab400-trailer
  [:register-type           1     1        to-string
   :return-code             2     2        to-string
   :service-code            3     4        to-string
   :bank-code               5     7        to-string
   :whites                  8     17       to-string
   :titles-amount           18    25       to-string
   :total-value             26    39       to-bigdec
   :bank-advisory           40    47       to-string
   :whites-2                48    57       to-string
   :titles-amount-2         58    65       to-string
   :total-value-2           66    79       to-bigdec
   :bank-advisory-2         80    87       to-string
   :whites-3                88    177      to-string
   :titles-amount-3         178   185      to-string
   :total-value-3           186   199      to-bigdec
   :bank-advisory-3         200   207      to-string
   :file-control            208   212      to-string
   :details-amount          213   220      to-string
   :total-value-informed    221   234      to-bigdec
   :whites-4                235   394      to-string
   :sequential-number       395   400      to-int])

(defn- parse-line [[map-key start end parser & more] line]
  (let [small-map {map-key (parser (.substring line (dec start) end))}]
    (cond
      (nil? more) small-map
      :else (merge small-map (parse-line more line)))))

(defn- validate-header-400 [header]
  (assert (= "0" (:register-type header)))
  (assert (= "2" (:return-code header)))
  (assert (= "RETORNO" (:literal-return header)))
  (assert (= "01" (:service-code header)))
  (assert (= "COBRANCA" (:literal-service header)))
  (assert (= "00" (:zeros header)))
  (assert (= "341" (:bank-code header)))
  (assert (= "BPI" (:density-unit header)))
  (assert (= 1 (:sequential-number header))))

(defn- validate-entry-400 [entry]
  (assert (= "1" (:register-type entry)))
  (assert (= "00" (:zeros entry)))
  (assert (= (:our-number entry) (:our-number-2 entry)))
  (assert (= (:our-number entry) (:our-number-3 entry)))
  (assert (= (:wallet entry) (:wallet-2 entry))))

(defn- validate-trailer-400 [trailer]
  (assert (= "9" (:register-type trailer)))
  (assert (= "2" (:return-code trailer)))
  (assert (= "01" (:service-code trailer)))
  (assert (= (:title-amount trailer) (:title-amount-2 trailer)))
  (assert (= (:title-amount trailer) (:title-amount-3 trailer))))

(defn cnab400 [input]
  (let [lines            (s/split-lines input)

        original-header  (first lines)
        original-entries (butlast (rest lines))
        original-trailer (last lines)

        parsed-header    (parse-line cnab400-header original-header)
        parsed-entries   (map #(parse-line cnab400-entry %) original-entries)
        parsed-trailer   (parse-line cnab400-trailer original-trailer)]

    (validate-header-400 parsed-header)

    (map validate-entry-400 parsed-entries)

    (validate-trailer-400 parsed-trailer)

    {:cnab400 {:header parsed-header
               :entries parsed-entries
               :trailer parsed-trailer}}))

(def cnab240-header
  [:bank-code            1     3           to-string
   :lot-code             4     7           to-string
   :register-type        8     8           to-string
   :whites               9     14          to-string
   :file-layout          15    17          to-string
   :inscription-code     18    18          to-string
   :inscription-number   19    32          to-string
   :whites-2             33    52          to-string
   :agency               53    57          to-string
   :whites-3             58    58          to-string
   :account              59    70          to-string
   :whites-4             71    71          to-string
   :account-digit        72    72          to-string
   :client-name          73    102         to-string-trim
   :bank-name            103   132         to-string-trim
   :whites-5             133   142         to-string
   :file-code            143   143         to-string
   :writing-date         144   151         to-date-4
   :writing-time         152   157         to-string
   :zeros                158   166         to-string
   :density-unit         167   171         to-string
   :whites-6             172   240         to-string])

(def cnab240-lot-header
  [:bank-code            1     3           to-string
   :lot-code             4     7           to-string
   :register-type        8     8           to-string
   :operation-type       9     9           to-string
   :payment-type         10    11          to-string
   :payment-style        12    13          to-string
   :lot-layout           14    16          to-string
   :whites               17    17          to-string
   :inscription-code     18    18          to-string
   :inscription-number   19    32          to-string
   :throwing-id          33    36          to-string
   :whites               37    52          to-string
   :agency               53    57          to-string
   :whites-2             58    58          to-string
   :account              59    70          to-string
   :whites-3             71    71          to-string
   :account-digit        72    72          to-string
   :client-name          73    102         to-string-trim
   :lot-reason           103   132         to-string
   :account-history      133   142         to-string
   :client-address       143   172         to-string-trim
   :client-number        173   177         to-string
   :client-complement    178   192         to-string
   :client-city          193   212         to-string-trim
   :client-cep           213   220         to-string
   :client-state         221   222         to-string
   :whites-4             223   230         to-string
   :occurrences          231   240         to-string])

(def cnab240-segment-o


(def cnab240-lot-trailer
    [:bank-code         1     3           to-string
     :lot-code          4     7           to-string
     :register-type     8     8           to-string
     :whites            9     17          to-string
     :register-amount   18    23          to-int
     :payment-total     24    41          to-bigdec
     :currency-total    42    56          to-bigdec
     :whites            57    230         to-string
     :occurrences       231   240         to-string])

(def cnab240-trailer
  [:bank-code         1    3           to-string
   :lot-code          4    7           to-string
   :register-type     8    8           to-string
   :whites            9    17          to-string
   :lot-amount        18   23          to-int
   :register-amount   24   29          to-int
   :whites            30   240         to-string])

(defn- validate-header-240 [header]
  (assert (= "0" (:register-type header)))
  (assert (= "0000" (:lot-code header)))
  (assert (= "341" (:bank-code header)))
  (assert (= "081" (:file-layout header))))

(defn- validate-lot-header-240 [header]
  (assert (= "1" (:register-type header)))
  (assert (= "341" (:bank-code header))))

(defn- validate-segment-o [entry]
  (assert (= "3" (:register-type entry)))
  (assert (= "341" (:bank-code entry)))
  (assert (= "O" (:segment entry)))
  (assert (= "REA" (:currency entry))))

(defn- validate-lot-trailer [trailer entries]
  (assert (= "5" (:register-type trailer)))
  (assert (= "341" (:bank-code trailer)))
  (assert (= (+ 2 (count entries)) (:register-amount trailer))))

(defn- validate-trailer-240 [trailer lines]
  (assert (= "9" (:register-type trailer)))
  (assert (= "341" (:bank-code trailer)))
  (assert (= "9999" (:lot-code trailer)))
  (assert (= 1 (:lot-amount trailer)))
  (assert (= (count lines) (:register-amount trailer))))

; Currently only one lot per file is supported.
(defn cnab240 [input]
  (let [lines                (s/split-lines input)

        original-header      (first lines)
        original-lot-header  (first (rest lines))
        original-entries     (-> lines rest rest butlast butlast)
        original-lot-trailer (-> lines butlast last)
        original-trailer     (last lines)

        parsed-header        (parse-line cnab240-header original-header)
        parsed-lot-header    (parse-line cnab240-lot-header original-lot-header)
        parsed-entries       (map #(parse-line cnab240-segment-o %) original-entries)
        parsed-lot-trailer   (parse-line cnab240-lot-trailer original-lot-trailer)
        parsed-trailer       (parse-line cnab240-trailer original-trailer)]

    (validate-header-240 parsed-header)
    (validate-lot-header-240 parsed-lot-header)

    (map validate-segment-o parsed-entries)

    (validate-lot-trailer parsed-lot-trailer parsed-entries)
    (validate-trailer-240 parsed-trailer lines)

    {:cnab240 {:header parsed-header
               :lot {:header parsed-lot-header
                     :entries parsed-entries
                     :trailer parsed-lot-trailer}
               :trailer parsed-trailer}}))
