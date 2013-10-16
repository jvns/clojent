(ns clojent.parser)

(def test-str
    "d8:announce40:http://tracker.thepiratebay.org/announce13:announce-list5:blahe")


(defn bdecode [s]
  (parse-value s))

; Returns
; - a list of byte strings
; - the remainder of the string
(defn parse-bytes [s]
    (let [parts (split-with #(not= \: %) s)
          ntoread (->> parts first (apply str) read-string)
          secondpart (->> parts second (drop 1))
          bytestr (take ntoread secondpart)
          rst (drop ntoread secondpart)
        ]
        {:val (apply str bytestr) :rest rst}
    ))

(defn end-char? [c] (not= c \e))
(defn parse-int [s]
    (let [parts (split-with end-char? s)]
        {
            :val
            (->> parts first (apply str) read-string)
            :rest
            (->> parts second (drop 1) (apply str))
        }
    ))

(declare parse-list)

(defn parse-value [s]
    (let [fst (first s)
          rst (rest s)]
        (case fst
            \i (parse-int rst)
            \l (parse-list rst)
            \d (parse-dict rst)
            (parse-bytes s)
        )))

(defn parse-list [s]
  (if (= (first s) \e)
    {:val [] :rest (rest s)}
    (let [fstval (parse-value s)
          othervals (parse-list (:rest fstval))]
      { :val (cons (:val fstval) (:val othervals))
        :rest (:rest othervals)
       })))

(defn parse-dict [s]
  (let [list-vals (parse-list s)]
    {:val (apply hash-map (:val list-vals))
     :rest (:rest list-vals)
     }))

(parse-int "10ei20e")

(parse-list "i10ei10ei150ee")

(parse-list "3:abc4:abcde")

(parse-bytes "3:abc4:abcd")

(parse-bytes (:rest (parse-bytes "3:abc4:abcde")))

(parse-value ( :rest (parse-value "i10ei20ee") ))

(parse-value "d3:abc4:abcde")



'(
(assert (=
    (parse-int "10ei20e")
    {:val 10 :rest "i20e"}))

(assert (=
    (parse-value "i10ei20e")
    {:val 10 :rest "i20e"}))

(assert (=
    (parse-list "li10ei20e")
    {:val [10,20] :rest ""}))
)
