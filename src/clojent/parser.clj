(def test-str 
    "d8:announce40:http://tracker.thepiratebay.org/announce13:announce-list5:blahe")

(defn end-char? [c] (not= c \e))

(defn bdecode [s]
    2)

(defn parse-dict [s]
    (print "dict")
    3
    )

; Returns
; - a list of byte strings
; - the remainder of the string
(defn parse-bytes [s]
    (print "bytes")
    (let [parts (split-with #(not= \: %) s)
          ntoread (->> parts first (apply str) read-string)
          secondpart (->> parts second (drop 1))
          bytestr (take ntoread secondpart)
          rst (drop ntoread secondpart)
        ]
        {:val bytestr :rest rst}
    ))

(defn parse-list [s]
    (print "list")
    (loop [remaining s values []]
        (println remaining)
        (if (= (first remaining) \e)
            {:val values :rest (rest remaining)}
            (let [result (parse-value s)
                  value (:val result)] 
                  (recur (:rest result) (concat values [value]))))))

(defn parse-int [s]
    (let [parts (split-with end-char? s)]
        {
            :val 
            (->> parts first (apply str) read-string)
            :rest 
            (->> parts second (drop 1) (apply str))
        }
    ))

(defn parse-value [s]
    (let [fst (first s)
          rst (subs s 1)]
        (case fst 
            \i (parse-int rst)
            \l (parse-list rst)
            \d (parse-dict rst)
            (parse-bytes s)
        )))

(assert (= 
    (parse-int "10ei20e")
    {:val 10 :rest "i20e"}))

(assert (= 
    (parse-value "i10ei20e")
    {:val 10 :rest "i20e"}))

(assert (= 
    (parse-list "li10ei20e")
    {:val [10,20] :rest ""}))