(ns akovantsev.pp-test
  (:require
   [akovantsev.pp :as pp :refer [$]]
   [clojure.string :as str]))


(binding [pp/*sort* ::pp/sort-az
          pp/*log-fn* pp/tap-prn
          pp/*max-el-chars-len* 44
          pp/*limit-seq-elements* 32
          pp/*col-overflow-tolerance-chars-len* 15]
  (let [randlen    #(+ 5 (rand-int 10))
        randword   #(->> "qwertyuioplkjhgfdsazxcvbnm" seq shuffle (take (randlen)) str/join)
        randnum    #(rand-int 10000000)
        randkw     #(keyword (randword))
        randqkw    #(keyword (randword) (randword))
        randbool   #(rand-nth [true false nil])
        randscalar #((rand-nth [randbool randbool randkw randkw randqkw randqkw randword randword randword randnum randnum #_random-uuid]))
        randset    #(set (repeatedly (randlen) randscalar))
        randvec    #(vec (repeatedly (randlen) randscalar))
        randmap    #(apply hash-map (repeatedly (* 2 (randlen)) randscalar))
        random     #((rand-nth [randscalar randscalar randscalar  randscalar randmap randset randvec]))]
    (->
      ;(repeatedly 100 random)
      ;(with-meta {::ncols 3})
      {3 4 (keyword "lel kek") ($ 6) (range) (with-meta (range) {::ncols 5})
       [12312312312 123 213 123 1231231 312 123 123 123 123
        #for-akovantsev-pp-test [12312312312 123 213 123 1231231 312 123 123 123 "datareader"]]
       [12312312312 123 213 123 1231231 312 123 123 123 123 112312312 123 213 123 129331231 312 123 123 123 123]
       (into #{} [1 2 4 7907 31 312 123 123 123 123 12312312312 123 213 786789789 123 1231231 312 123 123 123 123]) ::lel
       1 2
       (into #{} [0 12312312312 123 211233 123 12312456 45647 5786 7907 31 312 123 123 123 123 12312312312 123 213 786789789 123 1231231 312 123 123 123 123]) ::kek
       (range 10 100) (range)}
      $
      (do nil))))


(binding [pp/*log-fn* pp/tap-prn]
  ($ (macroexpand-1 '($ (let [{:keys [::lel ::kek]} {::lel 1}]
                          (/ (clojure.string/lower-case lel) kek))))))


;; this must throw, testing try catch in let spy function:
(binding [pp/*log-fn* pp/tap-prn]
  #for-akovantsev-pp-test
  (let [{:keys [::lel ::kek]} {::lel 1}
        pek (/ (clojure.string/lower-case lel) kek)]
    pek))