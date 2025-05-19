(ns akovantsev.pp
  (:require
   [clojure.string :as str]
   [com.akovantsev.blet.core :refer [blet blet!]]))


(def ^:dynamic *colored* true)
(def ^:dynamic *max-el-chars-len* 44) ;;uuid len
(def ^:dynamic *col-overflow-tolerance-chars-len* 15)
(def ^:dynamic *limit-seq-elements* 32)

(def ^:dynamic *sort* nil)
(def ^:dynamic *sort-vs* nil)
(def ^:dynamic *sort-kvs* nil)

;; depending on before or after -pps
(def MORE (symbol ",,,"))
(def MOREV [3 [MORE]])
(def MOREKV [MOREV MOREV])

(def COLORS
  {::RESET   "\033[0m"
   ::BLACK   "\033[30m"
   ::GREY    "\033[90m"
   ::RED     "\033[31m"
   ::REDU    "\033[31;4m"
   ::GREEN   "\033[32m"
   ::GREENB  "\033[92m"
   ::BLUE    "\033[34m"
   ::MAGENTA "\033[35m"
   ::CYAN    "\033[36m"})


(def -spaces-mem (fn [n] (str/join (repeat n " "))))

(declare -pps)

(defn -pscalar [x]
  (blet [k? (keyword? x)
         sk (str x)
         s? (re-find #"\s" sk)
         p  (pr-str x)
         n  (count p)
         c  (cond
              (nil? x)     ::MAGENTA
              (string? x)  ::GREY
              (symbol? x)  ::RED
              (number? x)  ::BLUE
              (boolean? x) ::MAGENTA
              (uuid? x)    ::CYAN
              k?           ::GREEN)]
    (cond
      (and k? s?)       (-pps (with-meta
                                (list 'keyword (subs sk 1))
                                {::call? true}))
      (and *colored* c) [n [(str (COLORS c) p (COLORS ::RESET))]]
      :else             [n [p]])))

(comment
  (mapv -pscalar [::MAGENTA 1 "yo" 1M (random-uuid) true nil (keyword "lel kek")]))


(defn -wrap [lenpref pref suf [len lines]]
  (let [lenpref (or lenpref (count pref))
        lensuf  (count suf)
        pad     (-spaces-mem lenpref)
        fst     (first lines)
        fst+    (str pref fst)
        lst     (peek lines)
        lst+    (str pad lst suf)
        lines-  (pop lines)
        -lines- (rest lines-)
        -lines* (mapv #(str pad %) -lines-)
        nlines  (count lines)
        LEN     (+ lenpref len lensuf)]
    (case nlines
      0 [LEN [(str pref ,,, suf)]]
      1 [LEN [(str pref fst suf)]]
      , [LEN (conj (into [fst+] -lines*) lst+)])))

(comment
  (-wrap 1 "{" "}" [0 []])
  (-wrap 1 "{" "}" [1 ["1"]])
  (-wrap 2 "#{" "}" (-pscalar ::MAGENTA))
  (-wrap 1 "[" "]" [4 ["lellel" "kekkek" ":goof" "1" "true"]])
  (-wrap 1 "[" "]" (-wrap 1 "{" "}" [3 ["1 2" "3 4"]])))


(defn -stitch-row [els]
  (->> els
    (reduce
      (fn rf1 [[llen left] right]
        (let [prev- (pop left)
              gap   " "
              pref  (str (peek left) gap)
              [rlen right*] (-wrap (inc llen) pref "" right)]
          [rlen (into prev- right*)])))))

(defn -stitch-col [els]
  (->> els
    (reduce
      (fn rf1 [[tlen top] [blen bot]]
        [blen (into top bot)]))))

(comment
  (-stitch-row [[6 ["lellel"]] [6 ["kekkek"]] [5 [":goof"]] [1 ["1"]] [4 ["true"]]])
  (-stitch-col [[4 ["abc" "defg"]]
                [4 ["abc" "defg"]]]))


(defn -partition-all-by-len [max-row-len els]
  (let [els     (-> els vec)
        lastidx (-> els count dec)]
    (loop [idx      0
           grouplen 0
           group    []
           groups   []]
      (blet [idx+      (inc idx)
             el        (get els idx)
             ellen     (first el)
             group+    (conj group el)
             groups+   (conj groups group)
             grouplen+ (+ grouplen ellen 1)]
        (cond
          (< lastidx idx)            groups+
          (= 0 grouplen)             (recur idx+ grouplen+ group+ groups)
          (<= grouplen+ max-row-len) (recur idx+ grouplen+ group+ groups)
          (< max-row-len grouplen+)  (recur idx 0 [] groups+))))))

(comment
  (-partition-all-by-len 10
    (shuffle [[1] [5] [3] [11] [1] [2] [3] [1] [2] [3]])))


(defn -partition-all-by-ncols [ncols els]
  ;;rows are already limited
  (let [OVERFLOW    'OVERFLOW
        LAST        'LAST
        maxwidth    (+ *max-el-chars-len* *col-overflow-tolerance-chars-len*)
        nels        (count els)
        els         (vec els)]
    ;; 1) render each el,
    ;; 2) collect actual max col widths
    (loop [idx   0
           maxes {}
           !done (transient [])]
      (blet [x         (get els idx)
             idx+      (+ 1 idx)
             n         (mod idx ncols)
             is-last   (or
                         (= 0 (mod idx+ ncols))
                         (= idx+ nels))
             is-done   (= idx nels)
             len       (first x)
             rows      (second x)
             is-overfl (< maxwidth len)
             len       (min len maxwidth)
             maxes+    (if is-overfl
                         maxes
                         (update maxes n (fnil max 0) len))
             rows      (cond
                         is-last   (conj rows LAST)
                         is-overfl (conj rows OVERFLOW)
                         :else     rows)
             !done+    (conj! !done [len rows])]
        (if-not is-done
          (recur idx+ maxes+ !done+)
          (let [padright (fn padright [idx [len rows]]
                           (blet [n         (mod idx ncols)
                                  ;; max len if every in column is overflown:
                                  ;; extra 1 pad
                                  maxlen    (+ 1 (get maxes n *max-el-chars-len*))
                                  lastrow   (peek rows)
                                  rows-     (pop rows)
                                  padlen    (- maxlen len)
                                  spacesmin (-spaces-mem padlen)
                                  spacesmax (-spaces-mem maxlen)]
                             (cond
                               (= lastrow LAST)     [len rows-]
                               (= lastrow OVERFLOW) [maxlen (conj rows- spacesmax)]
                               :else                [maxlen (conj rows- (str lastrow spacesmin))])))]
            (->> !done
              (persistent!)
              ;; 3) set last lines in each el considering maxcolwidth and el overflows:
              (map-indexed padright)
              (partition-all ncols))))))))


(defn -pack [ncols els]
  (let [partition-fn (if ncols
                       (partial -partition-all-by-ncols ncols)
                       (partial -partition-all-by-len *max-el-chars-len*))]
    (->> els
      vec
      partition-fn
      (mapv -stitch-row)
      -stitch-col)))

(defn -limited [placeholder x]
  (if (some-> *limit-seq-elements* inc (bounded-count x) (> *limit-seq-elements*))
    (-> (take *limit-seq-elements* x) vec (conj , placeholder) (with-meta , (meta x)))
    x))

(comment
  (-limited 'MORE (range)))




(defn -kv-height-short-first-az
  [[[w1 k1] [_ v1]]
   [[w2 k2] [_ v2]]]
  (compare
    [(min 10 (count v1)) w1 (first k1)]
    [(min 10 (count v2)) w2 (first k2)]))

(defn -v-height-short-first-az [[w1 v1] [w2 v2]]
  (compare
    [(count v1) w1 (first v1)]
    [(count v2) w2 (first v2)]))

(defn -kv-az [[[_ k1] [_ v1]] [[_ k2] [_ v2]]]
  (compare (first k1) (first k2)))

(defn -v-az [[_ v1] [_ v2]]
  (compare (first v1) (first v2)))


(defmulti sort-kvs               (fn [_kvs] (or *sort-kvs* *sort*)))
(defmethod sort-kvs               nil [kvs] kvs)
(defmethod sort-kvs ::sort-short-tall [kvs] (sort -kv-height-short-first-az kvs))
(defmethod sort-kvs ::sort-az         [kvs] (sort -kv-az kvs))

(defmulti sort-vs               (fn [_vs] (or *sort-vs* *sort*)))
(defmethod sort-vs               nil [vs] vs)
(defmethod sort-vs ::sort-short-tall [vs] (sort -v-height-short-first-az vs))
(defmethod sort-vs ::sort-az         [vs] (sort -v-az vs))




(defn -pset [x] (-wrap 2 "#{" "}" (-pack (-> x meta ::ncols) (->> x (mapv -pps) sort-vs (-limited MOREV) vec))))
(defn -pseq [x] (-wrap 2 "'(" ")" (-pack (-> x meta ::ncols) (->> x (-limited MORE) (mapv -pps)))))
(defn -pvec [x] (-wrap 1 "["  "]" (-pack (-> x meta ::ncols) (->> x (-limited MORE) (mapv -pps)))))
(defn -pcal [x] (-wrap 1 "("  ")" (-pack (-> x meta ::ncols) (->> x (mapv -pps)))))
(defn -pmap [x] (-wrap 1 "{"  "}" (-pack 2 (->> x (mapv #(mapv -pps %)) sort-kvs (-limited MOREKV) (reduce into [])))))

(defn -pps  [x]
  (let [c? (-> x meta ::call?)
        f  (cond
             c?          -pcal
             (map? x)    -pmap
             (vector? x) -pvec
             (set? x)    -pset
             (list? x)   -pseq
             (seq? x)    -pseq
             :else       -pscalar)]
    (f x)))

(defn string [x] (str/join "\n" (second (-pps x))))


(comment
  (defn- $ [x] (println (pr-str x)) x)
  (defn spy [x] (println (string x)) x)
  (binding [*sort* ::sort-az
            *max-el-chars-len* 44
            *limit-seq-elements* 32
            *col-overflow-tolerance-chars-len* 15]
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
        {3 4 (keyword "lel kek") 6 (range) (with-meta (range) {::ncols 5})
         [12312312312 123 213 123 1231231 312 123 123 123 123
          [12312312312 123 213 123 1231231 312 123 123 123 123]]
         [12312312312 123 213 123 1231231 312 123 123 123 123 112312312 123 213 123 129331231 312 123 123 123 123]
         (into #{} [1 2 4 7907 31 312 123 123 123 123 12312312312 123 213 786789789 123 1231231 312 123 123 123 123]) ::lel
         1 2
         (into #{} [0 12312312312 123 211233 123 12312456 45647 5786 7907 31 312 123 123 123 123 12312312312 123 213 786789789 123 1231231 312 123 123 123 123]) ::kek
         (range 10 100) (range)}
        spy
        (do nil)))))

