(ns akovantsev.pp
  #?(:cljs (:require-macros [akovantsev.pp :refer [$ locals data-reader-basecase-macro]]))
  (:require
   [clojure.string :as str]
   [clojure.walk :as walk]
   [com.akovantsev.blet.core :refer [blet blet!]]))


(def ^:dynamic *colored* true)
(def ^:dynamic *max-el-chars-len* 44) ;;uuid len
(def ^:dynamic *col-overflow-tolerance-chars-len* 15)
(def ^:dynamic *limit-seq-elements* 100)

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
  (if (empty? els)
    [0 [""]]
    (->> els
      (reduce
        (fn rf1 [[llen left] right]
          (let [prev- (pop left)
                gap   " "
                pref  (str (peek left) gap)
                [rlen right*] (-wrap (inc llen) pref "" right)]
            [rlen (into prev- right*)]))))))

(defn -stitch-col [els]
  (if (empty? els)
    [0 [""]]
    (->> els
      (reduce
        (fn rf1 [[tlen top] [blen bot]]
          [blen (into top bot)])))))

(comment
  (-stitch-row [[6 ["lellel"]] [6 ["kekkek"]] [5 [":goof"]] [1 ["1"]] [4 ["true"]]])
  (-stitch-col [])
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



(def ^:dynamic *quoted?* false)

(defn -pset [x] (-wrap 2 "#{" "}" (-pack (-> x meta ::ncols) (->> x (mapv -pps) sort-vs (-limited MOREV) vec))))
(defn -pseq [x] (binding [*quoted?* true]
                  (-wrap 2 "'(" ")" (-pack (-> x meta ::ncols) (->> x (-limited MORE) (mapv -pps))))))
(defn -pvec [x] (-wrap 1 "["  "]" (-pack (-> x meta ::ncols) (->> x (-limited MORE) (mapv -pps)))))
(defn -pcal [x] (-wrap 1 "("  ")" (-pack (-> x meta ::ncols) (->> x (mapv -pps)))))
(defn -pmap [x] (-wrap 1 "{"  "}" (-pack 2 (->> x (mapv #(mapv -pps %)) sort-kvs (-limited MOREKV) (reduce into [])))))
(defn -pmet [x] (-wrap 1 "^"  "" (-pps (with-meta x (-> x meta (dissoc ::meta?))))))

(defn -pps  [x]
  (let [c? (-> x meta ::call?)
        m? (-> x meta ::meta?)
        f  (cond
             m?          -pmet
             c?          -pcal
             (map? x)    -pmap
             (vector? x) -pvec
             (set? x)    -pset
             (list? x)   (if *quoted?* -pcal -pseq)
             (seq? x)    (if *quoted?* -pcal -pseq)
             :else       -pscalar)]
    (f x)))

(defn string [x] (str/join "\n" (second (-pps x))))



;; SPY
;; cljs wip, sorry:



(def -LOCK #?(:clj (Object.) :cljs nil))


;; dont forget to wrap in bindings:
(defn tap-prn [x]
  (locking -LOCK
    (cond
      (and (vector? x) (-> x first map?) (-> x first (contains? ::evaled)))
      (do
        (let [{:keys [::line ::class ::file ::label ::meta ::form ::evaled]} (first x)]
          (println
            (str
              "\n"
              (when class (str ";; " file ":" line " " class "\n"))
              (->> x
                (mapcat (juxt
                          (juxt ::label ::form)
                          (juxt ::label ::evaled)))
                (reduce into ^{::ncols 2} [])
                ;(into {})
                string)))))


      (and (map? x) (contains? x ::evaled))
      (let [{:keys [::line ::class ::file ::label ::meta ::form ::evaled]} x]
        (println
          (str
            "\n"
            (when file (str ";; " file ":" line " " class " " label "\n"))
            ;(when label (str ";; " (pr-str label) "\n"))
            ;(when meta  (str ";; ^" (pr-str meta) "\n"))
            (when form  (str "#_ " (pr-str form) "\n"))
            (when meta (str (string (with-meta meta {::meta? true})) "\n"))
            (string evaled))))

      :else
      (println
        (str "\n" (string x))))))



(comment
  (add-tap #'tap-prn)
  (remove-tap #'tap-prn))

(def ^:dynamic *log-fn* tap-prn)

#?(:clj
   (defmacro locals [& banned]
     (let [ks (->> &env keys
                ;(remove #{'_})
                (remove #(re-matches #".+__\d+(?:__auto__)?" (name %)))
                (remove (set banned)))]
       (zipmap (map #(list 'quote %) ks) ks))))


(def -result-sym (gensym "result"))

(defn -hide [form]
  (cond
    ;; unwrapping (let [res orig] (spy res)):
    (and (-> form seq?)
      (-> form second vector?)
      (-> form second first (= -result-sym)))
    (-> form second second)

    ;; unwrapping ($ x):
    (and (-> form seq?)
      (-> form first (= '$)))
    (-> form second)

    ;; removing (-> x $ inc $):
    (-> form seq?)
    (remove #{'$} form)

    :else
    form))


(defn -spy [label meta form evaled]
  ;(->> (Thread/currentThread) (.getStackTrace) (map #(-where nil %)) (pp/spy))
  (*log-fn*
    #?(:cljs
       {::line   (or (:end-line meta) (:line meta))
        ::column (or (:end-column meta) (:column meta))
        ::file   (-> meta :file)
        ::label  label
        ::meta   (-> meta (dissoc :file :line :column :end-line :end-column) not-empty)
        ::form   form
        ::evaled evaled}

       :clj
       (let [el ^StackTraceElement
                (->> (Thread/currentThread) (.getStackTrace) (drop 4) first)]
         ;(println *log-fn*)
         {::line   (or (:line meta) (.getLineNumber el))
          ::column (-> meta :column)
          ::class  (.getClassName el)
          ::file   (.getFileName el)
          ::label  label
          ::meta   (-> meta (dissoc :file :line :column) not-empty)
          ::form   form
          ::evaled evaled})))
  evaled)



(def ^:dynamic *db* nil) ;;(atom [])
(defn -store! [m] (swap! *db* conj m))




(declare data-reader-multi)

(defn data-reader-basecase [label orig-form]
  (let [clean (walk/postwalk -hide orig-form)
        ometa (meta orig-form)]
    ;(pp/spy (locals))
    (with-meta
      `(let [~-result-sym ~orig-form]
         (-spy ~label '~ometa '~clean ~-result-sym))
      (meta orig-form))))

(defn spy-data-reader
  ([orig-form] (spy-data-reader nil orig-form))
  ([label orig-form]
   (if (seq? orig-form)
     (data-reader-multi label orig-form)
     (data-reader-basecase label orig-form))))


#?(:clj
   (defmacro data-reader-basecase-macro [label orig-form]
     (data-reader-basecase label orig-form)))


#?(:clj
   (defmacro $
     ([form]
      (spy-data-reader ''pp/$ form))
     ([label form]
      (spy-data-reader label form))
     ([a b c]
      (cond
        (= '- a) (spy-data-reader b c)  ;; (->> form ($ - label ,))
        (= '- b) (spy-data-reader c a)  ;; (->  form ($ , - label))
        :else    (throw (new Error "(f - label form) or (f form - label) expected"))))))


;; todo skip forms with :- meta
;; fixme: pass and use prefix: concat with op


(defn -data-reader-like-cond [form]
  (let [[op & pairs] form
        pairs+   (->> pairs
                   (partition-all 2)
                   (mapcat (fn [[pred then]]
                             [(data-reader-basecase ''<cond-pred> pred)
                              (data-reader-basecase ''<cond-then> then)])))]
    (with-meta
      `(~op ~@pairs+)
      (meta form))))


(defn -data-reader-like-if [form]
  (let [[op pred then else] form]
    (with-meta
      `(~op
        ~(data-reader-basecase ''<if-pred> pred)
        ~(data-reader-basecase ''<if-then> then)
        ~(data-reader-basecase ''<if-else> else))
      (meta form))))



(defn -data-reader-like-when [form]
  (let [[op pred & then] form]
    (with-meta
      `(~op
        ~(data-reader-basecase ''<when-pred> pred)
        ~(data-reader-basecase ''<when-then> (cons 'do then)))
      (meta form))))

(defn -data-reader-like-case [form]
  (let [[op expr & branches] form
        pairs    (->> branches
                   (partition 2)
                   (mapcat (fn [[pred then]]
                             [pred (data-reader-basecase ''<case-then> then)])))
        defaults (when (-> branches count odd?)
                   [(data-reader-basecase ''<case-default> (last branches))])]
    (with-meta
      `(~op
        ~(data-reader-basecase ''<case> expr)
        ~@pairs
        ~@defaults)
      (meta form))))



(defn -get-destructure-fn []
  (or (resolve 'cljs.core/destructure)
      (resolve 'clojure.core/destructure)))

;; pp_test.js:6
;; WARNING: Use of undeclared Var akovantsev.pp/Throwable

(defn -data-reader-like-let [form]
  (let [[op pairs & bodies] form
        js?    (resolve 'cljs.core/destructure)
        pairs* (->> pairs
                 ((-get-destructure-fn))
                 (partition-all 2)
                 (mapcat (fn [[sym expr]] [sym (data-reader-basecase `'~sym expr)]))
                 vec)]
    (with-meta
      `(let [stash# *log-fn*]
         (binding [*db*      (or *db* (atom []))
                   *log-fn*  -store!]
           ;(-data-reader-basecase-macro (symbol "<locals>") (locals))
           (try
             (~op
              ~pairs*
              (stash# @*db*)
              (reset! *db* [])
              (binding [*db*     nil
                        *log-fn* stash#]
                ~@bodies))
             (catch ~(if js? `js/Error `Throwable) e#
               (data-reader-basecase-macro (symbol "Throwable") e#)
               (stash# @*db*)
               (throw e#)))))
      (meta form))))


(defmulti  data-reader-multi      (fn [label seqform] (first seqform)))
(defmethod data-reader-multi :default [label form] (data-reader-basecase label form))
(defmethod data-reader-multi      'if [label form] (-data-reader-like-if form))
(defmethod data-reader-multi    'when [label form] (-data-reader-like-when form))
(defmethod data-reader-multi    'case [label form] (-data-reader-like-case form))
(defmethod data-reader-multi    'cond [label form] (-data-reader-like-cond form))
(defmethod data-reader-multi     'let [label form] (-data-reader-like-let form))




;; todo nospy #- to exclude huge vals from locals
;; todo make all form spies collect data like lets! until exception or return
;;fixme dedup 2 nested #$: #$ #$ -> #$



(comment
  ;; https://stackoverflow.com/a/77460102
  (defmethod clojure.core/print-method ::as-map [o, ^java.io.Writer w]
    (#'clojure.core/print-meta o w)
    (#'clojure.core/print-sequential "{" #'clojure.core/pr-on " " "}" o w))

  (pr-str ^{:type ::as-map} [1 2 3]))  ;; => "{1 2 3}"

