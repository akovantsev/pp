(ns com.akovantsev.pp
  (:require
   [clojure.string :as str]
   [com.akovantsev.blet.core :refer [blet]]))


(declare -pps)

(def ^:dynamic *colored* true)
(def ^:dynamic *limit-seq-elements* 20)
(def ^:dynamic *max-line-chars* 50)
(def ^:dynamic *max-key-chars* 30)

(def LEFT (symbol "➡︎"))
(def DOWN (symbol "⬇︎"))


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

(def COLORS-RE #"\033\[(\d+;)*\d+m")

(defn -str-count [x]
  (if *colored*
    (-> x (str/replace COLORS-RE "") count)
    (-> x count)))

(defn -pscalar [x]
  (blet [c (cond
             (nil? x)     ::MAGENTA
             (string? x)  ::GREY
             (symbol? x)  ::RED
             (number? x)  ::BLUE
             (boolean? x) ::MAGENTA
             (uuid? x)    ::CYAN
             (keyword? x) ::GREEN)]
    (if (and *colored* c)
      [(str (COLORS c) (pr-str x) (COLORS ::RESET))]
      [(pr-str x)])))


(defn -pack [vecs]
  (assert (vector? vecs))
  (assert (every? vector? vecs))
  (loop [todo  vecs
         line  nil
         lines []]
    (blet [v         (first todo)
           many?     (-> v count (> 1))
           x         (first v)
           line+     (str line " " x)
           lines+    (conj lines line)
           todo-     (next todo)
           overflow? (-> line (str " " x) -str-count (>= *max-line-chars*))]
      (cond
        (nil? todo) (cond-> lines line (conj line))   ;; conj wip
        many?       (recur todo- nil   (cond-> lines, line (conj line), true (into v)))
        (nil? line) (recur todo- x     lines)         ;; always init first lines
        overflow?   (recur todo- x     lines+)
        :else       (recur todo- line+ lines)))))


(defn -wrap [pref pad suf lines]
  (case (count lines)
    0 [(str pref suf)]
    1 [(str pref (first lines) suf)]
    (let [fst    (first lines)
          lst    (peek lines)
          fst+   (str pref fst)
          lst+   (str lst suf)
          lines* (-> lines pop (conj lst+) rest)]
      (->> lines* (map #(str pad %)) (cons fst+) vec))))



(defn -height-short-first-az [[k1 v1] [k2 v2]]
  (let [s1 (peek k1), s2 (peek k2)]
    (compare [(max 10 (count v1)) (count s1) s1] [(max 10 (count v2)) (count s2) s2])))


(defn -pmap [x]
  (let [kss    (binding [*max-line-chars* *max-key-chars*]
                 (->> x keys (map -pps)))
        vss    (->> x vals (map -pps))
        kvss   (->> (interleave kss vss) (partition 2) (sort -height-short-first-az))
        maxk   (->> kss (map last) (map -str-count) (reduce max 0))
        padlen (inc maxk)
        pad    (str/join (repeat padlen " "))]
    (loop [todo  kvss
           done  []]
      (blet [[klines
              vlines](first todo)
             todo-   (next todo)
             klines- (pop klines)
             lastkl  (peek klines)
             diff    (- padlen (-str-count lastkl))
             prefix  (str lastkl (str/join (repeat diff " ")))
             vlines+ (-wrap prefix pad "" vlines)]
        (cond
          (empty? todo) (-wrap "{" " " "}" done)
          ;; todo: limit last key line len
          :else         (recur todo- (-> done (into klines-) (into vlines+))))))))


(defn --pseq [x]
  (let [x- (if (some-> *limit-seq-elements* inc (bounded-count x) (> *limit-seq-elements*))
             (concat (take *limit-seq-elements* x) [LEFT])
             x)]
    (-pack (mapv -pps x-))))


(defn -pset [x] (-wrap "#{" "  " "}" (--pseq x)))
(defn -pvec [x] (-wrap "["  " "  "]" (--pseq x)))
(defn -pseq [x] (-wrap "("  " "  ")" (--pseq x)))
(defn -pps  [x]
  (let [f (cond
            (map? x)    -pmap
            (vector? x) -pvec
            (set? x)    -pset
            (list? x)   -pseq
            (seq? x)    -pseq
            :else       -pscalar)]
    (f x)))



(defn string [x] (str/join "\n" (-pps x)))
(defn spy [x] (println (string x)) x)
