(ns erdos.yield)

(defn- quoted? [x] (boolean (and (seq? x) ('#{quote clojure.core/quote} (first x)))))

(defn- walk [inner outer form]
  (cond
    (list? form) (outer (with-meta (apply list (map inner form)) (meta form)))
    (instance? clojure.lang.IMapEntry form) (outer (vec (map inner form)))
    (seq? form) (outer (doall (with-meta (map inner form) (meta form))))

    (set? form) (outer (with-meta (into (empty form) (map inner form)) (meta form)))
    (map? form) (outer (with-meta (into (empty form) (map inner form)) (meta form)))
    (vector? form) (outer (with-meta (mapv inner form) (meta form)))

    (instance? clojure.lang.IRecord form)
    (outer (reduce (fn [r x] (conj r (inner x))) form form))
    (coll? form) (outer (into (empty form) (map inner form)))
    :else (outer form)))

(defn- prewalk [f form] (walk (partial prewalk f) identity (f form)))

(defn- postwalk-code [f expr]
  (if (quoted? expr)
    expr
    (walk (partial postwalk-code f) f expr)))

(defn- macroexpand-code [form]
  (prewalk
   (fn [x]
     (if (quoted? x)
       x
       (let [e (macroexpand x)]
         (if (instance? clojure.lang.IObj e)
           (with-meta e (meta x))
           e))))
   form))

(defn with-yield-meta [e] (with-meta e {:yield true}))

(defn yield? [e]
  (assert (seq? e))
  (when (= (first e) 'yield)
    (assert (= 2 (count e)))
    (second e)))

(defmulti rewrite (fn [e] (when (seq? e) (first e))))

(defmethod rewrite :default [e] e)

;; retunrs a (lazy seq) pair of (concat-vals return-value)
(defmulti rewrite-val (fn [e] (when (seq? e) (first e))))

(defmethod rewrite-val :default [e] `[nil ~e])

(defn rewritten? [e] (-> e meta :yield boolean))

(defmethod rewrite 'do [[do & bodies]]
  (assert (= 'do do))
  (let [rs (map rewrite bodies)]
    (if-not (some rewritten? rs)
      `(do ~@bodies nil)
      (with-yield-meta
        (if (= 1 (count rs))
          (first rs)
          `(concat ~@(doall (for [r rs]
                              (if (rewritten? r)
                                (list 'lazy-seq r)
                                (list 'lazy-seq (list do r nil)))))))))))

(defmethod rewrite-val 'do [[do & bodies]]
  (assert (= 'do do))
  (let [rr (map rewrite (butlast bodies))
        r0 (rewrite-val (last bodies))]
    (if-not (or (rewritten? r0) (some rewritten? rr))
          `(do ~@bodies nil)
      (with-yield-meta
        (case (count rr)
          0 r0
          1 `(let [xs#     ~@rr
                   [a# b#] ~r0]
               [[xs# a#] b#])
          `(let [xs#     (doall (concat ~@rr))
                 [a# b#] ~r0]
             [(concat xs [a#]) b#]))))))

(def ^:dynamic *loop-id*)

(defmethod rewrite 'loop* [[loop exprs & bodies]]
  (assert (= 'loop* loop))
  (assert (vector? exprs))
  (binding [*loop-id* (gensym "loop")]
    (let [body (rewrite (cons 'do bodies))]
      (if-not (rewritten? body)
        (list 'do (list* 'loop* exprs bodies) nil)
        (with-yield-meta
          (list*
           (list 'fn *loop-id* (mapv first (partition 2 exprs)) body)
           (map second (partition 2 exprs))))))))

(defmethod rewrite 'recur [[_ & args]]
  (with-yield-meta `(lazy-seq (~*loop-id* ~@args))))

(defmethod rewrite 'if [[_ cond then else]]
  (let [then (rewrite then)
        else (rewrite else)]
    (if-not (or (rewritten? then) (rewritten? else))
      (list 'if cond then else)
      (with-yield-meta
        (list 'if
              cond
              (if (rewritten? then)
                then
                (list 'do then nil))
              (if (rewritten? else)
                else
                (list 'do else nil)))))))

;;
(defmethod rewrite-val 'if [[_ cond then else]]
  (let [rewritten-cond (rewrite-val cond)
        then           (rewrite-val then)
        else           (rewrite-val else)]
    (if-not (some rewritten? [rewritten-cond then else]))
    (if (rewritten? cond)
      `(let [[cc# r#] ~rewritten-cond]
          (if r#
              (let [[tc# tr#] ~then]
                [(concat cc# tc#) tr#])
              (let [[ec# er#] ~else]
                [(concat cc# ec#) er#])))
      `(if ~cond
          (let [[tc# tr#] ~then]
            [(concat cc# tc#) tr#])
          (let [[ec# er#] ~else]
            [(concat cc# ec#) er#])))

    (if-not (or (rewritten? then) (rewritten? else))
      (list 'if cond then else)
      (with-yield-meta
        (list 'if
              cond
              (if (rewritten? then)
                then
                (list 'do then nil))
              (if (rewritten? else)
                else
                (list 'do else nil)))))))

(defmethod rewrite 'let* [[_ bindings & bodies]]
  (let [body (rewrite (cons 'do bodies))]
    (cond-> `(let* ~bindings ~body)
      (rewritten? body) (with-yield-meta))))

(defmethod rewrite 'letfn* [[_ bindings & bodies]]
  (let [body (rewrite (cons 'do bodies))]
    (cond-> `(letfn* ~bindings ~body)
      (rewritten? body) (with-yield-meta))))

(defmethod rewrite 'case* [[_ e shift mask default m & args]]
  (let [default (rewrite default)

        {:keys [any-clause-rewritten]
         m :result}
        (reduce (fn [acc [minhash [c then]]]
                  (let [then (rewrite then)]
                    (if (rewritten? then)
                      (-> acc
                          (assoc-in [:result minhash] [c then])
                          (assoc :any-clause-rewritten true))
                      (assoc-in acc
                                [:result minhash]
                                [c `(do ~then nil)]))))
                {:any-clause-rewritten false
                 :result {}}
                m)]
    (cond-> `(case* ~e
                    ~shift
                    ~mask
                    ~(if (rewritten? default)
                       default
                       `(do ~default nil))
                    ~m
                    ~@args)
      (or (rewritten? default)
          any-clause-rewritten) (with-yield-meta))))

(defmethod rewrite 'yield [e]
  (assert (= 2 (count e)) "Call to (yield ..) must have 1 parameter!")
  (with-yield-meta (list 'list (second e))))

(defmethod rewrite-val 'yield [e]
  (assert (= 2 (count e)) "Call to (yield ..) must have 1 parameter!")
  (with-yield-meta
    `(let* [l# ~(second e)]
       [[l#] l#])))

(defmethod rewrite 'yield-all [e]
  (assert (= 2 (count e)) "Call to (yield-all ..) must have 1 parameter!")
  (with-yield-meta (lazy-seq (second e))))

(defmacro gen-seq [& bodies]
  (let [result (rewrite (macroexpand-code (list* 'do bodies)))]
    (if-not (rewritten? result)
      (throw (ex-info "Call to (seq-expr ...) should have at least one yield or yield-all call in it!" {:e result}))
      `(lazy-seq ~result))))

(defn yield [e]
  (throw (ex-info "Can not use yield outside of gen-seq form!" {:expr e})))

(defn yield-all [e]
  (throw (ex-info "Can not use yield-all outside of gen-seq form!" {:expr e})))
