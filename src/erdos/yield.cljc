(ns erdos.yield)

(set! *warn-on-reflection* true)

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

;; for each partition: pred is true for the last item
(defn- part-last [pred coll]
  (lazy-seq
   (when-let [s (seq coll)]
     (let [run (take-while (complement pred) s)
           run (take (inc (count run)) s)]
       (cons run (part-last pred (drop (count run) s)))))))

(defn with-yield-meta [e] (with-meta e {:yield true}))

(defn yield? [e]
  (assert (seq? e))
  (when (= (first e) 'yield)
    (assert (= 2 (count e)))
    (second e)))

; (declare rewrite)

(defmulti rewrite (fn [e] (when (seq? e) (first e))))

(defmethod rewrite :default [e] e)

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
    (cond-> `(if ~cond ~then ~else)
      (or (rewritten? then) (rewritten? else)) (with-yield-meta))))

(defmethod rewrite 'let* [[_ bindings & bodies]]
  (let [body (rewrite (cons 'do bodies))]
    (cond-> `(let* ~bindings ~body)
      (rewritten? body) (with-yield-meta))))

(defmethod rewrite 'letfn* [[_ bindings & bodies]]
  (let [body (rewrite (cons 'do bodies))]
    (cond-> `(letfn* ~bindings ~body)
      (rewritten? body) (with-yield-meta))))

(defmethod rewrite 'yield [e]
  (assert (= 2 (count e)) "Call to (yield ..) must have 1 parameter!")
  (with-yield-meta (list 'list (second e))))

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
