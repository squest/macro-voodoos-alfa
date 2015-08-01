(ns macros.core)

(defmacro for-var
  [reducer-fn lst]
  (let [[l ls res r l1] (repeatedly 5 gensym)]
    `(loop [[~l & ~ls] (rest ~lst) ~res (map vector (first ~lst))]
       (if ~l
         (recur ~ls (for [~r ~res ~l1 ~l] (conj ~r ~l1)))
         (map ~reducer-fn ~res)))))

(defn salmons
  [& lst]
  (for-var (partial into #{}) lst))

(defmacro for-var1
  [reducer-fn lst]
  (let [comps (vec (repeatedly 10 gensym))
        [ctr binds] (repeatedly 2 gensym)]
    `(let [~binds (vec (mapcat vector ~comps ~lst))]
       (for ~binds (apply ~reducer-fn (take ~ctr comps))))))
