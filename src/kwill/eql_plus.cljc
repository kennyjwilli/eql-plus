(ns kwill.eql-plus
  "EQL helper functions.

  Functions that take and return AST will be suffixed with a `*`. Functions
  without a `*` suffix tend to take and return EQL queries."
  (:require
    [edn-query-language.core :as eql]))

(defn merge-queries*
  "Merges `ast1` and `ast2` node maps recursively. Similar to eql/merge-asts, but
  allows overriding default merge behaviors (e.g., EQL parameters get merged).
  Takes an options map with the following keys:
    `:merge-params` - a function taking two node params maps and returns a single,
      merged param map. Defaults to `merge`."
  ([ast1 ast2] (merge-queries* ast1 ast2 nil))
  ([ast1 ast2 {:keys [merge-params]
               :or   {merge-params merge}}]
   (let [transform (fn transform [children1 children2]
                     (let [dispatch->child1 (into {} (map (juxt :dispatch-key identity)) children1)
                           dispatch->child2 (into {} (map (juxt :dispatch-key identity)) children2)]
                       (vals
                         (merge-with
                           (fn [node1 node2]
                             ;; TODO: Handle unions: https://edn-query-language.org/eql/1.0.0/specification.html#_unions
                             (let [params (merge-params (:params node1) (:params node2))]
                               (cond-> (if (or
                                             (= :join (:type node1))
                                             (= :join (:type node2)))
                                         (assoc node1
                                           :type :join
                                           :query (vec (concat (:query node1) (:query node2)))
                                           :children (transform (:children node1) (:children node2)))
                                         node1)
                                 params
                                 (assoc :params params))))
                           dispatch->child1 dispatch->child2))))]
     (assoc ast1
       :children (transform (:children ast1) (:children ast2))))))

(defn merge-queries
  "See [[merge-queries*]]."
  ([q1 q2] (merge-queries q1 q2 nil))
  ([q1 q2 opts]
   (eql/ast->query
     (merge-queries* (eql/query->ast q1) (eql/query->ast q2) opts))))

(defn merge-many-queries*
  "Merges many `asts` via [[merge-queries*]]."
  ([asts] (merge-many-queries* asts nil))
  ([asts opts]
   (reduce (fn [a1 a2] (merge-queries* a1 a2 opts)) (first asts) (rest asts))))

(defn merge-many-queries
  "See [[merge-many-queries*]]."
  ([queries] (merge-many-queries queries nil))
  ([queries opts]
   (eql/ast->query (merge-many-queries* (map eql/query->ast queries) opts))))

(comment
  (eql/query->ast [(list :a {:param1 true})
                   {(list :c {:param2 true}) [:c]}
                   {:b [:c {:d [:d]}]}])

  (merge-queries*
    (eql/query->ast [(list :a {:param1 true}) {:b [:c]}])
    (eql/query->ast [:b {:b [:d]}]))
  (eql/ast->query *1))

(defn difference-query*
  "Filters `source-ast` by removing any paths in `not-ast`. Similar to a set
  difference."
  [source-ast not-ast]
  (let [diff
        (fn diff
          [source-children not-children]
          (let [key->child (into {} (map (juxt :key identity)) source-children)
                diff-key->child
                (reduce
                  (fn [key->child {not-children :children
                                   :keys        [key]}]
                    (if (and (contains? key->child key) (not (seq not-children)))
                      (dissoc key->child key)
                      (if-let [children' (seq (diff (:children (get key->child key)) not-children))]
                        (update key->child key assoc :children children')
                        (dissoc key->child key))))
                  key->child
                  not-children)]
            (mapv val diff-key->child)))]
    (assoc source-ast :children (diff (:children source-ast) (:children not-ast)))))

(comment
  (difference-query*2
    (eql/query->ast [:a {:b [:c :d]}])
    (eql/query->ast [{:b [:c]}]))
  (eql/ast->query *1)

  (difference-query*2
    (eql/query->ast [:a {:b [:c :d]}])
    (eql/query->ast [:a {:b [:c :d]}]))
  (eql/ast->query *1))

(defn difference-query
  "See [[difference-query*]]."
  [source-query not-query]
  (let [source-ast (eql/query->ast source-query)
        not-ast (eql/query->ast not-query)]
    (eql/ast->query (difference-query* source-ast not-ast))))

(defn default-datomic-pull-select-params
  "Default `select-params` function used in [[ast->datomic-pull]] to select
  the Datomic keys from a node."
  [node]
  (let [{:keys [params]} node]
    (select-keys params [:default :limit :as :xform])))

(defn ast->datomic-pull
  ([ast] (ast->datomic-pull ast {}))
  ([ast {:keys [select-params]
         :or   {select-params default-datomic-pull-select-params}}]
   (let [transform (fn transform [children]
                     (into []
                       (mapcat
                         (fn [node]
                           (let [{:keys [type dispatch-key children]} node
                                 datomic-params (select-params node)]
                             (let [param-ized-k (if (seq datomic-params)
                                                  (list* dispatch-key (mapcat identity datomic-params))
                                                  dispatch-key)
                                   param-ized-k-simplified (if (and (sequential? param-ized-k)
                                                                 (= 1 (count param-ized-k)))
                                                             (first param-ized-k)
                                                             param-ized-k)]
                               [(if (= dispatch-key '*)
                                  '*
                                  (case type
                                    :prop param-ized-k-simplified
                                    :join {param-ized-k-simplified (transform children)}))]))))
                       children))]
     (transform (:children ast)))))

(defn query->datomic-pull
  "See [[ast->datomic-pull]]."
  ([query] (query->datomic-pull query nil))
  ([query opts]
   (ast->datomic-pull (eql/query->ast query) opts)))

(defn datomic-pull->ast
  ([pull-pattern] (datomic-pull->ast pull-pattern {}))
  ([pull-pattern _]
   (let [param->node
         (fn param->node [param]
           (cond
             (keyword? param)
             {:type         :prop
              :dispatch-key param
              :key          param}
             (symbol? param)
             {:dispatch-key param
              :key          param}
             (list? param)
             (let [[p & params] param]
               (assoc (param->node p)
                 :params (apply hash-map params)))))
         transform
         (fn transform [query]
           (mapv (fn [q]
                   (if (map? q)
                     (let [entry (first q)
                           k (key entry)
                           k-node (param->node k)
                           sub-q (val entry)]
                       (assoc k-node
                         :type :join
                         :query sub-q
                         :children (transform sub-q)))
                     (param->node q))) query))]
     {:type     :root
      :children (transform pull-pattern)})))

(comment
  (datomic-pull->ast [(list :a :limit 1)])
  (datomic-pull->ast '[*])
  (datomic-pull->ast [:a {:b [:a]}]))

(defn datomic-pull->query
  "See [[datomic-pull->ast]]."
  ([pull-pattern] (datomic-pull->query pull-pattern nil))
  ([pull-pattern opts]
   (eql/ast->query (datomic-pull->ast pull-pattern opts))))

(defn apply-transform*
  "Returns a map created by recursively following paths that exist in the EQL `ast`
  and `m`. Takes an option map of the following keys.
    `rf` - Reducing function called for every kv in `m`. The function is passed
      the accumulated map and a map with the following keys.
        `:key` - the key in `m`
        `:value` - the value in `m`
        `:node` - the node at the path for the kv pair.
    `node-key-fn` - Function passed the AST node returning a unique identifier
      for the node at that position in the tree. Defaults to :dispatch-key.
    `vf` - Function passed the `:key`, `:value`, and `:node` and should return
      the value to be passed to `rf`."
  [ast m {:keys [rf vf node-key-fn]
          :or   {rf          #(assoc %1 (:key %2) (:value %2))
                 node-key-fn :dispatch-key
                 vf          :value}
          :as   opts}]
  (let [dispatch->node (into {} (map (juxt node-key-fn identity)) (:children ast))]
    (reduce-kv
      (fn [new-map k v]
        (let [node-at-k (get dispatch->node k)
              children? (:children node-at-k)
              v' (vf {:key k :value v :node node-at-k})
              v'' (cond
                    (and children? (map? v'))
                    (apply-transform* node-at-k v' opts)
                    (and children? (sequential? v'))
                    (mapv #(apply-transform* node-at-k % opts) v')
                    :else v')]
          (rf new-map {:key k :value v'' :node node-at-k})))
      {} m)))

(defn apply-transform
  "See [[apply-transform*]]."
  [query m opts]
  (apply-transform* (eql/query->ast query) m opts))

(defn transform-rename-rf
  "rf function for use with [[apply-transform*]] that renames map keys using
  data on the AST node."
  [{:keys [get-new-key]}]
  (fn [new-map {:keys [value node]}]
    (let [new-k (get-new-key node)]
      (if (and node new-k)
        (assoc new-map new-k value)
        new-map))))

(comment
  (apply-transform
    ['(:a {:rename :A})
     {'(:b {:rename :B}) [:c]}
     {'(:c {:rename :C}) ['(:a {:rename :A})]}]
    {:a 1
     :b {:c 2}
     :c [{:a 1}]}
    {:rf (transform-rename-rf {:get-new-key #(get-in % [:params :rename])})}))

(defn map-select*
  "Filters `m` by the `ast`. Similar to `select-keys` or Datomic pull. "
  [ast m]
  (apply-transform* ast m
    {:rf (fn [new-map {:keys [key value node]}]
           (if node
             (assoc new-map key value)
             new-map))}))

(defn map-select
  "See [[map-select*]]."
  [query m]
  (map-select* (eql/query->ast query) m))

(comment
  (map-select
    [:a :b :f]
    {:a 1 :b 2 :c {:d 3 :e 4} :f {:g 5}})
  )

(defn top-level-keys*
  "Returns the top-level (children at root level) `dispatch-key` in `ast`."
  [ast]
  (->> ast :children (mapv :dispatch-key)))

(defn top-level-keys
  "See [[top-level-keys*]]."
  [query]
  (->> query eql/query->ast top-level-keys*))

(defn split*
  "Returns a tuple of ASTs where the first element does not contain any idents
  specified in `range-ast`, and the second element contains at most all the
  elements specified in `range-ast`."
  [input-ast range-ast]
  (let [action-item-resource-ast (eql/focus-subquery* input-ast range-ast)
        resource-eql-ast' (difference-query* input-ast action-item-resource-ast)]
    [resource-eql-ast' action-item-resource-ast]))

(defn split
  "See [[split*]]."
  [input-query range-query]
  (let [input-ast (eql/query->ast input-query)
        range-ast (eql/query->ast range-query)
        [x y] (split* input-ast range-ast)]
    [(eql/ast->query x) (eql/ast->query y)]))
