(ns kwil.eql-plus-test
  (:require
    [clojure.test :refer :all]
    [edn-query-language.core :as eql]
    [kwill.eql-plus :as eql+]))

(deftest merge-queries*-unit-test
  (let [a-as-b+c {:type         :join
                  :dispatch-key :a
                  :key          :a
                  :params       {:foo :bar}
                  :children     [{:type         :prop
                                  :dispatch-key :c
                                  :key          :c}]}
        d+e {:type     :root
             :children [{:type         :join
                         :dispatch-key :d
                         :key          :d
                         :children     [{:type         :prop
                                         :dispatch-key :e
                                         :key          :e}]}]}
        merge-result
        {:children     [{:dispatch-key :c
                         :key          :c
                         :type         :prop}
                        {:children     [{:dispatch-key :e
                                         :key          :e
                                         :type         :prop}]
                         :dispatch-key :d
                         :key          :d
                         :type         :join}]
         :dispatch-key :a
         :key          :a
         :params       {:foo :bar}
         :type         :join}]
    (testing "Merge behavior is identical between eql and eql+."
      (is (= merge-result
            (eql/merge-asts a-as-b+c d+e))
        "stdlib: `eql/merge-asts`")
      (is (= merge-result
            (eql+/merge-queries* a-as-b+c d+e))
        "eql+/merge-queries*"))
    (is (= {:type     :root
            :children [{:children     [{:dispatch-key :c
                                        :key          :c
                                        :type         :prop}]
                        :dispatch-key :a
                        :key          :a
                        :params       {:bar :baz
                                       :foo :bar}
                        :query        []
                        :type         :join}]}
          (eql+/merge-queries*
            {:type :root :children [a-as-b+c]}
            {:type :root :children [(assoc-in a-as-b+c [:params :bar] :baz)]}))
      "Params are merged")))

(deftest merge-queries-test
  (is (= [] (eql+/merge-queries [] []))
    "empty queries")
  (is (= [:a :b]
        (eql+/merge-queries [:a] [:b]))
    "top-level queries")
  (is (= [(list :a {:param1 false})]
        (eql+/merge-queries
          [(list :a {:param1 true})]
          [(list :a {:param1 false})]))
    "later parameters override")
  (is (= [{:a [:b (list :c {:param1 true})]}]
        (eql+/merge-queries
          [{:a [:b]}]
          [{:a [(list :c {:param1 true})]}]))
    "join queries"))

(deftest difference-query-test
  (is (= []
        (eql+/difference-query [] []))
    "base empty case")
  (is (= [:a {:b [:d]}]
        (eql+/difference-query
          [:a {:b [:c :d]}]
          [{:b [:c]}]))
    "remove key from join")
  (is (= [:a]
        (eql+/difference-query
          [:a {:b [:c]}]
          [{:b [:c]}]))
    "join -> prop node")
  (is (= []
        (eql+/difference-query
          [:a :b]
          [:a :b]))
    "remove all attrs")
  (is (= []
        (eql+/difference-query
          [:a {:b [:c]}]
          [:a {:b [:c]}]))
    "remove all join"))

(deftest ast->datomic-pull-unit-test
  (is (= [:a] (eql+/query->datomic-pull [:a]))
    "simple query")
  (is (= [(list :a :limit 1000)]
        (eql+/query->datomic-pull [(list :a {:limit 1000})]))
    "params into list")
  (is (= [{:a [:b]}]
        (eql+/query->datomic-pull [{:a [:b]}]))
    "join")
  (is (= [{(list :a :as :foo) [:b]}]
        (eql+/query->datomic-pull [{(list :a {:as :foo}) [:b]}]))
    "join")
  (is (= [{:a ['*]}]
        (eql+/query->datomic-pull [{:a ['*]}]))
    "* in query"))

(deftest datomic-pull->ast-unit-test
  (is (= {:children [{:dispatch-key :a
                      :key          :a
                      :type         :prop}]
          :type     :root}
        (eql+/datomic-pull->ast [:a]))
    "simple query")
  (is (= {:children [{:dispatch-key :a
                      :key          :a
                      :type         :prop
                      :params       {:limit 10}}]
          :type     :root}
        (eql+/datomic-pull->ast [(list :a :limit 10)]))
    "simple query containing pull params")
  (is (= {:children [{:children     [{:dispatch-key :b
                                      :key          :b
                                      :type         :prop}
                                     {:dispatch-key :c
                                      :key          :c
                                      :params       {:limit 1}
                                      :type         :prop}]
                      :dispatch-key :a
                      :key          :a
                      :query        [:b (list :c :limit 1)]
                      :type         :join}]
          :type     :root}
        (eql+/datomic-pull->ast [{:a [:b (list :c :limit 1)]}]))
    "join at root child with params")
  (is (= '{:children [{:dispatch-key *
                       :key          *}]
           :type     :root}
        (eql+/datomic-pull->ast ['*]))
    "wildcard")
  (is (= '{:children [{:dispatch-key :foo
                       :key          :foo
                       :query        *
                       :type         :join}]
           :type     :root}
        (eql+/datomic-pull->ast '[{:foo *}]))
    "nested wildcard")
  (is (= '{:type     :root
           :children [{:type :join :dispatch-key :foo :key :foo :query ...}]}
        (eql+/datomic-pull->ast '[{:foo ...}]))
    "unlimited recursion")
  (is (= '{:type     :root
           :children [{:type :join :dispatch-key :foo :key :foo :query 2}]}
        (eql+/datomic-pull->ast '[{:foo 2}]))
    "limited recursion"))

(comment
  (apply-transform*
    (eql/query->ast
      [{(list :terms {::transform-fn (comp vec vals)})
        [(list :priceDimensions {::transform-fn (comp vec vals)})]}])
    {:terms {"ad1.abc" {:priceDimensions {"a" {:id "a"}
                                          "b" {:id "b"}}}}}
    {:rf (fn [acc {:keys [key value node]}]
           (assoc acc key
             (if-let [f (get-in node [:params ::transform-fn])]
               (f value)
               value)))})
  )

(deftest apply-transform-unit-test
  (is (= {:a 1 :b {:c 2} :d 3}
        (eql+/apply-transform
          [:a :b :c]
          {:a 1 :b {:c 2} :d 3}
          {})))
  (is (= {:A 1
          :B {}
          :C [{:A 1}]}
        (eql+/apply-transform
          ['(:a {:as :A})
           {'(:b {:as :B}) [:c]}
           {'(:c {:as :C}) ['(:a {:as :A})]}
           :d]
          {:a 1
           :b {:c 2}
           :c [{:a 1}]
           :d 3}
          {:rf (eql+/transform-rename-rf {:get-new-key #(get-in % [:params :as])})})))
  (is (= {:id->m [{:lookup [{:id "a"}
                            {:id "b"}]}]}
        (eql+/apply-transform
          [{(list :id->m {::vf (comp vec vals)}) [(list :lookup {::vf (comp vec vals)})]}]
          {:id->m {"id1" {:lookup {"a" {:id "a"}
                                   "b" {:id "b"}}}}}
          {:rf (fn [acc {:keys [key value]}]
                 (assoc acc key value))
           :vf (fn [{:keys [node value]}]
                 (if-let [vf (-> node :params ::vf)]
                   (vf value)
                   value))}))))

(deftest map-select-unit-test
  (is (= {}
        (eql+/map-select [] {:a 1})))
  (is (= {:a 1}
        (eql+/map-select [:a] {:a 1})))
  (is (= {:a 1}
        (eql+/map-select [:a] {:a 1 :b 2})))
  (is (= {:a 1
          :c {:d 3}
          :f {:g 4}}
        (eql+/map-select [:a {:c [:d]} :f]
          {:a 1 :b 2 :c {:d 3} :f {:g 4}})))
  (is (= {}
        (eql+/map-select '[*]
          {:a 1 :c {:d 3}}))
    "Default wildcard selects nothing")
  (is (= {:a 1
          :c {:d 3}}
        (eql+/map-select '[* {:c [:d]}]
          {:a 1 :c {:d 3 :e 2}}
          {:node-at-key-fn eql+/node-at-key-with-wildcard}))
    "Adding node-at-key-with-wildcard supports wildcard."))

(deftest top-level-keys-unit-test
  (is (= [] (eql+/top-level-keys [])))
  (is (= [:a] (eql+/top-level-keys [:a])))
  (is (= [:a :b] (eql+/top-level-keys [:a {:b [:c]}]))))

(deftest split-unit-test
  (is (= [[] []] (eql+/split [] [])))
  (is (= [[:a] []]
        (eql+/split [:a] [])))
  (is (= [[] [:a]]
        (eql+/split [:a] [:a])))
  (is (= [[:a] [:b]]
        (eql+/split [:a :b] [:b])))
  (is (= [[:a]
          [{:b [:c]}]]
        (eql+/split [:a {:b [:c]}] [:b]))
    "union in input")
  (is (= [[:a]
          [{:b []}]]
        (eql+/split [:a {:b [:c]}] [{:b [:d]}]))
    "union in input and range with no shared keys"))

