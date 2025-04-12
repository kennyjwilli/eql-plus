# eql-plus

A collection of helper functions for common EQL tasks — an extension to the EQL standard library.

[![Clojars Project](https://img.shields.io/clojars/v/dev.kwill/eql-plus.svg)](https://clojars.org/dev.kwill/eql-plus)

## Installation

```clojure
dev.kwill/eql-plus {:mvn/version "1.0.10"}
```

## Overview

eql-plus provides utility functions for working with [EQL (EDN Query Language)](https://edn-query-language.org/), making common operations on EQL queries and data more convenient. The library is designed to complement the standard EQL library.

Functions that take and return AST are suffixed with a `*`. Functions without a `*` suffix typically take and return EQL queries.

## Functions

### Query Manipulation

#### `merge-queries` / `merge-queries*`

Merges two EQL queries or ASTs recursively, allowing customization of the merge behavior.

```clojure
;; Merge two queries
(merge-queries 
  [:a {:b [:c]}] 
  [:d {:b [:e]}])
;; => [:a :d {:b [:c :e]}]

;; With custom merge params behavior
(merge-queries 
  [(list :a {:param1 true})] 
  [(list :a {:param2 false})] 
  {:merge-params (fn [p1 p2] (merge p1 p2))})
;; => [(list :a {:param1 true, :param2 false})]
```

#### `merge-many-queries` / `merge-many-queries*`

Merges multiple EQL queries or ASTs.

```clojure
(merge-many-queries 
  [[:a] 
   [:b] 
   [{:c [:d]}]])
;; => [:a :b {:c [:d]}]
```

#### `difference-query` / `difference-query*`

Filters a source query by removing any paths in another query (similar to set difference).

```clojure
(difference-query 
  [:a {:b [:c :d]}] 
  [{:b [:c]}])
;; => [:a {:b [:d]}]
```

#### `top-level-keys` / `top-level-keys*`

Returns the top-level dispatch keys in a query or AST.

```clojure
(top-level-keys [:a {:b [:c]}])
;; => [:a :b]
```

#### `split` / `split*`

Splits a query into two parts: one without the specified paths, and one with only those paths.

```clojure
(split [:a {:b [:c]}] [:b])
;; => [[:a] [{:b [:c]}]]
```

### Datomic Integration

#### `query->datomic-pull` / `ast->datomic-pull`

Converts an EQL query or AST to a Datomic pull pattern.

```clojure
(query->datomic-pull [:a (list :b {:limit 10}) {:c [:d]}])
;; => [:a (list :b :limit 10) {:c [:d]}]
```

#### `datomic-pull->query` / `datomic-pull->ast`

Converts a Datomic pull pattern to an EQL query or AST.

```clojure
(datomic-pull->query [:a (list :b :limit 10) {:c [:d]}])
;; => [:a (list :b {:limit 10}) {:c [:d]}]
```

### Data Transformation

#### `apply-transform` / `apply-transform*`

Returns a map created by recursively following paths that exist in both an EQL query/AST and a map, allowing for custom transformations.

```clojure
(apply-transform 
  [:a {:b [:c]}] 
  {:a 1 :b {:c 2 :d 3}} 
  {})
;; => {:a 1 :b {:c 2}}
```

#### `transform-rename-rf`

A reducing function for use with `apply-transform*` that renames map keys using data on the AST node.

```clojure
(apply-transform
  ['(:a {:as :A}) {:b ['(:c {:as :C})]}]
  {:a 1 :b {:c 2}}
  {:rf (transform-rename-rf {:get-new-key #(get-in % [:params :as])})})
;; => {:A 1 :b {:C 2}}
```

#### `map-select` / `map-select*`

Filters a map by an EQL query or AST (similar to `select-keys` or Datomic pull).

```clojure
(map-select 
  [:a {:c [:d]}] 
  {:a 1 :b 2 :c {:d 3 :e 4}})
;; => {:a 1 :c {:d 3}}
```

#### Wildcard Support

The function `node-at-key-with-wildcard` can be used with `map-select` to support wildcard selection.

```clojure
(map-select 
  '[* {:c [:d]}]
  {:a 1 :b 2 :c {:d 3 :e 4}}
  {:node-at-key-fn node-at-key-with-wildcard})
;; => {:a 1 :b 2 :c {:d 3}}
```

## License

Copyright © Kenny Williams

Distributed under the MIT License.