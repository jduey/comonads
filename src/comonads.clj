;   Copyright (c) Jim Duey. All rights reserved.
;   The use and distribution terms for this software are covered by the
;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;   which can be found in the file epl-v10.html at the root of this distribution.
;   By using this software in any fashion, you are agreeing to be bound by
;   the terms of this license.
;   You must not remove this notice, or any other, from this software.

(ns comonads
  (:refer-clojure :exclude [extend])
  )

(defprotocol Comonad
  (extract [wv])
  (extend [wv f]))

(deftype covect [index ^clojure.lang.PersistentVector vec-value]
  clojure.lang.IDeref
  (deref [_]
    [index vec-value])

  Comonad
  (extract [_]
    (get vec-value index))
  (extend [_ f]
    (covect. index (into [] (map #(f (covect. % vec-value))
                                 (range (count vec-value)))))))

(defn left [wv]
  (let [[i v] @wv]
    (if (> i 0)
      (covect. (dec i) v)
      wv)))

(defn right [wv]
  (let [[i v] @wv]
    (if (< i (dec (count v)))
      (covect. (inc i) v)
      wv)))

;; set
(deftype coset [value ^clojure.lang.PersistentHashSet set-value]
  clojure.lang.IDeref
  (deref [_]
    [value set-value])

  Comonad
  (extract [_]
    value)
  (extend [_ f]
    (coset. value (set (map #(f (coset. % set-value)) set-value)))))

(defprotocol Matrix
  (new-matrix [mx])
  (set-cell [mx x y v]))

(defmacro extend-matrix-type [prim-type set-fn]
  `(extend-type (class (make-array ~prim-type 1 1))
    Matrix
    (new-matrix [mx#]
      (make-array ~prim-type (alength mx#) (alength (aget mx# 0))))
    (set-cell [mx# x# y# v#]
      (~set-fn mx# x# y# v#))))

(extend-matrix-type Boolean/TYPE aset-boolean)
(extend-matrix-type Byte/TYPE aset-byte)
(extend-matrix-type Short/TYPE aset-short)
(extend-matrix-type Character/TYPE aset-char)
(extend-matrix-type Integer/TYPE aset-int)
(extend-matrix-type Long/TYPE aset-long)
(extend-matrix-type Float/TYPE aset-float)
(extend-matrix-type Double/TYPE aset-double)

(deftype comatrix [x y matrix]
  clojure.lang.IDeref
  (deref [_]
    [x y matrix])

  Comonad
  (extract [_]
    (aget matrix x y))
  (extend [_ f]
    (co-bool. x y
              (let [new-m (new-matrix matrix)]
                (doseq [x (range (alength matrix))
                        y (range (alength (aget matrix 0)))]
                  (set-cell new-m x y (f (co-bool. x y matrix))))
                new-m))))

;; return the coordinates of the neighbors of a cell in a matrix
(defn neighbors [x y a & {:keys [wrap]
                          :or {wrap true}}]
  (let [x-dim (alength a)
        y-dim (alength (aget a 0))]
    (for [dx [-1 0 1]
          dy [-1 0 1]
          :when (not= 0 dx dy)
          :when (or wrap
                    (and (<= x x-dim)
                         (<= y y-dim)))]
      (let [x (+ x dx)
            y (+ y dy)
            x (cond
               (< x 0) (+ x x-dim)
               (>= x x-dim) (- x x-dim)
               :else x)
            y (cond
               (< y 0) (+ y y-dim)
               (>= y y-dim) (- y y-dim)
               :else y)]
        [x y]))))

(defn neighbor-vals [x y a & {:keys [wrap]
                              :or {wrap true}}]
  (map #(apply aget a %) (neighbors x y a wrap)))

