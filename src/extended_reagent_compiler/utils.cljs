(ns extended-reagent-compiler.utils)

(defn map-array [f coll]
  (let [js-array ^js (array)]
    (doseq [e coll]
      (.push js-array (f e)))
    js-array))
