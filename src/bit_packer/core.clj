(ns bit-packer.core)

(defn conjunct [& preds]
  (fn [& args]
    (every? #(apply % args) preds)))

(def pos-int? (conjunct integer? pos?))

(defn pos-ints? [[& coll]]
  (and (coll? coll)
       (every? pos-int? coll)))

(defn pack
  [num base]
  {:pre [(> base 1)
         (integer? base)]
   :post [(pos-ints? %)]}
  (map second
       (take-while #(not= [0 0] %)
                   (rest (iterate (fn [[n]]
                                    ((juxt quot rem) n base))
                                  [num 0])))))

(defn unpack
  [[& nums] base]
  {:pre [(pos-ints? nums)]
   :post [(pos-int? %)]}
  (reduce + (map *
                 nums
                 (iterate #(* base %) 1))))