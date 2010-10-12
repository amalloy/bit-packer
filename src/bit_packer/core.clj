(ns bit-packer.core)

(defn pack [num base]
  (map second
       (take-while (complement #(every? zero? %))
                   (rest (iterate (fn [[n]] ((juxt quot rem) n base)) [num 0])))))

(defn unpack [[& nums] base]
  (reduce +
          0
          (map *
               nums
               (iterate #(* base %) 1))))