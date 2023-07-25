(def a (atom 1))
(def b (atom 2))

(dosync
 (swap! a inc)
 (swap! b inc))

(dosync
 (swap! a inc)
 (swap! b inc))

(print (str "a is " @a " b is " @b))



(let [{name :name
       location :location
       description :description} client]
  (println name location "-" description))