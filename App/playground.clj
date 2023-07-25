(def a (atom 1))
(def b (atom 2))

(dosync
 (swap! a inc)
 (swap! b inc))

(dosync
 (swap! a inc)
 (swap! b inc))

(print (str "a is " @a " b is " @b))



(loop [oldvalue @counter]
  (let [newvalue (inc oldvalue)]
    (if (is_okay newvalue)
      (if (compare-and-set! counter oldvalue newvalue)
        [oldvalue, newvalue] ;with this we can see whether we have actually update
        ))))

(defn update-atom-if [Atom, Condition])