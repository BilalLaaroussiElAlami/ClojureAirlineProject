(def flights
  {["BRU", "LUX"] [(atom 'flightdata1) (atom 'flightdata2)]
   ["PAR LAX"]    [(atom 'flightdata3)  (atom 'flightdata4)]})

;(println (str "flights " (flights ["BRU", "LUX"])))

(defn print-map-elements [data-map]
  (doseq [[_ value] data-map]
    (doseq [v value] (println v))))


;; Example usage:
(def my-map
  {:fruit ["apple" "banana" "orange"]
   :vegetable ["carrot" "spinach" "broccoli"]
   :animal ["dog" "cat" "elephant"]})
(print-map-elements my-map)
(when true (print "ok"))


;example usage: (split '(1 2 3 4 5 6 7 8) 3) returns '((1 2 3)(4 5 6)(7 8))
(defn split [lst size]
  (loop [ls lst
         res '()]
    (if (empty? ls)
      (reverse res)
      (recur (drop size ls) (cons (take size ls) res)))))


(def ^:dynamic x 3)

(defn main []
  (binding [x x]  ;; defaulted to the root binding value
    (set! x 4))    ;; ok, because in dynamic binding scope
  (println x))



(defn update [lst f]
  (f))

(defn test []
  (let [x (atom 1)]
    (println "x before: ")
    (println @x)
    (update '(1 2 3) (fn [] (reset! x 2) '(2 4 6)))
    (println "x after: ")
    (println @x)))
(test)