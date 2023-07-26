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

