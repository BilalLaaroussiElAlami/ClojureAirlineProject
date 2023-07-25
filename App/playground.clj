(def flights
  {["BRU", "LUX"] [(atom 'flightdata1) (atom 'flightdata2)]
   ["PAR LAX"]    [(atom 'flightdata3)  (atom 'flightdata4)]})

(println (str "flights " (flights ["BRU", "LUX"])))



(println (filter (fn [x] (< x 0)) '(1 2 3)))