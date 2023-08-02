(def flights
  {["BRU", "LUX"] [(atom 'flightdata1) (atom 'flightdata2)]
   ["PAR LAX"]    [(atom 'flightdata3)  (atom 'flightdata4)]})

;(println (str "flights " (flights ["BRU", "LUX"])))


;example usage: (split '(1 2 3 4 5 6 7 8) 3) returns '((1 2 3)(4 5 6)(7 8))
(defn split [lst size]
  (loop [ls lst
         res '()]
    (if (empty? ls) 
      (reverse res)
      (recur (drop size ls)(cons (take size ls) res)))))


(defn tesst []
  (println "a")
  (defn fni []
    (println "b"))
  (fni)
  (println "c"))

(tesst)
