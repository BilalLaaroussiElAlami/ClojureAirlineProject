(ns flight-reservation_test
  (:require [flight_reservation_parallel.clj]
            [clojure.pprint]))

(print "hello world")
(defn flight-test []
  (let [F (flight_reservation_parallel/make-flight 0, "BRU", "ATL", "Delta", [[600 145 5] [650 50 0] [700 50 0]])]
    flight_reservation_parallel/print-flight-data (@F)))

(flight-test)
