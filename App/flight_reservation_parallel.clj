(ns flight-reservation-parallel
  (:require [clojure.string]
            [clojure.pprint]
            [input-simple]
            [input-random]))


(def logger (agent nil))
;(defn log [& msgs] (send logger (fn [_] (apply println msgs))))
(defn log [& msgs] nil)


; encapsulate a singular flight by an atom
; flights will be a map:  keys : (id,from,to,carrier) values: flightatom (map id,from,to,carrier,pricing)  
; This way we make a clear speration between the part that is immutable and the part that is mutable
; when a customer wants to book a flight we can easily get a list of candidates
; For each candidate flight peek details -> details ok? -> update.
; I need a function (update-atom-if atom condition update) that returns true if the condition succeeds and the atom has been update and false if the condition fail
; The sale becomes then also easy; we just find all the flights associated with the carrier and do a transaction on them

;for a singular flight we could also use a ref. But then when one flight needs to be updated we'd need to do transaction for only 
;one update, the overhead is probably bigger than the swap! of an atom

(defn get-available-seats [travel-class]
  (first travel-class))
(defn get-price [travel-class]
  (first (rest travel-class)))
(defn get-taken-seats [travel-class]
  (first (rest (rest travel-class))))

;I will call flightData  the map that is contained in the flight atom and I will call flight the flight atom
(defn validate-flight [flightData]
  (println (str "validating flightdata: " flightData))
  (let [pricing (flightData :pricing)]
    (every?
     (fn [travel-class]
       (and
        (>= (get-price           travel-class) 0)
        (>= (get-available-seats travel-class) 0)
        (>= (get-taken-seats     travel-class) 0)))
     pricing)))

(defn make-flight-data [id from to carrier pricing]
  {:id  id :from from :to to :carrier carrier :pricing pricing})
(defn make-flight [id from to carrier pricing]
  (atom  {:id  id :from from :to to :carrier carrier :pricing pricing} :validator validate-flight))

;(tries to) update the flight, returns true if the flight got updated and false if the flight couldn't be updated (already full)
(defn update-flight [flight update-flight-data]
  (loop [oldFlightData @flight]
    (let [newFlightData (update-flight-data oldFlightData)]
      (println "called update-flight")
      (println (str "old flight data" oldFlightData))
      (println (str "newFlightData" newFlightData))
      ;if the newdata is not valid (overbooking, negative price, ...) we immediately return false
      ;if we wouldn't do this the automatic validator function in the flight atom would always fail and we would infinitely recur
      ;however this is just an extra safety mechanism because the update-flight-data should not return corrupted data in the first place
      (if (not (validate-flight newFlightData))
        false
        (if (compare-and-set! flight oldFlightData newFlightData)
          [oldFlightData, newFlightData] ;returning the old an new data so the user can confirm the update really happened
          (recur @flight))))))



;I would like to book 5 seats for at most €600 per seat.
;searches a suitable travel class and returns an updated pricing (will return the same pricing if  a suitable travel class isn't founded)
(defn take-seats [pricing maxprice seats]
  (let [result
        (doall  ;might be unnecessary (map on a non-lazy list might not be lazy but idk) TODO find out
         (map (fn [travel-class]
                (if (and (< (get-price travel-class) maxprice) (< seats (get-available-seats travel-class)))
                  [(get-price travel-class) (- (get-available-seats travel-class) seats) (+ (get-taken-seats travel-class) seats)]
                  travel-class))
              pricing))]
    (println "result" result)
    result))


;from, to and id should be already matching with the customer
;extra (redundant) safety mechanism: do this check again
;returns a boolean that indicates whether the booking succeeded
(defn book [flight, customer]
  (let [[oldFlightData newFlightData] (update-flight
                                       flight
                                       (fn [flight-data] (take-seats (flight-data :pricing) (customer :budget) (customer :seats))))]
    (not (oldFlightData = newFlightData))))
;As a convenience, = also returns true when used to compare Java collections against each other, or against Clojure’s immutable collections, 
;if their contents are equal


(defn pricing->str [pricing]
  (->> pricing
       (map (fn [[p a t]] (clojure.pprint/cl-format nil "$~3d: ~3d ~3d" p a t)))
       (clojure.string/join ", ")))

(defn print-flight-data [flight-data]
  (let [{id :id from :from to :to carrier :carrier pricing :pricing} flight-data]
    (println (clojure.pprint/cl-format nil "Flight ~3d from ~a to ~a with ~a: ~a"
                                       id from to carrier (pricing->str pricing)))))

(defn initialize-flights [initial-flights]
  'notUsed)

(defn print-flights [flights]
  ;"Print `flights`."
  (letfn [(pricing->str [pricing]
            (->> pricing
                 (map (fn [[p a t]] (clojure.pprint/cl-format nil "$~3d: ~3d ~3d" p a t)))
                 (clojure.string/join ", ")))]
    (doseq [{:keys [id from to pricing]} (doall (map deref flights))]
      (println (clojure.pprint/cl-format nil "Flight ~3d from ~a to ~a: ~a"
                                         id from to (pricing->str pricing))))))

(defn- update-pricing [flight-data factor]
  ;"Updated pricing of `flight` with `factor`."
  (update flight-data :pricing
          #(map (fn [[p a t]] [(* p factor) a t]) %)))

(comment
  (defn start-sale [flights, carrier]
  ;"Sale: all flights of `carrier` -20%."
    (log "Start sale for" carrier "!")
    (swap! flights
           (fn [old-flights]
             (vec (map
                   (fn [flight]
                     (if (= (:carrier flight) carrier)
                       (update-pricing flight 0.80)
                       flight))
                   old-flights)))))

  (defn end-sale [carrier]
  ;"End sale: all flights of `carrier` +25% (inverse of -20%)."
    (log "End sale for" carrier "!")
    (swap! flights
           (fn [old-flights]
             (vec (map
                   (fn [flight]
                     (if (= (:carrier flight) carrier)
                       (update-pricing flight 1.25)
                       flight))
                   old-flights)))))

  (defn sort-pricing [pricing]
  ;"Sort `pricing` from lowest to highest price."
    (sort-by first pricing))

  (defn filter-pricing-with-n-seats [pricing seats]
  ;"Get `pricing` for which there are at least `seats` empty seats available."
    (filter #(>= (second %) seats) pricing))

  (defn lowest-available-price [flight seats]
    "Returns the lowest price in `flight` for which at least `seats` empty seats
  are available, or nil if none found."
    (-> (:pricing flight)                 ; [[price available taken]]
        (filter-pricing-with-n-seats seats)
        (sort-pricing)
        (first)                             ; [price available taken]
        (first)))                           ; price

  (defn- find-flight [flights customer]
    "Find a flight in `flights` that is on the route and within the budget of
  `customer`. If a flight was found, returns {:flight flight :price price},
  else returns nil."
    (let [{:keys [_id from to seats budget]}
          customer
          flights-and-prices
          ; flights that are on the route and within budget, and their price
          (for [f flights
                :when (and (= (:from f) from) (= (:to f) to))
                :let [lowest-price (lowest-available-price f seats)]
                :when (and (some? lowest-price) (<= lowest-price budget))]
            {:flight f :price lowest-price})
          cheapest-flight-and-price
          (first (sort-by :price flights-and-prices))]
      cheapest-flight-and-price))

  (defn- book [flight price seats]
    "Updates `flight` to book `seats` at `price`."
    (update flight :pricing
            (fn [pricing]
              (for [[p a t] pricing]
                (if (= p price)
                  [p (- a seats) (+ t seats)]
                  [p a t])))))

  (defn- process-customer [flights customer]
    "Try to book a flight from `flights` for `customer`, returning the updated
  flight if found, or nil if no suitable flight was found."
    (if-let [{:keys [flight price]} (find-flight flights customer)]
      (let [updated-flight (book flight price (:seats customer))]
        (log "Customer" (:id customer) "booked" (:seats customer)
             "seats on flight" (:id updated-flight) "at $" price " (< budget of $"
             (:budget customer) ").")
        updated-flight)
      (do
        (log "Customer" (:id customer) "did not find a flight.")
        nil)))

  (def finished-processing?
    "Set to true once all customers have been processed, so that sales process
  can end."
    (atom false))

;idea: process-one-customer -> future and then wait on all futures
  (defn process-customers [customers]
    "Process `customers` one by one."
    (doseq [customer customers]
      (swap! flights
             (fn [flights]
               (if-let [updated-flight (process-customer flights customer)]
                 (assoc flights (:id updated-flight) updated-flight)
                 flights))))
    (reset! finished-processing? true))

  (defn sales-process [carriers time-between-sales time-of-sales]
    "The sales process starts and ends sales periods, until `finished-processing?`
  is true."
    (loop []
      (let [discounted-carrier (rand-nth carriers)]
        (Thread/sleep time-between-sales)
        (start-sale discounted-carrier)
        (Thread/sleep time-of-sales)
        (end-sale discounted-carrier))
      (if (not @finished-processing?)
        (recur))))

  (defn main [& args]
    (let [; Parse first command line argument to get input file
          input-file (first args)
          [initial-flights customers carriers TIME_BETWEEN_SALES TIME_OF_SALES]
          (case input-file
            "simple" [input-simple/flights
                      input-simple/customers
                      input-simple/carriers
                      input-simple/TIME_BETWEEN_SALES
                      input-simple/TIME_OF_SALES]
            "random" [input-random/flights
                      input-random/customers
                      input-random/carriers
                      input-random/TIME_BETWEEN_SALES
                      input-random/TIME_OF_SALES]
            [input-simple/flights
             input-simple/customers
             input-simple/carriers
             input-simple/TIME_BETWEEN_SALES
             input-simple/TIME_OF_SALES])]
    ; Print parameters
      (println "Input file:" input-file)
      (println "Number of flights:" (count initial-flights))
      (println "Number of customers:" (count customers))
      (println "Number of carriers:" (count carriers))
      (println "Time between sales:" TIME_BETWEEN_SALES)
      (println "Time of sales:" TIME_OF_SALES)
    ; Initialize flights atom
      (initialize-flights initial-flights)
    ; Start two threads: one for processing customers, one for sales.
    ; Print the time to execute the first thread.
      (let [f1 (future (time (process-customers customers)))
            f2 (future (sales-process carriers
                                      TIME_BETWEEN_SALES
                                      TIME_OF_SALES))]
      ; Wait until both have finished
        @f1
        @f2
        (await logger))
    ; Print result, for manual verification and debugging
      (println "Flights:")
      (print-flights @flights)))

  (apply main *command-line-args*)
  (shutdown-agents))

;--------------------------------------TESTS----------------------------------------
(defn flight-test []
  (println "testing singular flight")
  (let [F (make-flight 0, "BRU", "ATL", "Delta", [[600 150 0] [650 50 0] [700 50 0]])]
    (println "original F")
    (print-flight-data @F)
    (update-flight F (fn [oldfd] (make-flight-data 0, "BRU", "ATL", "Delta", [[0 0 0] [0 0 0] [0 0 0]]))) ;legal
    (println "F afterlegal update")
    (print-flight-data @F)
    (update-flight F (fn [oldfd] (make-flight-data 0, "BRU", "ATL", "Delta", [[-1 0 0] [0 0 0] [0 0 0]]))) ;illegal nothing should change
    (println "F after illegal update")
    (print-flight-data @F)))

(defn book-test []
  (let [F (make-flight 0, "BRU", "ATL", "Delta", [[600 150 0] [650 50 0] [700 50 0]])
        customer  {:id  1 :from "BRU" :to "ATL" :seats 5 :budget 550}]
    (println  (str "result booking" (book F customer)))))

;(flight-test)
(book-test)