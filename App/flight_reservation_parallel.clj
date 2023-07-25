(ns flight-reservation-parallel
  (:require [clojure.string]
            [clojure.pprint]
            [input-simple]
            [input-random]))


(def logger (agent nil))
;(defn log [& msgs] (send logger (fn [_] (apply println msgs))))
(defn log [& msgs] nil)


; encapsulate a singular flight by an atom
; flights will be a map:  keys are (from,to) values are list of atom(flightdata)
(comment
  example {["BRU", "LUX"] [(atom 'flightdata1) (atom 'flightdata2)]
           ["PAR LAX"]    [(atom 'flightdata3)  (atom 'flightdata4)]})
; where flightdata is [id from to carrier pricing]
; This way we make a clear speration between the part that is immutable and the part that is mutable
; when a customer wants to book a flight we can easily get a list of candidates
; For each candidate flight peek details -> details ok? -> update.
; I need a function (update-atom-if atom condition update) that returns true if the condition succeeds and the atom has been update and false if the condition fail
; The sale becomes then also easy; we just find all the flights associated with the carrier and do a transaction on them

;for a singular flight we could also use a ref. But then when one flight needs to be updated we'd need to do transaction for only 
;one update, the overhead is probably bigger than the swap! of an atom

(defn get-available-seats [travel-class]
  (first (rest travel-class)))
(defn get-price [travel-class]
  (first travel-class))
(defn get-taken-seats [travel-class]
  (first (rest (rest travel-class))))

;I will call flightData  the map that is contained in the flight atom and I will call flight the flight atom
(defn validate-flight [flightData]
  ;(println (str "validating flightdata: " flightData))
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
      ;(println "called update-flight")
      ;(println (str "old flight data" oldFlightData))
      ;(println (str "function " update-flight-data))
      ;(println (str "newFlightData " newFlightData))
      ;if the newdata is not valid (overbooking, negative price, ...) we dont update
      ;this is just an extra safety mechanism because the update-flight-data function should not return corrupted data in the first place
      ;but if it would and we wouldn't do this check the validator function of the atom would also fail an we would infinitely recur
      (if (not (validate-flight newFlightData))
        [oldFlightData, oldFlightData]
        (if (compare-and-set! flight oldFlightData newFlightData)
          [oldFlightData, newFlightData] ;returning the old an new data so the user can confirm if a change really happened
          (recur @flight))))))



;I would like to book 5 seats for at most €600 per seat.
;searches a suitable travel class and returns an updated PRICING (will return the same pricing if  a suitable travel class isn't founded)
(defn take-seats [pricing maxprice seats]
  (let [result
          ;might be unnecessary (map on a non-lazy list might not be lazy but idk) TODO find out 
        (map (fn [travel-class]
               (if (and (<= (get-price travel-class) maxprice) (<= seats (get-available-seats travel-class)))
                 [(get-price travel-class) (- (get-available-seats travel-class) seats) (+ (get-taken-seats travel-class) seats)]
                 travel-class))
             pricing)]
    ;(println "result take seats" result)
    ;map is lazy vec will make a vector out of the lazy sequence
    (vec result)))


;from, to and id should be already matching with the customer
;extra (redundant) safety mechanism: do this check again
;returns a boolean that indicates whether the booking succeeded
(defn book [flight, customer]
  (let [[oldFlightData newFlightData] (update-flight
                                       flight
                                       (fn [flight-data]
                                         (make-flight-data (flight-data :id)
                                                           (flight-data :from)
                                                           (flight-data :to)
                                                           (flight-data :carrier)
                                                           (take-seats (flight-data :pricing) (customer :budget) (customer :seats)))))]
    ;(println (str "oldFlightData" oldFlightData))
    ;(println (str "newFlightData") newFlightData)
    (if (= oldFlightData newFlightData)
      nil
      newFlightData)))

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


(defn print-flights [flights]
  ;"Print `flights`."
  (letfn [(pricing->str [pricing]
            (->> pricing
                 (map (fn [[p a t]] (clojure.pprint/cl-format nil "$~3d: ~3d ~3d" p a t)))
                 (clojure.string/join ", ")))]
    (doseq [{:keys [id from to pricing]} (doall (map deref flights))]
      (println (clojure.pprint/cl-format nil "Flight ~3d from ~a to ~a: ~a"
                                         id from to (pricing->str pricing))))))

(defn find-and-book-flight [flights customer]
  (let [candidate-flights (flights [(customer :from) (customer :to)])]
    (first (filter (fn [flight] (book flight customer)) candidate-flights))))



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
        validcustomer  {:id  1 :from "BRU" :to "ATL" :seats 5 :budget 600}
        unvalidcustomerA {:id  1 :from "BRU" :to "ATL" :seats 5 :budget 200} ;te weinig budget 
        unvalidcustomerB {:id  1 :from "BRU" :to "ATL" :seats 160 :budget 200}] ;te veel stoelen
    (println "BOOKING TEST")
    (println (str "F before booking: " @F))
    (println (str "valid customer    " validcustomer))
    (println (str "result booking    " (book F validcustomer)))
    (println (str "F after booking:  " @F))
    (println)
    (println (str "unvalidcustomerA  " unvalidcustomerA))
    (println (str "result booking    " (book F unvalidcustomerA)))
    (println (str "F after booking:  " @F))
    (println)
    (println (str "unvalidcustomerB  " unvalidcustomerB))
    (println (str "result booking    " (book F unvalidcustomerB)))
    (println (str "F after booking:  " @F))))


(defn find-flight-test []
  (let [flights   {["PAR LAX"]    [(atom 'flightdata3) (atom 'flightdata4)]
                   ["BRU", "ATL"] [(atom {:id 0 :from "BRU" :to "ATL"
                                          :carrier "Delta"
                                          :pricing [[600 150 0]
                                                    [650 50 0]
                                                    [700 50 0]]})
                                   (atom {:id 0 :from "BRU" :to "ATL"
                                          :carrier "Delta"
                                          :pricing [[600 150 0]
                                                    [650 50 0]
                                                    [700 50 0]]})]}
        customer {:id 0 :from "BRU" :to "ATL"
                  :seats 5 :budget 600}]
    (println "FIND-FLIGHT-TEST")
    (println "result: ")
    (println @(find-and-book-flight flights customer))))



;(flight-test)
;(book-test)
(find-flight-test)