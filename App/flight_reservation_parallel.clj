(ns flight-reservation-parallel
  (:require [clojure.string]
            [clojure.pprint]
            [input-simple]
            [input-random]
            [input-experiment]
            )
  (:use clojure.test))


(def logger (agent nil))
(defn log [& msgs] (send logger (fn [_] (apply println msgs))))



; encapsulate a singular flight by an atom
; flights will be a map:  keys are (from,to) values are list of atom(flightdata)
;example:
(comment
  {["BRU" "LUX"] [(atom 'flightdata1) (atom 'flightdata2)]
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
(def reasonBooking "reasonBooking")
(def reasonSale    "reasonSale")
(def carriers-undergoing-sale (atom '()))
(defn update-flight [flight update-flight-data reason]
  (loop [oldFlightData @flight]
    (let [newFlightData (update-flight-data oldFlightData)]
      (if  (and (= reason reasonBooking)(some  (fn [carr] (= carr (@flight :carrier)))@carriers-undergoing-sale))
        ;this means the flights of the carrier are undergoing a sale, to get atomic/consistent results we recur
        (recur @flight)
        ;if the newdata is not valid (overbooking, negative price, ...) we dont update. extra safety mechanism because update-flight-data function should not return corrupted data in the first place
        (if (not (validate-flight newFlightData))
          [oldFlightData, oldFlightData]
          (if (compare-and-set! flight oldFlightData newFlightData)
            [oldFlightData, newFlightData] ;returning the old an new data so the user can confirm if a change really happened
            (recur @flight)))))))


(defn initialize-flights-old [flights]
  (group-by (fn [flight] [(@flight :from) (@flight :to)])
            (doall (map (fn [f] (atom f)) flights))))



(defn initialize-flights [flights]
  (let [atoms  (doall (map (fn [f] (atom f)) flights))
        grouped-by-from-and-to (group-by (fn [flight] [(@flight :from) (@flight :to)]) atoms)
        grouped-by-carrier     (group-by (fn [flight] (@flight :carrier)) atoms)]
    [grouped-by-from-and-to grouped-by-carrier]))



;I would like to book 5 seats for at most €600 per seat.
;searches a suitable travel class and returns an updated PRICING (will return the same pricing if  a suitable travel class isn't founded)
(defn take-seats [pricing maxprice seats]
  (let [found-suitable-travel-class (atom false)
        paid-price (atom nil)
        result
        (doall (map (fn [travel-class]
                      (if (and (<= (get-price travel-class) maxprice) (<= seats (get-available-seats travel-class))  (not @found-suitable-travel-class))
                        (do (reset! paid-price (get-price travel-class))
                            (reset! found-suitable-travel-class true)
                            [(get-price travel-class) (- (get-available-seats travel-class) seats) (+ (get-taken-seats travel-class) seats)])
                        travel-class))
                    pricing))]
    ;(if (not (= pricing result))
      ;(log "(take-seats" pricing "\n" maxprice seats "\nresult: " result "\n"))
    [@paid-price result]))



;from, to and id should be already matching with the customer
;extra (redundant) safety mechanism: do this check again
;returns a boolean that indicates whether the booking succeeded
(defn book [flight, customer]
  (let [paid (atom nil)
        [oldFlightData newFlightData] (update-flight
                                       flight
                                       (fn [flight-data]
                                         (let [[paid-price updated-pricing]  (take-seats (flight-data :pricing) (customer :budget) (customer :seats))]
                                           (reset! paid paid-price)
                                           (make-flight-data (flight-data :id)
                                                             (flight-data :from)
                                                             (flight-data :to)
                                                             (flight-data :carrier)
                                                             updated-pricing)))
                                       reasonBooking)]
    ;(println (str "oldFlightData" oldFlightData))
    ;(println (str "newFlightData") newFlightData)
    (if (= oldFlightData newFlightData)
      {:result-booking false :customer customer}
      {:result-booking true  :customer customer :paid-price @paid})))

;As a convenience, = also returns true when used to compare Java collections against each other, or against Clojure’s immutable collections, 
;if their contents are equal


(defn pricing->str [pricing]
  (->> pricing
       (map (fn [[p a t]] (clojure.pprint/cl-format nil "$~3d: ~3d ~3d" p a t)))
       (clojure.string/join ", ")))

(defn print-flight-data [flight-data]
  (let [{id :id from :from to :to carrier :carrier pricing :pricing} flight-data]
    (println (clojure.pprint/cl-format nil "Flight ~3d from ~a to ~a: ~a"
                                       id from to (pricing->str pricing)))))

(defn flight-data->str [flight-data]
  (let [{id :id from :from to :to carrier :carrier pricing :pricing} flight-data]
    (clojure.pprint/cl-format nil "Flight ~3d from ~a to ~a: ~a"
                              id from to (pricing->str pricing))))

(defn flights->str [flights]
  (loop [fls (reverse (vals flights)) result ""]
    (let [flightatoms  (first fls)
          flight-data-s (doall (map deref flightatoms))
          flight-datas-str  (clojure.string/join "\n"  (doall (map flight-data->str flight-data-s)))
          new-result (str flight-datas-str "\n" result)]
      (if (empty? fls)
        new-result
        (recur (rest fls) new-result)))))

(defn print-flights [flights]
  (doall (for [fass flights]
           (doall (for [fat (val fass)]
                    (print-flight-data @fat))))))


;(defn find-and-book-flight [flights customer]
;  (let [candidate-flights (flights [(customer :from) (customer :to)])
;        ;filter is lazy so customer can maximally book one flight!, if filter wouldn't be lazy a customer could book multiple times 
;        result-booking (first (filter (fn [flight] ((book flight customer) :result-booking)) candidate-flights))] ;get first booking where result-booking is true, nil if none 
;    (if result-booking
;      {:result-booking true :customer customer }
;      {:result-booking false :customer customer})))

(defn find-and-book-flight [flights customer]
   (let [candidate-flights (flights [(customer :from) (customer :to)])]
    (loop [flights candidate-flights]
      (if (empty? flights)
        {:result-booking false :customer customer}
        (let [first (first flights)
              booking (book first customer)]
          (if (booking :result-booking)
            booking
            (recur (rest flights))))))))


;unfortuantely this function doesnt work as expexted
(defn find-and-book-flight-with-logs [flights customer]
  (locking flights  ;;we lock flights so we can see the state of flights before and after every one booking without another booking changing the state inbetween
    (log (str "state before booking: \n" (flights->str flights) "\n"))
    (log (str "customer " customer "\n"))
    (let [candidate-flights (flights [(customer :from) (customer :to)])
          result-booking (first (filter (fn [flight] (book flight customer)) candidate-flights))]
      (log (str "state after booking: \n" (flights->str flights) "\n"))
      result-booking)))


(def finished-processing?
  "Set to true once all customers have been processed, so that sales process
  can end."
  (atom false))

;TODO bigger granularity !!!!!!

;example usage: (split '(1 2 3 4 5 6 7 8) 3) returns '((1 2 3)(4 5 6)(7 8))
(defn split [lst size]
  (loop [ls lst
         res '()]
    (if (empty? ls)
      (reverse res)
      (recur (drop size ls) (cons (take size ls) res)))))

;; https://clojuredocs.org/clojure.core/pmap
;; pmap is implemented using Clojure futures. Futures run in threads. 
;; These threads of a pmap's evaluation run independently from each other.

;;pick the amount of elements that will be processed in one future sequentially
;(defn coarse-pmap [fun args granularity]
;  (let [parts (split args granularity)]
;    (doall
;     (pmap (fn [part] (doall (map fun part))) parts))))


(def number-threads (atom 4))
;;pick the amount of threads
(def testing-sale-consistency? (atom false))


(defn coarse-pmap-threads [fun args]
  (let [size (quot (count args) @number-threads) ;;size of list that will be in one future
        parts (split args size)]
    (doall (pmap (fn [part] (doall (map fun part))) parts))))



;batches of customer, a singular batch will be processed sequently, different batches will be processed in parallel
(defn process-customers [customers flights]
  ;(doall (pmap
  ;        (fn [customer] (find-and-book-flight flights customer))
  ;        customers))
  (let [result-processing (coarse-pmap-threads (fn [customer] (find-and-book-flight flights customer)) customers)]
    (reset! finished-processing? true)
    (flatten result-processing)))


(defn- update-pricing [flight factor]
  "Updated pricing of `flight` with `factor`."
  (update-flight flight #(map (fn [[p a t]] [(* p factor) a t]) %) reasonSale))

;;when testing sale consistency we want to disprove that succesfull bookings and discounting of flights of the same carrier can be interleaved, 
;;to prove this we have to leave a chance to interleave them. The sleep statement prevents that the discount happens 
;;so quick that there is no chance for interleaving, and that the test would succeed by chance
(defn sale [flights carrier factor]
  (reset! carriers-undergoing-sale  (cons carrier @carriers-undergoing-sale))
  (let [flights-carrier (flights :carrier)]
    (coarse-pmap-threads (fn [fl] (when @testing-sale-consistency? (Thread/sleep (rand-int 20))) (update-pricing fl factor)) flights-carrier))
  (reset! carriers-undergoing-sale  (filter (fn [c] (not (= c carrier))) @carriers-undergoing-sale)))

(defn start-sale  [flights carrier]
  (sale flights carrier 0.80))
(defn end-sale [flights carrier]
  (sale flights carrier 1.25))

(defn sales-process [flights carriers time-between-sales time-of-sales]
  "The sales process starts and ends sales periods, until `finished-processing?`
  is true."
  (loop []
    (let [discounted-carrier (rand-nth carriers)]
      (Thread/sleep time-between-sales)
      (start-sale flights discounted-carrier)
      (Thread/sleep time-of-sales)
      (end-sale flights discounted-carrier))
    (if (not @finished-processing?)
      (recur))))

;---------------------------------------MAIN-----------------------------------------------

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
           input-simple/TIME_OF_SALES])
        ;initialize flights map 
        [flightsForBooking, flightsForSale]  (initialize-flights initial-flights)]
    ; Print parameters
    (println "!!! YOU ARE IN THE PARALLEL VERSION !!!")
    (println "Input file:" input-file)
    (println "Number of flights:" (count initial-flights))
    (println "Number of customers:" (count customers))
    (println "Number of carriers:" (count carriers))
    (println "Time between sales:" TIME_BETWEEN_SALES)
    (println "Time of sales:" TIME_OF_SALES)


    ; Start two threads: one for processing customers, one for sales.
    ; Print the time to execute the first thread.
    (let [f1 (future (time (process-customers customers flightsForBooking)))
          f2 (future (sales-process flightsForSale carriers
                                    TIME_BETWEEN_SALES
                                    TIME_OF_SALES))]
      ; Wait until both have finished
      @f1
      @f2
      (await logger))

    (println "Flights:")
    (print-flights flightsForBooking)))


;(apply main *command-line-args*)


;--------------------------------------TESTS----------------------------------------

(defn test-take-seats []
  (let [pricing [[300 5 10]
                 [350  10 0]
                 [370  20 0]
                 [380  30 0]]
        maxprice 375
        seats 15]
    (println "TEST take-seats")
    (println (take-seats pricing maxprice seats))))

;(test-take-seats)

(defn flight-test []
  (println "testing singular flight")
  (let [F (make-flight 0, "BRU", "ATL", "Delta", [[600 150 0] [650 50 0] [700 50 0]])]
    (println "original F")
    (print-flight-data @F)
    (update-flight F (fn [oldfd] (make-flight-data 0, "BRU", "ATL", "Delta", [[0 0 0] [0 0 0] [0 0 0]])) reasonBooking) ;legal
    (println "F afterlegal update")
    (print-flight-data @F)
    (update-flight F (fn [oldfd] (make-flight-data 0, "BRU", "ATL", "Delta", [[-1 0 0] [0 0 0] [0 0 0]])) reasonBooking) ;illegal nothing should change
    (println "F after illegal update")
    (print-flight-data @F)))

;tests the booking of a flight for a customer that should be able to book a flight, and customers that shouldn't be able tto book a flight
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


(defn find-and-book-flight-test []
  (let [flights   {["PAR LAX"]    [(atom 'flightdata3) (atom 'flightdata4)]
                   ["BRU", "ATL"] [(atom {:id 0 :from "BRU" :to "ATL"
                                          :carrier "Delta"
                                          :pricing [[600 150 0]
                                                    [650 50 0]
                                                    [700 50 0]]})
                                   (atom {:id 1 :from "BRU" :to "ATL"
                                          :carrier "Delta"
                                          :pricing [[600 150 0]
                                                    [650 50 0]
                                                    [700 50 0]]})]}
        customerFindFlight {:id 0 :from "BRU" :to "ATL"
                            :seats 5 :budget 600}
        customerNoFindFlight  {:id 1 :from "BRU" :to "TNG"
                               :seats 5 :budget 600}]
    (println "FIND-FLIGHT-TEST")
    (println "result lucky customer: ")
    (println (find-and-book-flight flights customerFindFlight))
    (println)
    (println "result unlucky customer: ")
    (println (find-and-book-flight flights customerNoFindFlight))))

(defn initialize-flights-test []
  (println "INITIALIZE FLIGHTS TEST ")
  (let [[grouped-by-from-and-to grouped-by-carrier] (initialize-flights input-simple/flights)]
    (println "grouped by from and to:")
    (println grouped-by-from-and-to)
    (println "grouped by carrier")
    (println grouped-by-carrier)))

(defn flights->str-test []
  (println "FLIGHTS->STR TEST")
  (let [initial-flights input-simple/flights
        flights (initialize-flights initial-flights)]
    (println (flights->str flights))
    (print-flights flights)))

(defn process-customer-test []
  "zet test-process-customers? variable handmatig op true")


;;test that no overbookings are possible
(defn test-no-overbooking []
  (println "TEST NO OVERBOOKINGS")
  (let [[flights,_] (initialize-flights
                     [{:id 0 :from "BRU" :to "ATL" :carrier "Delta"
                       :pricing [[600 5 0]  ;there are only 5 seats at 600 euro
                                 [1000 50 0]
                                 [2000 50 0]]}])
        customers  (for [id (range 100)] {:id id :from "BRU" :to "ATL" :seats  5 :budget 600})]  ;100 customers trying to book 5 seats at max 600 euro
    ;only one customer should be able to do a booking  
    (run! println (process-customers customers flights))
    (println "FLIGHTS AFTER PROCESSED CUSTOMERS: ")
    (print-flights flights)))

;-----------------------------------------CORRECTNESS TESTS---------------------------------------------------

(deftest test-no-overbooking
  (let [[flights,_] (initialize-flights
                     [{:id 0 :from "BRU" :to "ATL" :carrier "Delta"
                       :pricing [[600 10 0]  ;there are only 10 seats at 600 euro
                                 [1000 50 0]
                                 [2000 50 0]]}])
        customers  (for [id (range 100)] {:id id :from "BRU" :to "ATL" :seats  5 :budget 600}) ;100 customers trying to book 5 seat at max 600 euro
        count-customers-success (count (filter  (fn [booking] (booking :result-booking)) (process-customers customers flights)))]
     ;only 2 customer should be able to do a booking  
    (is (= 2 count-customers-success))))


(deftest test-booking
  (let [[flights,_] (initialize-flights
                     [{:id 0 :from "BRU" :to "ATL" :carrier "Delta"
                       :pricing [[500 50 0]
                                 [1000 50 0]
                                 [2000 50 0]]}])
        customers (for [id (range 150)] {:id id :from "BRU" :to "ATL" :seats  1 :budget 3000})
        count-customers-success (count (filter  (fn [booking] (booking :result-booking)) (process-customers customers flights)))]
    ;all the customers should be able to book
    (is (= 150 count-customers-success))))

(deftest test-no-overbooking-2
  (let [[flights _] (initialize-flights
                     [{:id 0 :from "BRU" :to "ATL" :carrier "Delta"
                       :pricing [[500 50 0]
                                 [1000 50 0]
                                 [2000 50 0]]}])
        customers (for [id (range 300)] {:id id :from "BRU" :to "ATL" :seats  1 :budget 3000})
        count-customers-success (count (filter  (fn [booking] (booking :result-booking)) (process-customers customers flights)))]
    ;only half the customers should be able to book
    (is (= 150 count-customers-success))))

;;tests that it's not possible that customer A sees a discounted flight and customer B sees an undiscounted flight
(deftest test-sale-consistency
  (reset! testing-sale-consistency? true)
  (let [ ;1000 customers want to book one seat for max 200 euro
        customers (for [id (range 1000)] {:id id :from "BRU" :to "ATL" :seats  1 :budget 200}) 
        ;1000 seats at 200
        [flightsForBooking flightsForSale]  (initialize-flights (for [id (range 20)] {:id id :from "BRU" :to "ATL" :carrier "DELTA" :pricing [[200 1000 0]]}))   
        f2 (future (start-sale flightsForSale "DELTA"))
        resultsBookings (future (process-customers customers flightsForBooking))]
    @f2 ;wait for sale to finish
    ;;check all customers paid the same price
    (is (apply = (map (fn [bkng] (bkng :paid-price)) @resultsBookings)))
    ;;check that the sale effectively did take place
    ;(is  (apply = (map (fn[flight-data] (get-price (first (flight-data :pricing)))) (doall (map @ (vals flightsForBooking))))))
    ;(let [x (vals flightsForBooking)
    ;      y (println "gra" (first x))
    ;      z (first (first x))
    ;      b (print "oooh" z)
    ;      list-of-flight-data-objects (vals flightsForBooking)
    ;      list-of-flight-data  (doall(map @ list-of-flight-data-objects))
    ;      pricings (map :pricing list-of-flight-data)
    ;      travel-classes (map first pricings)
    ;      prices (map get-price travel-classes)]
    ;  (is (apply = prices)))
    (println "test-sale-consistency, paid price" ((first @resultsBookings) :paid-price))
    ;(doall (map println @resultsBookings))
    )
  (reset! testing-sale-consistency? false))
  
;test one customer can only book one flight
;proof

;-----------------------------------------------PERFORMANCE EXPERIMENTS------------------------------------------


(defn experiment-speedup-threads []
  (reset! number-threads 8)
  (let [a (println "aaaaaa")
        [flightsForBooking flightsForSale] (initialize-flights input-experiment/flights)
        a (println "xxxxxxxxx")
        f1 (future (time (process-customers input-experiment/customers flightsForBooking)))
        b (println "ccccccccc")
        f2 (future (sales-process flightsForSale input-experiment/carriers
                                  input-experiment/TIME_BETWEEN_SALES
                                  input-experiment/TIME_OF_SALES))
        c (println "zzzzzzzzzz")]
    ; Wait until both have finished
    @f1
    @f2
    (await logger)
    ))

(experiment-speedup-threads)









;(flight-test)
;(book-test)
(find-and-book-flight-test)
;(initialize-flights-test)
;(flights->str-test)


(run-tests 'flight-reservation-parallel)
;(test-no-overbooking)
(shutdown-agents)