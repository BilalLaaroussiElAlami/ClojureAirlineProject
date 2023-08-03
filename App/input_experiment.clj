(ns input-experiment)

(def flights
  [{:id 0
    :from "BRU" :to "LON"
    :carrier "Ryanair"
    :pricing [[100 150 0] ; price; # seats available at that price; # seats taken at that price
              [150  50 0]
              [200  50 0]
              [300  50 0]]}
   {:id 1
    :from "BRU" :to "LON"
    :carrier "Ryanair"
    :pricing [[100  50 0]
              [200 150 0]
              [320  20 0]
              [240  30 0]]}
   {:id 2
    :from "BRU" :to "LON"
    :carrier "Brussels Airlines"
    :pricing [[250 100 0]
              [300  50 0]]}
   {:id 3
    :from "BRU" :to "LON"
    :carrier "Brussels Airlines"
    :pricing [[250 100 0]
              [300  50 0]]}
   {:id 4
    :from "BRU" :to "LON"
    :carrier "Brussels Airlines"
    :pricing [[250 100 0]
              [350  50 0]]}
   {:id 5
    :from "BRU" :to "LON"
    :carrier "Ryanair"
    :pricing [[150 100 0]
              [300 100 0]]}
   {:id 6
    :from "BRU" :to "MAD"
    :carrier "Iberia"
    :pricing [[200 150 0]
              [250  50 0]
              [300 100 0]]}
   {:id 7
    :from "BRU" :to "MAD"
    :carrier "Brussels Airlines"
    :pricing [[200 150 0]
              [250  50 0]
              [300  80 0]
              [350  20 0]]}
   {:id 8
    :from "BRU" :to "MAD"
    :carrier "Iberia"
    :pricing [[200 150 0]
              [250  50 0]
              [300  80 0]
              [350  20 0]]}
   {:id 9
    :from "BRU" :to "MAD"
    :carrier "Brussels Airlines"
    :pricing [[250 150 0]
              [300  50 0]]}
   {:id 10
    :from "BRU" :to "MAD"
    :carrier "Iberia"
    :pricing [[250 150 0]
              [300  50 0]]}
   {:id 11
    :from "BRU" :to "MAD"
    :carrier "Ryanair"
    :pricing [[150 150 0]
              [300  50 0]]}])

(def airport-codes
  '("JFK" "LAX" "ORD" "ATL" "LHR" "CDG" "DXB" "PEK" "HND" "FRA"
    "AMS" "ICN" "SIN" "HKG" "DEL" "MAD" "CAN" "DFW" "DEN" "SFO"
    "MIA" "IST" "BKK" "CGK" "MUC" "PDX" "MSP" "SEA" "BOS" "NRT"
    "IAD" "SVO" "MCO" "PVG" "YYZ" "EWR" "DUB" "MNL" "CPH" "ZRH"
    "MXP" "ATH" "BRU" "VIE" "LIS" "BLR" "BOM" "GIG" "GRU" "JNB"
    "AUH" "DOH" "KUL" "SYD" "LYS" "MRS" "VLC" "OSL" "ARN" "HEL"
    "WAW" "PRG" "BUD" "OTP" "VIE" "ZAG" "KRK" "MLA" "IST" "CPH"
    "ATH" "ATH" "BRU" "LUX" "AMS" "ZRH" "GVA" "LIS" "OPO" "BCN"
    "MAD" "PMI" "IBZ" "VLC" "AGP" "ALC" "FCO" "CIA" "MXP" "LIN"
    "NAP" "VCE" "FRA" "HHN" "STR" "MUC" "CGN" "DUS" "TXL" "SXF"
    "HAM" "BRE" "HEL" "OUL" "TLL" "RIX" "WAW" "KRK" "BUD" "OTP"
    "SOF" "PRG" "VIE" "BTS" "ZAG" "SPU" "LYS" "MRS" "NCE" "NTE"
    "TLS" "MRS" "ATH" "SKG" "HER" "CHQ" "RHO" "KGS" "LCA" "PFO"
    "IST" "SAW" "ADA" "ADB" "ESB" "SAW" "IZM" "AYT" "LXR" "SSH"
    "HRG" "CAI" "ARN" "GOT" "MMX" "BLL" "OSL" "SVG" "TRD" "SVG"
    "KEF" "DBV" "SPU" "RJK" "ZAD" "PUY" "BWK" "RZE" "SZZ" "WRO"
    "KTW" "LUZ" "POZ" "GDN" "BZG" "WMI" "WAW" "KRK" "LCJ" "BTS"
    "KSC" "LZS" "NIS" "PRG" "BRQ" "OSR" "PED" "PRG" "MUC" "FMM"
    "NUE" "STR" "FDH" "EAP" "CGN" "DUS" "NRN" "MUC" "ZQW" "HHN"
    "SCN" "FRA" "HHN" "LUX" "SCN" "BGY" "LIN" "MXP" "PMF" "VBS"))
(def carriers
  ["UPS" "FedEx" "DHL" "USPS" "TNT" "SIA" "Lufthansa" "ANA" "Qantas" "Air Canada"
   "Emirates" "British Airways" "Cathay Pacific" "Delta" "United Airlines"
   "Southwest" "JetBlue" "Air France" "KLM" "RAM"])



(def flights 
  (for [id (range 10000)] ;10 000 flights
    (let [from (rand-nth airport-codes)
          to   (rand-nth airport-codes)
          carrier (rand-nth carriers )]
      {:id id :from from :to to :carrier carrier :pricing [[100 150 0] [150  50 0][200  50 0][300  50 0]]})))
     
(def customers
  (for [id (range 1000000)] ;1000 000 customers
    (let [from (rand-nth airport-codes)
          to   (rand-nth airport-codes)]
      {:id     id
       :from   from
       :to     to
       :seats  (+ (rand-int 4) 1)        ; 1-4
       :budget (+ (rand-int 600) 200)}))) ; 200-799

(def TIME_BETWEEN_SALES 50) ; milliseconds
(def TIME_OF_SALES 10)

;(println "flights")
;(println (take 10 flights))
;(println "customers")
;(println (take 10 customers))
