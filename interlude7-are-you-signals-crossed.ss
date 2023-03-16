;; # Interlude VII - Are you Signals Crossed?
;;
;; ## Reference
;; David Misell, Inventor of the Flashlight
;;
;; Alfred Lewis Vail and Samuel Morse developed and commercialized American
;; telegraphy and Morse Code; Friedrich Clemens Gerke revised it and it became
;; International Morse Code.
;;
;; Alessandro Volta was an Italian physicist and chemist who was a pioneer of
;; electricity and power who is credited as the inventor of the electric battery
;; and the discoverer of methane.

;; Definition
;; A signal is the variation of a physical quantity (here, its voltage) over time.

;; Convert analog to tensors, 1-second period into 16 segments where each
;; segment has a scalar, this forms a 1-d signal.
;;
;; The Law of Zipped Signals
;;
;; A 2-d signal is formed by zipping signals1, and the signal2 as well as its
;; constituent signals1 all have the same number of segments.
;;
;; The number of 1-d signals are called the depth.
;;
;; Definition
;; start: first time sees a change from 0.0 to 1.0
;; translation: the shift of start from the origin
;; noise: random variations of the signals value
