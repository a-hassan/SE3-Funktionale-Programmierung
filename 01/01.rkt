#lang racket

;;; ==============================================================================
;;; Übungen zu Softwareentwicklung III,
;;; Funktionale Programmierung
;;; Blatt 1, Woche 2
;;; ==============================================================================

;;; 1 Konversionsfunktionen
;; 1.1 Bogenmaß und Grad

; Degrees → Radians
(define (degrees_to_radians alpha)
  (* (/ (* 2 pi) 360) alpha))

; gerundet
(define CONVERSIONFACTORTBOGENMASS 0.0174533)
(define (GradToBogenmass Grad)
 (* Grad CONVERSIONFACTORTBOGENMASS))

; Radians → Degrees 
(define (radians_to_degrees alpha)
  (* (/ 360 (* 2 pi)) alpha))

; gerundet
(define CONVERSIONFACTORTGRAD 57.2958)
(define (BogenmassTOGrad Bogenmass)
 (* Bogenmass CONVERSIONFACTORTGRAD))

;; 1.2 Umkehrfunktion acos

; sin²(α) + cos²(α) = 1
; sin(α) = sqrt(1 - cos²(α))
; tan(α) = sin(α) / cos²(α)
; tan(α) = (sqrt(1 - cos²(α))) / cos²(α)
; tan²(α) = (1 - cos²(α)) / cos²(α)
; tan²(α) = (1 / cos²(α)) - 1
; (tan²(α) + 1) * cos²(α) = 1
; cos²(α) = 1 / (tan²(α) + 1)
; cos(α) = sqrt(1 / (tan²(α) + 1))
(define (my-cos x)
  (sqrt
   (/ 1 (+ (expt (tan x) 2) 1))))

; arcos
(define (my-acos x)   ; die acos Funktion gibt es schon, ich bin mir nicht sicher wie das mit Redefinition funktioniert.
         (* 2 (atan (sqrt (/ (- 1 x) (+ 1 x))))))

;; 1.3 Kilometer und Seemeilen
(define (SeemeileToKilometer Kilometer)
  (* Kilometer 1.852))

;;; -------------------------------------------------------------------------------------
;;; 2 Großkreisentfernung und Kurse
;;; -------------------------------------------------------------------------------------
;; 2.1 Großkreisentfernung
; cos dG = sin ’A ∗ sin ’B + cos ’A ∗ cos ’B ∗ cos ∆λ

(define MEANRADIUS 6.371)
; latA: Latitude for Point A
; latB: Latitude for Point B

(define (gLaengeInBogenmass lonB lonA)
         (GradToBogenmass (- lonB lonA)))


(define (distanzAB phiA phiB lambdaA lambdaB)
  (SeemeileToKilometer
   (*
    (radians_to_degrees
     (my-acos
      (+
       (* (sin (degrees_to_radians phiA)) (sin (degrees_to_radians phiB)))
       (* (my-cos (degrees_to_radians phiA)) (my-cos (degrees_to_radians phiB)) (my-cos (degrees_to_radians (abs (- lambdaA lambdaB))))))))
    60)))

; von Oslo (59:93N, 10:75E) nach Hongkong (22:20N,114:10E)
(write "distance from Oslo to Hongkon: ")
(distanzAB 59.93 22.20 10.75 114.10) ; 8.577,64 km
                                    
; von San Francisco(37:75N, 122:45W) nach Honolulu (21:32N, 2157:83W)
(write "distance from San Francisco to Honolulu ")
(distanzAB 37.75 21.32 -122.45 -157.83) ; 3.853,74 km

; von der Osterinsel (27:10S, 109:40W) nach Lima (12:10S, 77:05W)
(write "distance from Osterinsel to Lima: ")
(distanzAB -27.10 -12.10 -109.40 -77.05) ; 3.762,44 km

;; 2.2 Anfangskurs
(define (Anfangskurs latA latB lonA lonB)
  (/
   (- (sin latB)
      (* (cos(distanzAB latA latB lonA lonB))
         (sin latA)))
   (* (cos latA) (sin (distanzAB latA latB lonA lonB)))))

(define (AnfangskursAngle anfangsAngle latA latB lonA lonB)
  (acos ((BogenmassTOGrad (Anfangskurs latA latB lonA lonB))))
  (if
   (and ((AnfangskursAngle anfangsAngle) > 0 ) (AnfangskursAngle anfangsAngle) < 180 )
      anfangsAngle (- (360 anfangsAngle))))

;; 2.3 Himmelsrichtungen 
; 2.3.1 GradToHimmelsrichtung

(define (GradToHimmelsrichtung x) 
  (if (or (and (> x 348.75) (<= x 360)) (and (>= x 0) (<= x 11.25)))  (display "N")
  (if (and (> x 11.25) (<= x 33.75))  (display "NNO") 
  (if (and (> x 33.75) (<= x 56.25))   (display "NO")
  (if (and (> x 56.25) (<= x 78.75))   (display "ONO") 
  (if (and (> x 78.75) (<= x 101.25))  (display "O")
  (if (and (> x 101.25) (<= x 123.75)) (display  "OSO") 
  (if (and (> x 123.75) (<= x 146.25)) (display  "SO") 
  (if (and (> x 146.25) (<= x 168.75))  (display "SSO") 
  (if (and (> x 168.75) (<= x 191.25))  (display "S") 
  (if (and (> x 191.25) (<= x 213.75))  (display "SSW") 
  (if (and (> x 213.75) (<= x 236.25))  (display "SW") 
  (if (and (> x 236.25) (<= x 258.75))  (display "WSW") 
  (if (and (> x 258.75) (<= x 281.25))  (display "W") 
  (if (and (> x 281.25) (<= x 303.75))  (display "WNW") 
  (if (and (> x 303.75) (<= x 326.25))  (display "NW") 
  (if (and (> x 326.25) (<= x 348.75))  (display "NNW") (display "Bitte gültigen Grad angeben.")
)))))))))))))))))

; 2.3.2 HimmelsrichtungToGrad

(define (HimmelsrichtungToGrad x) 
  (if (equal? "N" x) 0
  (if (equal? "NNO" x) 22.5
  (if (equal? "NO" x) 45
  (if (equal? "ONO" x) 67.5
  (if (equal? "O" x) 90
  (if (equal? "OSO" x) 122.5
  (if (equal? "SO" x) 135
  (if (equal? "SSO" x) 157.5
  (if (equal? "S" x) 180
  (if (equal? "SSW" x) 202.5
  (if (equal? "SW" x) 225
  (if (equal? "WSW" x) 247.5
  (if (equal? "W" x) 270
  (if (equal? "WNW" x) 292.5
  (if (equal? "NW" x) 315
  (if (equal? "NNW" x) 337.5
  (if (equal? "N" x) 360 (display "Bitte gültige Himmelsrichtung angeben.")
      ))))))))))))))))))
