#lang racket

; ==============================================================================
; Übungen zu Softwareentwicklung III,
; Funktionale Programmierung
; Blatt 1, Woche 2
; ==============================================================================

; 1 Konversionsfunktionen
; 1.1 Bogenmaß und Grad

; Degrees → Radians
(define (degrees_to_radians alpha) (* (/ (* 2 pi) 360) alpha))

; gerundet
(define CONVERSIONFACTORTBOGENMASS 0.0174533)
(define (GradToBogenmass Grad)
 (* Grad CONVERSIONFACTORTBOGENMASS))

; Radians → Degrees 
(define (radians_to_degrees alpha) (* (/ 360 (* 2 pi)) alpha))

; gerundet
(define CONVERSIONFACTORTGRAD 57.2958)
(define (BogenmassTOGrad Bogenmass)
 (* Bogenmass CONVERSIONFACTORTGRAD))

; 1.2 Umkehrfunktion acos
(define (asin Winkel)
         (sin Winkel))

; sin²(α) + cos²(α) = 1
; sin(α) = sqrt(1 - cos²(α))
; tan(α) = sin(α) / cos²(α)
; tan(α) = (sqrt(1 - cos²(α))) / cos²(α)
; tan²(α) = (1 - cos²(α)) / cos²(α)
; tan²(α) = (1 / cos²(α)) - 1
; (tan²(α) + 1) * cos²(α) = 1
; cos²(α) = 1 / (tan²(α) + 1)
; cos(α) = sqrt(1 / (tan²(α) + 1))
(define (my-cos x) (sqrt (/ 1 (+ (expt (tan x) 2) 1))))
; arcos
(define (acos Winkel)   ; die acos Funktion gibt es schon, ich bin mir nicht sicher wie das mit Redefinition funktioniert.
         (cos Winkel))  ; TODO 

(define (atan Winkel)
         (/(asin Winkel) (acos Winkel)))

; 1.3 Kilometer und Seemeilen
(define (SeemeileToKilometer Kilometer)
  (* Kilometer 1.852))

; -------------------------------------------------------------------------------------
; 2 Großkreisentfernung und Kurse
; -------------------------------------------------------------------------------------
; 2.1 Großkreisentfernung
; cos dG = sin ’A ∗ sin ’B + cos ’A ∗ cos ’B ∗ cos ∆λ

(define MEANRADIUS 6.371)
; latA: Latitude for Point A
; latB: Latitude for Point B

(define (gLaengeInBogenmass lonB lonA)
         (GradToBogenmass (- lonB lonA)))


(define (distanzAB phiA phiB lambdaA lambdaB)
  (nm_to_km (* (radians (acos (+ (* (sin phiA) (sin phiB)) (* (my-cos phiA) (my-cos phiB) (my-cos (abs (- lambdaA lambdaB))))))) 60)))
; in beiden ist noch ein Fehler drin. 
; (define (distanzAB latA latB lonA lonB) (+  (* (sin (GradToBogenmass latA)) (sin (GradToBogenmass latB))) (* (cos (GradToBogenmass latA)) (cos (GradToBogenmass latB)) (cos (GradToBogenmass (- lonB lonA))))))


; von Oslo (59:93N, 10:75E) nach Hongkong (22:20N,114:10E)
(write "distance from Oslo to Hongkon: ")
(distanzAB 59.93 22.20 10.75 114.10) ; 8.577,64 km
                                    
; von San Francisco(37:75N, 122:45W) nach Honolulu (21:32N, 2157:83W)
(write "distance from San Francisco to Honolulu ")
(distanzAB 37.75 21.32 122.45 2157.83) ; 3.853,74 km

; von der Osterinsel (27:10S, 109:40W) nach Lima (12:10S, 77:05W)
(write "distance from Osterinsel to Lima: ")
(distanzAB 27.10 12.10 109.40 77.05) ; 3.762,44 km

; 2.2 Anfangskurs
(define (Anfangskurs latA latB lonA lonB) (/ (- (sin latB) (* (cos(distanzAB latA latB lonA lonB)) (sin latA))) (* (cos latA) (sin (distanzAB latA latB lonA lonB)))))
(define (AnfangskursAngle anfangsAngle latA latB lonA lonB) (acos ((BogenmassTOGrad (Anfangskurs latA latB lonA lonB)))) (if (and ((AnfangskursAngle anfangsAngle) > 0 ) (AnfangskursAngle anfangsAngle) < 180 ) anfangsAngle (- (360 anfangsAngle))))



