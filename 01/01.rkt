#lang racket

; ==============================================================================
; Übungen zu Softwareentwicklung III,
; Funktionale Programmierung
; Blatt 1, Woche 2
; ==============================================================================

; 1 Konversionsfunktionen
; 1.1 Bogenmaß und Grad

(define CONVERSIONFACTORTBOGENMASS 0.0174533)
(define (GradToBogenmass Grad)
 (* Grad CONVERSIONFACTORTBOGENMASS))

(define CONVERSIONFACTORTGRAD 57.2958)
(define (BogenmassTOGrad Bogenmass)
 (* Bogenmass CONVERSIONFACTORTGRAD))

; 1.2 Umkehrfunktion acos
(define (asin Winkel)
         (sin Winkel))

(define (acos Winkel)
         (cos Winkel))

(define (atan Winkel)
         (/(asin Winkel) (acos Winkel)))

; 1.3 Kilometer und Seemeilen
(define (KilometerToSeemeilen Kilometer)
  (* Kilometer 1.852))

; -------------------------------------------------------------------------------------
; 2 Großkreisentfernung und Kurse
; 2.1 Großkreisentfernung
; cos dG = sin ’A ∗ sin ’B + cos ’A ∗ cos ’B ∗ cos ∆λ

(define distanzEquation (define (distanzABEquation A B gLaenge)
  (+ (* (sin A) (sin B)) (* (cos A) (cos B) (cos gLaenge)))))

(define (distanzAB distanzEquation)
  (* distanzEquation 60))

; von Oslo (59:93oN, 10:75oE) nach Hongkong (22:20oN,114:10oE)
(distanzEquation (distanzABEquation 59.93
                                    
; von San Francisco(37:75oN, 122:45oW) nach Honolulu (21:32oN, 2157:83oW)
                                    
; von der Osterinsel (27:10oS, 109:40oW) nach Lima (12:10oS, 77:05oW)
                                    
