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

