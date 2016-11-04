#lang racket
;; Blatt 2
;; Aufgabe 1:
(define miau 'Plueschi)
(define katze miau)
(define tiger 'miau)

(define (welcherNameGiltWo PersonA PersonB)
  (let ((PersonA 'Sam)
        (PersonC PersonA))
    PersonC))

(define xs1 '(0 2 3 miau katze))
(define xs2 (list miau katze))
(define xs3 (cons katze miau))

;miau
; evaluiert zu 'Plueschi, da die Funktion miau aufgerufen wird.

;katze
; evaluiert ebenfals zu 'Plueschi, da die Funktion katze wiederum die Funktion miau aufruft.

;tiger 
; evaluert zu 'miau, da wie in 1. die Funktion miau aufgerufen wird.

;(quote katze) 
; gibt das Symbol 'katze zurück, weil das die Konvention ist.

;(eval tiger)
; scheitert
;(eval katze)
; scheitert
;(eval 'tiger)
; scheitert

;(welcherNameGiltWo 'harry 'potter) 
; gibt 'harry zurück. 2. Parameter wird in der aufgerufenen Funktion gar nicht verwendet.
; Zeigt, dass let mit den Werten der Variablen aufgerufen wird, nicht mit Referenzen auf
; Variablen. Denn sonst würde 'Sam der Rückgabewert sein.

;(cdddr xs1)
; Resultiert in der der Liste '(miau katze), bzw dem Rest vom Rest vom Rest der Liste xs1

;(cdr xs2)
; Resultiert in '(Plueschi), also der Restliste. Da das ursprüngliche Element eine Funktion
; war, wird diese ausgewertet und der Rückgabewert in der Liste verwendet.

;(cdr xs3)
; Resultiert in 'Plueschi, als Symbol, nicht Liste, da der zweite Teil des Paares zurückgegeben wurde.

;(eval (sqrt 3))
; Schlägt fehl. Vermutlich wird zunächst die Wurzel von 3 berechnet und anschließend versucht
; das Resultat auszuwerten. "no #%datum syntax transformer is bound in: 1.7320508075688772"


;(eval '(welcherNameGiltWo 'tiger 'katze))
; scheitert
;(eval (welcherNameGiltWo 'tiger 'katze))
;scheitert

; Aufgabe 2:
; 2.1:
(define (fak n) ; Funktionsdefinition
  (if (= n 0)   ; Rekursionsbedingung
      1         ; Rekursionsschluss
      (* n (fak (- n 1))) ; Rekursionsschritt
  ))
