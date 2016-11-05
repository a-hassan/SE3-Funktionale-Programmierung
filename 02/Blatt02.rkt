#lang racket
;; Blatt 2

;;; Aufgabe 1 Symbole und Werte, Umgebungen
(displayln "1 Symbole und Werte, Umgebungen\n")

; Gegeben:
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

; 1. miau
miau ; 'Plueschi, da miau → 'Plueschi
; Evaluiert zu 'Plueschi, da die Funktion miau aufgerufen wird.
; 2. katze
katze ; 'Plueschi, da katze → miau → 'Plueschi
; Evaluiert ebenfals zu 'Plueschi, da die Funktion katze wiederum die Funktion miau aufruft.
; 3. tiger
tiger ; 'miau, da tiger → 'miau
; Evaluert zu 'miau, da wie in 1. die Funktion miau aufgerufen wird.
; 4. (quote katze)
(quote katze) ; 'katze, da (quote katze) → 'katze
; Gibt das Symbol 'katze zurück, weil das die Konvention ist.
; 5. (eval tiger)
;(eval tiger) ; 'Plueschi, da (eval tiger) → (eval 'miau) → (miau) → ⊥
; Scheitert.
; 6. (eval katze)
;(eval katze) ; ⊥, da (eval katze) → (eval miau) → (eval 'Plueschi) → (Plueschi) → ⊥ 
; Scheitert.
; 7. (eval 'tiger)
;(eval 'tiger) ; 'miau, da (eval 'tiger) → tiger → 'miau 
; Scheitert.
; 8. (welcherNameGiltWo 'harry 'potter)
(welcherNameGiltWo 'harry 'potter) ; 'harry, da (welcherNameGiltWo 'harry 'potter) → (let ((PersonA 'Sam)(PersonC 'harry)) PersonC) → 'harry
; Gibt 'harry zurück. 2. Parameter wird in der aufgerufenen Funktion gar nicht verwendet.
; Zeigt, dass let mit den Werten der Variablen aufgerufen wird, nicht mit Referenzen auf
; Variablen. Denn sonst würde 'Sam der Rückgabewert sein.
; 9. (cdddr xs1)
(cdddr xs1) ; '(miau katze), da (cdddr xs1) → (cdddr '(0 2 3 miau katze)) → (cddr '(2 3 miau katze)) → (cdr '(3 miau katze)) → '(miau katze)
; Resultiert in der der Liste '(miau katze), bzw dem Rest vom Rest vom Rest der Liste xs1.
; 10. (cdr xs2)
(cdr xs2) ; '(Plueschi), da (cdr xs2) → (cdr (list miau katze)) → (cdr '(miau katze)) → (katze) → (Plueschi)
; Resultiert in '(Plueschi), also der Restliste. Da das ursprüngliche Element eine Funktion
; war, wird diese ausgewertet und der Rückgabewert in der Liste verwendet.
; 11. (cdr xs3)
(cdr xs3) ; 'Plueschi, da (cdr xs3) → (cdr (cons katze miau)) → (cdr '(miau . katze)) → katze → 'Plueschi
; Resultiert in 'Plueschi, als Symbol, nicht Liste, da der zweite Teil des Paares zurückgegeben wurde.
; 12. (eval (sqrt 3))
;(eval (sqrt 3)) ; ⊥, (eval (sqrt 3)) → (eval 1.7320508075688772) → ⊥
; Schlägt fehl. Vermutlich wird zunächst die Wurzel von 3 berechnet und anschließend versucht
; das Resultat auszuwerten. "no #%datum syntax transformer is bound in: 1.7320508075688772"
; 13. (eval '(welcherNameGiltWo 'tiger 'katze))
;(eval '(welcherNameGiltWo 'tiger 'katze)) ; ⊥, da (eval '(welcherNameGiltWo 'tiger 'katze)) → (eval '((let ((PersonA 'Sam)(PersonC 'tiger)) PersonC))) → (eval '('tiger)) → ('tiger) → ⊥
; Scheitert.
; 14. (eval '(welcherNameGiltWo 'katze 'tiger))
;(eval '(welcherNameGiltWo 'katze 'tiger)) ; ⊥, da (eval '(welcherNameGiltWo 'katze 'tiger)) → (eval '((let ((PersonA 'Sam)(PersonC 'katze)) PersonC))) → (eval '('katze)) → ('katze) → ⊥ 
; Scheitert.


;;; Aufgabe 2 Rechnen mit exakten Zahlen
(displayln "\n2 Rechnen mit exakten Zahlen\n")

;; 2.1 Die Fakultät einer Zahl
(displayln "2.1 Die Fakultät einer Zahl")

(define (integer? n)
  (if (= n (floor n)) #t #f)) ; check if is integer; check n∈ℤ.

(define (natural? n)
  (if (>= n 0) (integer? n) #f)) ; check n∈ℕ, by checking not negativity and integer.

(define (fac n) ; Funktionsdefinition
  (if (natural? n) ; Fakultät ist nur für ein n∈ℕ definiert.
  ; Abbruchbedingung, nach definition gilt 0! = 1
      (if (= n 0) ; Rekursionsbedingung
          1 ; Rekursionsschluss
          ; Sonst, n = n·(n-1)!
          (* n (fac (- n 1)))) ; Rekursionsschritt
  n)) ; If n∉ℕ return n.

; Testeingaben
(fac 3)  ; Erwartet 6
(fac 5)  ; Erwartet 120
(fac -2) ; Erwartet -2, da -2 ∉ ℕ

;; 2.2 Potenzen und Rationalzahlen
(displayln "2.2 Potenzen und Rationalzahlen")

(define (my-odd? n)
  (rational? (if (= (modulo n 2) 0) #t #f)) n)

(define (my-even? n)
  (rational? (not my-odd? n)) n)

(define (my-sqr n)
  (rational? (* n n) n))
  
(define (power r n)
  (if (natural? n)
      (if (= n 0) 1
          (if (my-odd? n)
              (* r (power r (- n 1)))
              (my-sqr (power r (/ n 2)))))
  (if (integer? n)
      (/ 1 (power r (* n -1)))
      (list r '^ n))))

; Testeingaben
(power 2 0)
(power 2 1)
(power 2 2)
(power 2 3)
(power 2 10)
(power 2 -1)
(power 2 1.5)

;; 2.3 Die Eulerzahl ℯ
(displayln "2.3 Die Eulerzahl ℯ")

(define (eulerreihe n)
  (let ((glied (/ n (fac (- n 1)))))
    (if (> glied (/ 1 (power 10 1000)))
          (+ glied (eulerreihe (+ n 1)))
          0)))

(define e
  (* (/ (eulerreihe 1) 2) (power 10 1001)))

; Testeingaben
e

;; 2.4 π
(displayln "2.4 π")

(define (pireihe n)
  (let ((glied (/ 1 n)))
    (if (> (abs glied) (/ 1 (power 10 4)))
          (+ glied (pireihe (* (- 1) (if (> n 0) (+ n 2) (- n 2)))))
          0)))

(define pi
  (* (pireihe 1) 4))

pi

;;; Aufgabe 3 Typprädikate
(displayln "\n3 Typprädikate\n")


