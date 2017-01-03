#lang racket
(require swindle/extra)
(require se3-bib/setkarten-module)

; 1 Funktionen höherer Ordnung und Closures

; 1.1
; Eine Funktion ist eine Funktion höherer Ordnung, wenn sie Funktionen als Parameter oder Rückgabewert hat.

; 1.2
; (a) ja, da der erste Parameter von foldr eine Funktion sein muss. 
; (b) nein, da der einziger Parameter x einen numerischen Wert hat.
; (c) ja, da der Rückgabewert eine Funktion ist und der Parameter f eine Funktion sein muss. 
; (d) ja, da der Rückgabewert eine Funktion ist und der Parameter f zusätzlich eine Funktion sein muss.

; 1.3
(define (masala f a1) (λ (a2) (f a1 a2)))
((masala / 1) 3)
; ((masala / 1) 3) → (#<procedure> 3) → (‌/ 1 3) → #e0.3
; f := / ; a1 := 1 ; a2 := 3
; Eine Closure hat eine Funktion als Rückgabewert.

; 1.4
(foldl (curry * 3) 1 '(1 2 3))
; (foldl (curry * 3) 1 '(1 2 3)) → (foldl (lambda (x) (* 3 x)) 1 '(1 2 3)) → (foldl (lambda (x) (* 3 x)) (* 1 (* 3 1)) '(2 3))
; → (foldl (lambda (x) (* 3 x)) (* 1 (* 3 1) (* 3 2)) '(3)) → (* 1 (* 3 1) (* 3 2) (* 3 3)) → 162
(define (flip f) (λ (x y) (f y x)))
(map (flip cons) '(1 2 3) '(3 2 1)) 
; (map (flip cons) '(1 2 3) '(3 2 1)) → (map (lambda (x y) (cons y x)) '(1 2 3) '(3 2 1))
; → (map (lambda (1 3) (cons 3 1)) '(2 3) '(2 1)) → (cons (lambda (1 3) (cons 3 1)) (lambda (2 2) (cons 2 2)) (lambda (3 1) (cons 1 3)))
; → (cons (cons 3 1) (cons (cons 2 2) (cons 1 3))) → '((3 . 1) (2 . 2) (1 . 3))
(filter list? '((a b ) () 1 (())))
; (filter list? '((a b ) () 1 (()))) → (cons '(a b) (filter list? '(() 1 (())))) → (cons '(a b) (cons '() (filter list? '(1 (())))))
; → (cons '(a b) (cons '() (filter list? '((()))))) → (cons '(a b) (cons '() '((())))) → '((a b) () (()))
(map (compose (curryr / 1.8) (curry - 32)) '(9941 212 32 -459.67))
; (map (compose (curryr / 1.8) (curry - 32)) '(9941 212 32 -459.67))

;;; 2 Einfache funktionale Ausdrücke höherer Ordnung
(define xs '(-27 -26 -25 -14 -13 -12 -2 -1 0 1 2 12 13 14 25 26 27))
; 2.1
(map (λ (x) (abs x)) xs)
; 2.2
(filter (λ (x) (= (modulo x 13) 0)) xs)
; 2.3
(foldr + 0 (filter (λ (x) (and (even? x) (> x 3))) xs))
; 2.4
(define (split l f)
  (cons (filter (λ (x)      (f x))  l)
  (list (filter (λ (x) (not (f x))) l))))
(split xs odd?)

;;; 3 Spieltheorie: Das Kartenspiel SET

; 3.1 
(struct spielkarte (form farbe anzahl-symbole fullung))

(spielkarte 'oval 'red '1 'outline)

(define symbols '(oval rectangle waves))
(define color '(red blue green))
(define anzahl '(1 2 3))
(define fullungen '(outline solid hatched))

; 3.2
(define (erzeuge-kartenstapel)
  (erzeuge-kartenstapel-help symbols color anzahl fullungen))

(define (erzeuge-kartenstapel-help s c a f)
  (amb-collect 
   (let ((x1 (amb (car s) (cadr s) (caddr s)))
         (x2 (amb (car c) (cadr c) (caddr c)))
         (x3 (amb (car a) (cadr a) (caddr a)))
         (x4 (amb (car f) (cadr f) (caddr f))))
     (amb-assert (spielkarte x1 x2 x3 x4))
     (spielkarte x1 x2 x3 x4))))

(define (show-cards cards)
  (map
   (λ (card)
     (show-set-card
      (spielkarte-anzahl-symbole card)
      (spielkarte-form card)
      (spielkarte-fullung card)
      (spielkarte-farbe card)))
   cards))

(show-cards (erzeuge-kartenstapel))

; 3.3

(define (equal-list? l)
  (let ((first (car l)))
    (= (length (filter (λ (x) (equal? x first)) l)) (length l))))

(define (disjoint-list? l)
  (= (length (set->list (list->set l))) (length l)))

(define (is-a-set? a b c)
  (let ((count (map (λ (x) (spielkarte-anzahl-symbole x)) (list a b c)))
        (forms (map (λ (x) (spielkarte-form x))           (list a b c)))
        (color (map (λ (x) (spielkarte-farbe x))          (list a b c)))
        (shade (map (λ (x) (spielkarte-fullung x))        (list a b c))))
    (and (or (equal-list? count) (disjoint-list? count))
         (or (equal-list? forms) (disjoint-list? forms))
         (or (equal-list? color) (disjoint-list? color))
         (or (equal-list? shade) (disjoint-list? shade)))))

(is-a-set? (spielkarte 'oval 'red '2 'hatched)
           (spielkarte 'rectangle 'red '2 'hatched)
           (spielkarte 'wave 'red '2 'hatched))
(is-a-set? (spielkarte 'rectangle 'red '2 'outline)
           (spielkarte 'rectangle 'green '2 'outline)
           (spielkarte 'rectangle 'green '1 'solid))

; 3.4

(define (ziehe-zwolf-karten stapel)
  (if (< (length stapel) 12) #f
      (ziehe-zwolf-karten-help stapel 0)))
(define (ziehe-zwolf-karten-help stapel gezogen)
  (let ((neuer-stapel (shuffle stapel)))
    (if (= gezogen 12)
        '()
        (cons (car neuer-stapel) (ziehe-zwolf-karten-help (cdr neuer-stapel) (+ gezogen 1))))))

(define aktuelles-blatt (ziehe-zwolf-karten (erzeuge-kartenstapel)))
(show-cards aktuelles-blatt)

(define (finde-alle-sets cards)
  (map (λ (s)
         (let ((l (set->list s)))
           (show-cards (map (λ (i) (list-ref cards i)) l))))
       (set->list (list->set (finde-alle-sets-help cards)))))
(define (finde-alle-sets-help cards)
  (amb-collect 
   (let ((a (amb 0 1 2 3 4 5 6 7 8 9 10 11))
         (b (amb 0 1 2 3 4 5 6 7 8 9 10 11))
         (c (amb 0 1 2 3 4 5 6 7 8 9 10 11))
         (known (set)))
     (amb-assert
      (and
       (disjoint-list? (list a b c))
       (is-a-set? (list-ref cards a)
                 (list-ref cards b)
                 (list-ref cards c))))
     (list->set (list a b c)))))

(finde-alle-sets aktuelles-blatt)




