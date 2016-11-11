#lang racket
;;; Blatt 3

;;; 1 Die internationale Buchstabiertafel
(displayln "1 Die internationale Buchstabiertafel")

(define buchstabiertafel (list
                          '(#\space . " ")
                          '(#\A . "Alfa")
                          '(#\B . "Bravo")
                          '(#\C . "Charlie")
                          '(#\D . "Delta")
                          '(#\E . "Echo")
                          '(#\F . "Foxtrott")
                          '(#\G . "Golf")
                          '(#\H . "Hotel")
                          '(#\I . "India")
                          '(#\J . "Juliett")
                          '(#\K . "Kilo")
                          '(#\L . "Lime")
                          '(#\M . "Mike")
                          '(#\N . "November")
                          '(#\O . "Oscar")
                          '(#\P . "Papa")
                          '(#\Q . "Quebec")
                          '(#\R . "Romeo")
                          '(#\S . "Sierra")
                          '(#\T . "Tango")
                          '(#\U . "Uniform")
                          '(#\V . "Viktor")
                          '(#\W . "Whiskey")
                          '(#\X . "X-ray")
                          '(#\Y . "Yankee")
                          '(#\Z . "Zulu")
                          '(#\0 . "Nadazero")
                          '(#\1 . "Unaone")
                          '(#\2 . "Bissotwo")
                          '(#\3 . "Terrathree")
                          '(#\4 . "Kartefour")
                          '(#\5 . "Pantafive")
                          '(#\6 . "Soxisix")
                          '(#\7 . "Setteseven")
                          '(#\8 . "Oktoeight")
                          '(#\9 . "Novenine")
                          '(#\, . "Decimal")
                          '(#\. . "Stop")))

;; 1.2 Codierfunktion
(define (codiere-buchstabentafel buchstabe)
  (if (char? buchstabe)
      (cdr (assoc buchstabe buchstabiertafel))
      buchstabe))

;; 1.3 Zusatzaufgabe: Codierfunktion
(define (codiere-zusatz buchstabe)
  (codiere buchstabe buchstabiertafel))

(define (codiere buchstabe tafel)
  (if (char? buchstabe)
      (cdr (assoc
            (klein->gross buchstabe)
            tafel))
      buchstabe))

; require char
(define (klein->gross buchstabe)
  (let ((intval (char->integer buchstabe)))
    (if (and (>= intval 97) (<= intval 122))
        (integer->char (- intval 32))
        buchstabe)))

;; 1.4 Buchstabieren eines Textes
(define (buchstabiere text)
  (if (string? text)
      (let ((erster-buchstabe-nachgeschaut (codiere-zusatz (car (string->list text)))))
      (if (> (string-length text) 1)
          (string-append
           erster-buchstabe-nachgeschaut
           " "
           (buchstabiere (substring text 1)))
          erster-buchstabe-nachgeschaut))
      text))

;;; 2 Das internationale Flaggenalphabet
(require se3-bib/flaggen-module)

(list R A C K E T)

(define flagentafel (list
                          '(#\A . A)
                          '(#\B . B)
                          '(#\C . C)
                          '(#\D . D)
                          '(#\E . E)
                          '(#\F . F)
                          '(#\G . G)
                          '(#\H . H)
                          '(#\I . I)
                          '(#\J . J)
                          '(#\K . K)
                          '(#\L . L)
                          '(#\M . M)
                          '(#\N . N)
                          '(#\O . O)
                          '(#\P . P)
                          '(#\Q . Q)
                          '(#\R . R)
                          '(#\S . S)
                          '(#\T . T)
                          '(#\U . U)
                          '(#\V . V)
                          '(#\W . W)
                          '(#\X . X)
                          '(#\Y . Y)
                          '(#\Z . Z)))

;; 2.2 Eine Codierfunktion
(define (codiere-flagge buchstabe)
  (eval (codiere buchstabe flagentafel)))

;; 2.3 Buchstabieren eines Textes

(define (flaggiere text)
  (if (string? text)
      (let ((erster-buchstabe-nachgeschaut (codiere-flagge (car (string->list text)))))
      (if (> (string-length text) 1)
          (string-append
           "TODO: Flaggen Append"
           (flaggiere (substring text 1)))
          erster-buchstabe-nachgeschaut))
      text))