#lang racket
;;; Blatt 3

;;; 1 Die internationale Buchstabiertafel
(displayln "1 Die internationale Buchstabiertafel")

;; 1.1 Eine Datenstruktur für die internationale Buchstabiertafel
(displayln "1.1 Eine Datenstruktur für die internationale Buchstabiertafel")

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
; Haben uns für die Datenstruktur Map entschieden, weil wir es erst kürzlich in der Vorlesung gehört haben und es einmal Anwenden wollten.
; Je nachdem welcher char als Eingabeschlüssel kommt, wir er in eine Zeichenkette als Wert übersetzt.
; Wenn jedoch ein Schlüssel gewählt wird, der nicht in der List vorhanden ist, wird ein Fehler geworfen.

;; 1.2 Codierfunktion
(displayln "1.2 Codierfunktion")

(define (codiere-buchstabentafel buchstabe) 
  (if (char? buchstabe) ; Der Definitionsbereich liegt nur bei Zeichen. 
      (cdr (assoc buchstabe buchstabiertafel)) ; Lokalisiere das Element mithilfe des Schlüssels buchstabe und gebe dann den Wert zurück.
      buchstabe)) ; sonst wird der nicht definierte Wert zurückgegeben.

;; 1.3 Zusatzaufgabe: Codierfunktion
(displayln "1.3 Zusatzaufgabe: Codierfunktion")

(define (codiere-zusatz buchstabe)
  (codiere buchstabe buchstabiertafel)) ; Kodiere den Buchstaben mithilfe eine Hilfsfunktion und der Buchstabentafel.

(define (codiere buchstabe tafel)
  (if (char? buchstabe) ; Reduziere den Definitionsbereich auf Zeichen
      (cdr (assoc ; Lokalisiere das Element und gebe dann den Wert zurück.
            (klein->gross buchstabe) ; Der Schlüssel, der zuerst von Kleinbuchstaben zu Großbuchstaben umgewandelt wird.
            tafel)) ; Die Map in der gesucht wird.
      buchstabe)) ; Gebe sonst den undefinierten Wert zurück

; require char
(define (klein->gross buchstabe)
  (let ((intval (char->integer buchstabe))) ; definiere intval als ascii integerwert vom Zeichen 
    (if (and (>= intval 97) (<= intval 122)) ; prüfe ob dieser Wert im bereich der Kleinbuchstaben liegt 
        (integer->char (- intval 32)) ; wenn ja wandle sie in Großbuchstaben um
        buchstabe))) ; sonst gebe das Zeichen zurück, welches kein Kleinbuchstabe ist

(displayln (codiere-zusatz #\A))
(displayln (codiere-zusatz #\T))
(displayln (codiere-zusatz #\Y))
(displayln (klein->gross #\a))
(displayln (klein->gross #\t))
(displayln (klein->gross #\y))

;; 1.4 Buchstabieren eines Textes
(displayln "1.4 Buchstabieren eines Textes")

(define (buchstabiere text)
  (if (string? text) ; Setze den Definitionsbereich auf Zeichenketten
      (let ((erster-buchstabe-nachgeschaut (codiere-zusatz (car (string->list text))))) ; definiere erster-buchstabe-nachgeschaut als die Übersetzung des ersten Buchstaben.
      (if (> (string-length text) 1) ; Schaue ob der Text mehr als ein Zeichen enthält 
          (string-append ; Wenn ja verbinde die Folgenden Zeichenketten
           erster-buchstabe-nachgeschaut ; erster-buchstabe-nachgeschaut
           " " ; einen leerstring zur Veranschaulichung 
           (buchstabiere (substring text 1))) ; rekursiv die restlichen Zeichen bis auf das erste
          erster-buchstabe-nachgeschaut)) ; Sonst wenn der Text nurnoch ein zeichen lang ist, ist das erste Zeichen auch das letzte Zeichen
      text)) ; Sonst gebe den nicht definierten Wert zurück

(displayln (buchstabiere "Ahmed Hassan,"))
(displayln (buchstabiere "Tim Kilian,"))
(displayln (buchstabiere "Yuki Waisho."))

;;; 2 Das internationale Flaggenalphabet
(displayln "2 Das internationale Flaggenalphabet")
(require se3-bib/flaggen-module)

(list R A C K E T)

;; 2.1 Eine Datenstruktur für das Flaggenalphabet
(displayln "2.1 Eine Datenstruktur für das Flaggenalphabet")

(define flaggentafel (list
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
; Wie oben, um assoziative arrays zu üben auch die Datenstruktur map gewählt
; Wir bilden jedes Zeichen für Großbuchstaben auf die jeweiligen Flaggen ab.

;; 2.2 Eine Codierfunktion
(displayln "2.2 Eine Codierfunktion")
(define (codiere-flagge buchstabe)
  (eval (codiere buchstabe flaggentafel))) ; Nutze zum codieren die Hilfsfunktion aus 1.3. Da in der Tafel Symbole rauskommen und wir die jeweiligen Flaggen haben wollen, müssen wir sie noch berechnen

'(codiere-flagge #\A) ; Testeingaben in der Konsole eingeben
'(codiere-flagge #\T)
'(codiere-flagge #\Y)

;; 2.3 Buchstabieren eines Textes
(displayln "2.3 Buchstabieren eines Textes")

(define (flaggiere text)
  (if (string? text) ; Setze den Definitionsbereich auf Zeichenketten 
      (let ((erster-buchstabe-nachgeschaut (codiere-flagge (car (string->list text))))) ; wie oben, übersetze das erste Zeichen in eine Flagge
      (if (> (string-length text) 1) ; Rekursionsabbruch: schaue ob noch mehr als ein Zeichen übrig ist
          (append ; da wir mit flaggen arbeiten können wir nicht wie in 1.4 eine Zeichenkette bilden, sondern bauen uns ein array wo wir nacheinander die Flaggen anhängen
           (list erster-buchstabe-nachgeschaut) ; das erste Zeichen als Flagge
           (flaggiere (substring text 1))) ; rekursiver Aufruf: flaggiere den Rest der Zeichenkette
          (list erster-buchstabe-nachgeschaut))) ; geben das letzte Zeichen zurück, da nur eins da ist 
      text)) ; sonst gib den nicht definierten Wert zurück.

'(flaggiere "Ahmed Hassan")
'(flaggiere "Tim Kilian")
'(flaggiere "Yuki Waisho")