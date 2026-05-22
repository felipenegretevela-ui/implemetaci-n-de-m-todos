#lang racket

; AFD para reconocer cadenas binarias con número impar de 0's
; y número impar de 1's

(define pruebas
  '("01" "10" "0011" "0101" "0" "1" "00" "11" "" "101"))

(define (string->chars cadena)
  (map string (string->list cadena)))

(define (siguiente-estado estado simbolo)
  (cond
    [(and (equal? estado "q00") (equal? simbolo "0")) "q10"]
    [(and (equal? estado "q00") (equal? simbolo "1")) "q01"]

    [(and (equal? estado "q10") (equal? simbolo "0")) "q00"]
    [(and (equal? estado "q10") (equal? simbolo "1")) "q11"]

    [(and (equal? estado "q01") (equal? simbolo "0")) "q11"]
    [(and (equal? estado "q01") (equal? simbolo "1")) "q00"]

    [(and (equal? estado "q11") (equal? simbolo "0")) "q01"]
    [(and (equal? estado "q11") (equal? simbolo "1")) "q10"]

    [else "error"]))

(define (simular-afd cadena)
  (define chars (string->chars cadena))

  (define (procesar lista estado)
    (cond
      [(empty? lista)
       (equal? estado "q11")]

      [else
       (define simbolo (car lista))
       (define nuevo-estado (siguiente-estado estado simbolo))

       (if (equal? nuevo-estado "error")
           #f
           (procesar (cdr lista) nuevo-estado))]))

  (procesar chars "q00"))

(define (mostrar-resultado cadena)
  (displayln
   (string-append
    cadena
    " -> "
    (if (simular-afd cadena)
        "accepted"
        "rejected"))))

(displayln "AFD: reconoce cadenas con numero impar de 0's y numero impar de 1's")
(for-each mostrar-resultado pruebas)