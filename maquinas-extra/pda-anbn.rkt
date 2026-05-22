#lang racket

; PDA para reconocer a^n b^n
; Acepta cadenas con la misma cantidad de a's seguidas de b's

(define pruebas
  '("ab" "aabb" "aaabbb" "aab" "abb" "ba" "" "aaabb"))

(define (push pila simbolo)
  (cons simbolo pila))

(define (pop pila)
  (if (empty? pila)
      '()
      (cdr pila)))

(define (top pila)
  (if (empty? pila)
      ""
      (car pila)))

(define (simular-pda cadena)
  (define chars (map string (string->list cadena)))

  (define (procesar lista estado pila)
    (cond
      [(empty? lista)
       (and (equal? estado "q1")
            (equal? pila '("Z")))]

      [else
       (define simbolo (car lista))
       (define resto (cdr lista))

       (cond
         [(and (equal? estado "q0")
               (equal? simbolo "a"))
          (procesar resto "q0" (push pila "A"))]

         [(and (equal? estado "q0")
               (equal? simbolo "b")
               (equal? (top pila) "A"))
          (procesar resto "q1" (pop pila))]

         [(and (equal? estado "q1")
               (equal? simbolo "b")
               (equal? (top pila) "A"))
          (procesar resto "q1" (pop pila))]

         [else #f])]))

  (procesar chars "q0" '("Z")))

(define (mostrar-resultado cadena)
  (displayln
   (string-append
    cadena
    " -> "
    (if (simular-pda cadena)
        "accepted"
        "rejected"))))

(displayln "PDA: reconoce a^n b^n")
(for-each mostrar-resultado pruebas)