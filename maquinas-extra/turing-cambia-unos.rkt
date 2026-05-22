#lang racket

; Máquina de Turing sencilla
; Cambia todos los 1 por X hasta encontrar blanco B

(define pruebas
  '("1" "11" "111" "" "1111"))

(define blanco "B")

(define (string->tape cadena)
  (append (map string (string->list cadena)) (list blanco)))

(define (tape-ref-safe tape pos)
  (if (or (< pos 0) (>= pos (length tape)))
      blanco
      (list-ref tape pos)))

(define (replace-at lista pos valor)
  (cond
    [(empty? lista) '()]
    [(= pos 0) (cons valor (cdr lista))]
    [else
     (cons (car lista)
           (replace-at (cdr lista) (- pos 1) valor))]))

(define (simular-turing cadena)
  (define tape-inicial (string->tape cadena))

  (define (procesar estado tape cabeza)
    (define simbolo (tape-ref-safe tape cabeza))

    (cond
      [(equal? estado "qf")
       tape]

      [(and (equal? estado "q0")
            (equal? simbolo "1"))
       (procesar "q0"
                 (replace-at tape cabeza "X")
                 (+ cabeza 1))]

      [(and (equal? estado "q0")
            (equal? simbolo blanco))
       (procesar "qf" tape cabeza)]

      [else tape]))

  (procesar "q0" tape-inicial 0))

(define (limpiar-blanco tape)
  (filter
   (lambda (x)
     (not (equal? x blanco)))
   tape))

(define (mostrar-resultado cadena)
  (define resultado (limpiar-blanco (simular-turing cadena)))
  (displayln
   (string-append
    cadena
    " -> "
    (apply string-append resultado))))

(displayln "Máquina de Turing: cambia todos los 1 por X")
(for-each mostrar-resultado pruebas)