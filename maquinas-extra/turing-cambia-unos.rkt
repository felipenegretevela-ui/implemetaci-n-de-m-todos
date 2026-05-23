#lang racket

;----------------------------------------------------------
; Maquina de Turing
; Cambia todos los simbolos 1 por X
;----------------------------------------------------------

;-----------PRUEBAS-------------------
(define pruebas
  '("1" "11" "111" "" "1111"))

;-----------CONSTANTES-------------------
(define blanco "B")
(define start-state "q0")
(define accept-state "qf")

;-----------TRANSICIONES-------------------
(define turing
  (hash
   "q0" (hash
         "1" (list "q0" "X" "R")
         "B" (list "qf" "B" "S"))))

;-----------FUNCIONES DE CINTA-------------------
(define (string->tape cadena)
  (append
   (map string (string->list cadena))
   (list blanco)))

(define (get-symbol tape pos)
  (cond
    [(or (< pos 0)
         (>= pos (length tape)))
     blanco]
    [else
     (list-ref tape pos)]))

(define (replace-at lista pos valor)
  (cond
    [(empty? lista) '()]
    [(= pos 0)
     (cons valor (cdr lista))]
    [else
     (cons (car lista)
           (replace-at (cdr lista) (- pos 1) valor))]))

(define (move-head pos movimiento)
  (cond
    [(equal? movimiento "R") (+ pos 1)]
    [(equal? movimiento "L") (- pos 1)]
    [else pos]))

;-----------OBTENER TRANSICION-------------------
(define (get-transition auto estado simbolo)
  (cond
    [(not (hash-has-key? auto estado)) #f]
    [else
     (let ([transiciones-estado (hash-ref auto estado)])
       (cond
         [(hash-has-key? transiciones-estado simbolo)
          (hash-ref transiciones-estado simbolo)]
         [else #f]))]))

;-----------SIMULAR TURING-------------------
(define (validate-turing cadena)

  (define tape-inicial
    (string->tape cadena))

  (define (procesar estado tape cabeza)
    (cond
      [(equal? estado accept-state)
       tape]

      [else
       (let* ([simbolo (get-symbol tape cabeza)]
              [transition (get-transition turing estado simbolo)])
         (cond
           [transition
            (let* ([nuevo-estado (first transition)]
                   [simbolo-escrito (second transition)]
                   [movimiento (third transition)]
                   [nueva-tape (replace-at tape cabeza simbolo-escrito)]
                   [nueva-cabeza (move-head cabeza movimiento)])
              (procesar nuevo-estado nueva-tape nueva-cabeza))]
           [else tape]))]))

  (procesar start-state tape-inicial 0))

;-----------LIMPIAR CINTA-------------------
(define (limpiar-blanco tape)
  (filter
   (lambda (x)
     (not (equal? x blanco)))
   tape))

;-----------MOSTRAR RESULTADOS-------------------
(define (mostrar-resultado cadena)
  (let* ([resultado-tape (validate-turing cadena)]
         [resultado-limpio (limpiar-blanco resultado-tape)]
         [resultado-texto (apply string-append resultado-limpio)])
    (displayln
     (string-append cadena " -> " resultado-texto))))

;-----------EJECUCION-------------------
(displayln "Maquina de Turing: cambia todos los 1 por X")
(for-each mostrar-resultado pruebas)