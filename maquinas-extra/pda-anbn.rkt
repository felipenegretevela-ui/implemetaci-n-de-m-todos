#lang racket

;----------------------------------------------------------
; PDA - Automata con pila
; Lenguaje: a^n b^n
; Basado en el ejemplo de clase pda-anbn
;----------------------------------------------------------

;-----------PRUEBAS-------------------
(define pruebas
  '("ab" "aabb" "aaabbb" "aaaabbbb"
    "aaab" "bb" "a" "b" "aab" ""))

;-----------AUTOMATA PDA-------------------
(define pda
  (hash
   "start" (hash
            "" (hash
                "" (list "s0" '("#"))))

   "s0" (hash
         "a" (hash
              "#" (list "s0" '("a" "#"))
              "a" (list "s0" '("a" "a")))
         "b" (hash
              "a" (list "s1" '())))

   "s1" (hash
         "b" (hash
              "a" (list "s1" '()))
         "" (hash
             "#" (list "s2" '("#"))))))

(define start-state "start")
(define accept-states '("s2"))

;-----------FUNCIONES DE PILA-------------------
(define (top pila)
  (cond
    [(empty? pila) ""]
    [else (car pila)]))

(define (pop pila)
  (cond
    [(empty? pila) '()]
    [else (cdr pila)]))

(define (push-list pila lista-push)
  (append lista-push pila))

;-----------OBTENER TRANSICION-------------------
(define (get-transition auto estado simbolo cima)
  (cond
    [(not (hash-has-key? auto estado)) #f]
    [else
     (let* ([transiciones-estado (hash-ref auto estado)])
       (cond
         [(not (hash-has-key? transiciones-estado simbolo)) #f]
         [else
          (let* ([transiciones-simbolo (hash-ref transiciones-estado simbolo)])
            (cond
              [(hash-has-key? transiciones-simbolo cima)
               (hash-ref transiciones-simbolo cima)]
              [else #f]))]))]))

;-----------SIMULAR PDA-------------------
(define (validate-pda cadena)

  (define simbolos
    (map string (string->list cadena)))

  (define (procesar estado entrada pila)
    (cond
      ; Si ya no hay entrada, intenta aceptar por epsilon
      [(empty? entrada)
       (let* ([cima (top pila)]
              [epsilon-transition (get-transition pda estado "" cima)])
         (cond
           [epsilon-transition
            (let* ([nuevo-estado (first epsilon-transition)]
                   [push-chars (second epsilon-transition)]
                   [nueva-pila (push-list (pop pila) push-chars)])
              (member nuevo-estado accept-states))]
           [else
            (and (member estado accept-states)
                 (not #f))]))]

      ; Si hay entrada, consume simbolo
      [else
       (let* ([simbolo (car entrada)]
              [resto (cdr entrada)]
              [cima (top pila)]
              [transition (get-transition pda estado simbolo cima)])
         (cond
           [transition
            (let* ([nuevo-estado (first transition)]
                   [push-chars (second transition)]
                   [nueva-pila (push-list (pop pila) push-chars)])
              (procesar nuevo-estado resto nueva-pila))]
           [else #f]))]))

  (procesar start-state simbolos '()))

;-----------MOSTRAR RESULTADOS-------------------
(define (mostrar-resultado cadena)
  (displayln
   (string-append
    cadena
    " -> "
    (if (validate-pda cadena)
        "accepted"
        "rejected"))))

;-----------EJECUCION-------------------
(displayln "PDA: reconoce a^n b^n")
(for-each mostrar-resultado pruebas)