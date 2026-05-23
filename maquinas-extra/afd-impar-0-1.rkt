#lang racket

;----------------------------------------------------------
; AFD extra
; Reconoce cadenas binarias con numero impar de 0's
; y numero impar de 1's
;----------------------------------------------------------

;-----------PRUEBAS-------------------
(define pruebas
  '("01" "10" "0011" "0101"
    "0" "1" "00" "11" "" "101"))

;-----------AUTOMATA EN HASH-------------------
(define afd
  (hash
   "q00" (hash
          "0" "q10"
          "1" "q01")

   "q10" (hash
          "0" "q00"
          "1" "q11")

   "q01" (hash
          "0" "q11"
          "1" "q00")

   "q11" (hash
          "0" "q01"
          "1" "q10")))

(define start-state "q00")
(define accept-states '("q11"))

;-----------CONVERTIR CADENA A LISTA-------------------
(define (string->chars cadena)
  (map string (string->list cadena)))

;-----------OBTENER SIGUIENTE ESTADO-------------------
(define (get-next auto current symbol)
  (cond
    [(not (hash-has-key? auto current)) #f]
    [else
     (let ([transitions (hash-ref auto current)])
       (cond
         [(hash-has-key? transitions symbol)
          (hash-ref transitions symbol)]
         [else #f]))]))

;-----------SIMULAR AFD-------------------
(define (validate-afd cadena)

  (define simbolos
    (string->chars cadena))

  (define (procesar lista current)
    (cond
      [(empty? lista)
       (member current accept-states)]

      [else
       (let* ([symbol (car lista)]
              [resto (cdr lista)]
              [next (get-next afd current symbol)])
         (cond
           [next
            (procesar resto next)]
           [else #f]))]))

  (procesar simbolos start-state))

;-----------MOSTRAR CADENA-------------------
(define (mostrar-cadena cadena)
  (if (equal? cadena "")
      "epsilon"
      cadena))

;-----------MOSTRAR RESULTADOS-------------------
(define (mostrar-resultado cadena)
  (displayln
   (string-append
    (mostrar-cadena cadena)
    " -> "
    (if (validate-afd cadena)
        "accepted"
        "rejected"))))
        

;-----------EJECUCION-------------------
(displayln "AFD: reconoce cadenas con numero impar de 0's y numero impar de 1's")
(for-each mostrar-resultado pruebas)