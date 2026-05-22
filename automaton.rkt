#lang racket

(provide make-empty-automaton
         auto-add-states
         auto-add-start
         auto-add-finals
         auto-add-alphabet
         auto-add-check
         auto-add-transition
         clean-string
         string->symbol-list)

#|
Agregue que se pudieran leer transiciones con múltiples símbolos de entrada, ademas que 
detectara las trancisionces con vacio 
|#


;---------AUTOMATA VACIO INICIAL---------
(define (make-empty-automaton)
  (hash "estados" '()
        "inicial" ""
        "finales" '()
        "alfabeto" ""
        "checks" '()))

;-----------AGREGAR ESTADOS AL AUTOMATA------------------
(define (auto-add-states auto states)
  (hash-set auto "estados" states))

;-----------AGREGAR ESTADO INICIAL AL AUTOMATA------------------
(define (auto-add-start auto start)
  (hash-set auto "inicial" start))

;-----------AGREGAR ESTADOS FINALES AL AUTOMATA------------------
(define (auto-add-finals auto finals)
  (hash-set auto "finales" finals))

;-----------AGREGAR ALFABETO AL AUTOMATA------------------
(define (auto-add-alphabet auto alphabet)
  (hash-set auto "alfabeto" alphabet))

;-----------AGREGAR CHECK AL AUTOMATA------------------
(define (auto-add-check auto check-str)
  (define old-checks
    (hash-ref auto "checks"))
  (hash-set auto "checks"
            (append old-checks (list check-str))))

;-----------LIMPIAR COMILLAS DE UN STRING------------------
(define (clean-string str)
  (string-trim str "\""))

;-----------CONVERTIR STRING A LISTA DE SIMBOLOS INDIVIDUALES------------------
(define (string->symbol-list str)
  (if (equal? str "")
      '("") 
      (map string (string->list str))))

;-----------AGREGAR UNA TRANSICION INDIVIDUAL AL HASH----------
;Asociamos el símbolo en vez de a un solo estado string a una lista de estados para que cumpla con los requerimientos de NFA.
(define (add-one-transition transitions symbol to-state)
  (define current-destinations
    (if (hash-has-key? transitions symbol)
        (let ([val (hash-ref transitions symbol)])
          ; Aseguramos que el valor actual sea tratado como lista
          (if (list? val) val (list val)))
        '()))
  
  
  (define updated-destinations
    (if (member to-state current-destinations)
        current-destinations
        (append current-destinations (list to-state))))
        
  (hash-set transitions symbol updated-destinations))

;-----------AGREGAR VARIAS TRANSICIONES AL AUTOMATA------------------
(define (auto-add-transition auto from-state symbol to-state)
  ;Obtenemos el hash actual de transiciones para 'from-state'
  (define old-transitions
    (if (hash-has-key? auto from-state)
        (hash-ref auto from-state)
        (hash)))

  ;Convertimos el símbolo de entrada en caracteres individuales
  (define symbols
    (string->symbol-list symbol))

  ;Registramos la transición para cada uno de los símbolo
  (define new-transitions
    (foldl
     (lambda (current-symbol transitions-acc)
       (add-one-transition transitions-acc current-symbol to-state))
     old-transitions
     symbols))

  ;Guardamos de vuelta el mapa de transiciones
  (hash-set auto from-state new-transitions))