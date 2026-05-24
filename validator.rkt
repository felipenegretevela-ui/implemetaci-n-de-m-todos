#lang racket

#|
Habilite la lectura de vacio 
Elimie la función get-next y la cambie por por get-next-states, para soportar múltiples 
estados activos a la vez lo cual es la logica del NFA.
La funcion valida-str es qn realiza la simulación del autómata rastreando una lista completa 
de states en lugar de un único current state.
|#




(provide validate-checks
         exists?
         get-next-states
         epsilon-closure
         valida-str)

;------------REVISAR SI UN ELEMENTO EXISTE EN UNA LISTA----------
(define (exists? lista x)
  (cond
    [(empty? lista) #f]
    [(member x lista) #t]
    [else #f]))

;------------CALCULAR VACIO----------
(define (epsilon-closure auto active-states)
  (define (closure-worker current-states visited)
    (define next-epsilon-states
      (foldl
       (lambda (state accum)
         (if (hash-has-key? auto state)
             (let ([transitions (hash-ref auto state)])
               (if (hash-has-key? transitions "") ; Busca transiciones vacías
                   (let ([dest (hash-ref transitions "")])
                     (define dest-list (if (list? dest) dest (list dest)))
                     (remove-duplicates (append accum dest-list)))
                   accum))
             accum))
       '()
       current-states))
    
    ;Filtramos los estados que encontramos para no caer en ciclos infinitos
    (define new-discoveries
      (filter (lambda (s) (not (member s visited))) next-epsilon-states))
    
    (if (empty? new-discoveries)
        visited ;Si no hay estados nuevos finalizamos
        (closure-worker new-discoveries (append visited new-discoveries))))
  
  (closure-worker active-states active-states))

;------------OBTENER TODOS LOS ESTADOS SIGUIENTES (NFA)----------
(define (get-next-states auto symbol active-states)
  (foldl
   (lambda (state accum)
     (if (hash-has-key? auto state)
         (let* ([transitions (hash-ref auto state)])
           (if (hash-has-key? transitions symbol)
               (let ([dest (hash-ref transitions symbol)])
                 (define dest-list (if (list? dest) dest (list dest)))
                 (remove-duplicates (append accum dest-list)))
               accum))
         accum))
   '()
   active-states))

;-----------SIMULADOR DE AUTOMATA CON SOPORTE ÉPSILON----------
(define (valida-str auto cadena active-states finales)
  ;Aplicamos clausura épsilon antes de evaluar cualquier cosa
  (define states-after-epsilon (epsilon-closure auto active-states))
  
  (cond
    ;Si la cadena terminó, revisamos si estamos en algún estado final
    [(equal? cadena "")
     (ormap (lambda (state) (exists? finales state)) states-after-epsilon)]

    ;Si no nos quedan estados activos, se rechaza
    [(empty? states-after-epsilon)
     #f]

    ;Procesamos el siguiente símbolo consumiendo la cadena
    [else
     (define symbol (substring cadena 0 1))
     (define next-active (get-next-states auto symbol states-after-epsilon))
     (define remaining-cadena (substring cadena 1))
     (valida-str auto remaining-cadena next-active finales)]))

;------------VALIDAR TODOS LOS CHECKS DEL AUTOMATA----------
(define (validate-checks automaton)
  (define checks (hash-ref automaton "checks"))
  (define start-state (hash-ref automaton "inicial"))
  (define final-states (hash-ref automaton "finales"))

  (map
   (lambda (check)
     (list check (valida-str automaton check (list start-state) final-states)))
   checks))