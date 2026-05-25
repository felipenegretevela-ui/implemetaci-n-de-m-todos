#lang racket

(provide validate-checks epsilon-closure valida-str valida-pda valida-lba)

;------------CALCULAR VACIO (NFA)----------
(define (epsilon-closure auto active-states)
  (let loop ([current active-states] [visited active-states])
    (define next-states
      (for/fold ([acc '()]) ([state current])
        (append acc (map first (hash-ref (hash-ref auto state (hash)) "" '())))))
        
    (define new-discoveries (filter-not (lambda (s) (member s visited)) (remove-duplicates next-states)))
    (if (empty? new-discoveries) visited (loop new-discoveries (append visited new-discoveries)))))

;-----------GET NEXT STATES (DFA / NFA)----------
(define (get-next-states auto symbol active-states)
  (remove-duplicates
   (for/fold ([acc '()]) ([state active-states])
     (append acc (map first (hash-ref (hash-ref auto state (hash)) symbol '()))))))

;-----------SIMULADORES----------
(define (valida-str auto cadena active-states finales tipo)
  (define states (if (equal? tipo "DFA") active-states (epsilon-closure auto active-states)))
  (cond
    [(empty? states) #f]
    [(equal? cadena "") (ormap (lambda (s) (member s finales)) states)]
    [else
     (define next-active (get-next-states auto (substring cadena 0 1) states))
     (define strict (if (and (equal? tipo "DFA") (> (length next-active) 1)) '() next-active))
     (valida-str auto (substring cadena 1) strict finales tipo)]))

(define (valida-pda auto cadena active-configs finales)
  (cond
    [(empty? active-configs) #f]
    [(equal? cadena "") (ormap (lambda (c) (member (first c) finales)) active-configs)]
    [else
     (define sym (substring cadena 0 1))
     (define next-configs
       (for/fold ([acc '()]) ([config active-configs])
         (match config
           [(list state stack) 
            (append acc
                    (filter-map
                     (match-lambda
                       [(list to-state pop-s push-s _)
                        (cond
                          [(equal? pop-s "") (list to-state (append (string->list push-s) stack))]
                          [(and (not (empty? stack)) (equal? (first (string->list pop-s)) (first stack)))
                           (list to-state (append (string->list push-s) (rest stack)))]
                          [else #f])])
                     (hash-ref (hash-ref auto state (hash)) sym '())))])))
     (valida-pda auto (substring cadena 1) next-configs finales)]))

(define (valida-lba auto active-configs finales max-steps)
  (cond
    [(= max-steps 0) #f]
    [(empty? active-configs) #f]
    [(ormap (lambda (c) (member (first c) finales)) active-configs) #t]
    [else
     (define next-configs
       (for/fold ([acc '()]) ([config active-configs])
         (match config
           [(list state tape head)
            (if (or (< head 0) (>= head (length tape)))
                acc
                (let ([sym (list-ref tape head)])
                  (append acc
                          (for/list ([dest (hash-ref (hash-ref auto state (hash)) sym '())])
                            (match dest
                              [(list to-state _ push-s dir-s)
                               (define write-sym (if (equal? push-s "") sym push-s))
                               (define new-head (match dir-s ["R" (+ head 1)] ["L" (- head 1)] [_ head]))
                               (list to-state (list-set tape head write-sym) new-head)])))))])))
     (valida-lba auto next-configs finales (- max-steps 1))]))

;------------ROUTER MULTI-MÁQUINA----------
(define (validate-checks automaton tipo)
  (define start (hash-ref automaton "inicial"))
  (define finals (hash-ref automaton "finales"))
  
  (for/list ([check (hash-ref automaton "checks")])
    (define res
      (match tipo
        ["PDA" (valida-pda automaton check (list (list start '())) finals)]
        ["LBA" (valida-lba automaton (list (list start (if (equal? check "") '("_") (map string (string->list check))) 0)) finals 1500)]
        [_     (valida-str automaton check (list start) finals tipo)]))
    (list check res)))