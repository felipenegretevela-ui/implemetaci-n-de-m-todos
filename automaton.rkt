#lang racket

(provide make-empty-automaton auto-add-states auto-add-start auto-add-finals auto-add-alphabet
         auto-add-check auto-add-transition clean-string string->symbol-list)

(define (make-empty-automaton)
  (hash "estados" '() "inicial" "" "finales" '() "alfabeto" "" "checks" '()))

(define (auto-add-states auto states) (hash-set auto "estados" states))
(define (auto-add-start auto start) (hash-set auto "inicial" start))
(define (auto-add-finals auto finals) (hash-set auto "finales" finals))
(define (auto-add-alphabet auto alphabet) (hash-set auto "alfabeto" alphabet))

(define (auto-add-check auto check-str)
  (hash-update auto "checks" (lambda (checks) (append checks (list check-str))) '()))

(define (clean-string str)
  (string-trim str "\""))

(define (string->symbol-list str)
  (if (equal? str "") '("") (map string (string->list str))))

;-----------AGREGAR TRANSICION EN FORMATO TUPLA----------
(define (auto-add-transition auto from-state symbol pop-str to-state push-str dir-str)
  (define symbols (string->symbol-list symbol))
  (define new-dest (list to-state pop-str push-str dir-str))
  
  (hash-update auto from-state
               (lambda (state-trans)
                 (for/fold ([acc state-trans]) ([sym symbols])
                   (hash-update acc sym (lambda (dests) (append dests (list new-dest))) '())))
               (hash)))