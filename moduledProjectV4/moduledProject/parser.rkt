#lang racket

(require "automaton.rkt")
(provide parse-start)

;------------------DESCENSO RECURSIVO------------------
(define (current tokens) (car tokens))
(define (current-label tokens) (caar tokens))
(define (current-lexeme tokens) (cadar tokens))
(define (next tokens) (cdr tokens))

(define (match-n-next expected-label tokens)
  (cond
    [(empty? tokens) (error 'parser "se esperaba ~a, pero se llego al final" expected-label)]
    [(equal? (current-label tokens) expected-label) (next tokens)]
    [else (error 'parser "se esperaba ~a, pero se encontro ~a" expected-label (current-label tokens))]))

(define (match-n-get expected-label tokens)
  (cond
    [(empty? tokens) (error 'parser "se esperaba ~a" expected-label)]
    [(equal? (current-label tokens) expected-label) (list (next tokens) (current-lexeme tokens))]
    [else (error 'parser "se esperaba ~a" expected-label)]))

(define (parse-start tokens)
  (parse-start-rest (parse-automaton tokens (make-empty-automaton))))

(define (parse-start-rest result)
  (define tokens-restantes (first result))
  (define auto (second result))
  (if (equal? '() tokens-restantes)
      auto
      (error 'parser "tokens sobrantes al final: ~a" tokens-restantes)))

(define (parse-automaton tokens auto)
  (define result-states (parse-states-section tokens auto))
  (define result-start (parse-start-section (first result-states) (second result-states)))
  (define result-finals (parse-finals-section (first result-start) (second result-start)))
  (define result-alphabet (parse-alphabet-section (first result-finals) (second result-finals)))
  (define result-transitions (parse-transitions-section (first result-alphabet) (second result-alphabet)))
  (define result-checks (parse-checks-section (first result-transitions) (second result-transitions)))
  result-checks)

(define (parse-states-section tokens auto)
  (define t1 (match-n-next "kw-states" tokens))
  (define t2 (match-n-next "equal" t1))
  (define res (parse-state-list t2))
  (list (match-n-next "semicolon" (first res)) (auto-add-states auto (second res))))

(define (parse-start-section tokens auto)
  (define t1 (match-n-next "kw-start" tokens))
  (define t2 (match-n-next "equal" t1))
  (define res (match-n-get "stateId" t2))
  (list (match-n-next "semicolon" (first res)) (auto-add-start auto (second res))))

(define (parse-finals-section tokens auto)
  (define t1 (match-n-next "kw-finals" tokens))
  (define t2 (match-n-next "equal" t1))
  (define res (parse-state-list t2))
  (list (match-n-next "semicolon" (first res)) (auto-add-finals auto (second res))))

(define (parse-alphabet-section tokens auto)
  (define t1 (match-n-next "kw-alphabet" tokens))
  (define t2 (match-n-next "equal" t1))
  (define res (match-n-get "text" t2))
  (list (match-n-next "semicolon" (first res)) (auto-add-alphabet auto (clean-string (second res)))))

(define (parse-transitions-section tokens auto)
  (define t1 (match-n-next "kw-transitions" tokens))
  (define t2 (match-n-next "colon" t1))
  (define res (parse-transition-list t2 auto))
  (list (match-n-next "kw-end" (first res)) (second res)))

(define (parse-transition-list tokens auto)
  (if (and (not (empty? tokens)) (equal? (current-label tokens) "stateId"))
      (let* ([res (parse-transition tokens auto)])
        (parse-transition-list (first res) (second res)))
      (list tokens auto)))

;-----------PARSE TRANSITION------------------
(define (parse-transition tokens auto)
  (define res1 (match-n-get "stateId" tokens))
  (define from-state (second res1))
  (define t2 (match-n-next "lparen" (first res1)))
  
  (define res3 (match-n-get "text" t2))
  (define symbol-str (clean-string (second res3)))
  
  (define-values (t4 pop-str)
    (if (equal? (current-label (first res3)) "comma")
        (let* ([t-comma (match-n-next "comma" (first res3))]
               [res-pop (match-n-get "text" t-comma)])
          (values (first res-pop) (clean-string (second res-pop))))
        (values (first res3) "")))
        
  (define t5 (match-n-next "rparen" t4))
  (define t6 (match-n-next "arrow" t5))
  
  (define res7 (match-n-get "stateId" t6))
  (define to-state (second res7))
  
  (define-values (t8 push-str dir-str)
    (if (equal? (current-label (first res7)) "lparen")
        (let* ([t-lparen (match-n-next "lparen" (first res7))]
               [res-push (match-n-get "text" t-lparen)]
               [t-after-push (first res-push)]
               [val-push (clean-string (second res-push))])
          (if (equal? (current-label t-after-push) "comma")
              (let* ([t-comma2 (match-n-next "comma" t-after-push)]
                     [res-dir (match-n-get "text" t-comma2)]
                     [t-after-dir (first res-dir)]
                     [val-dir (clean-string (second res-dir))])
                (values (match-n-next "rparen" t-after-dir) val-push val-dir))
              (values (match-n-next "rparen" t-after-push) val-push "")))
        (values (first res7) "" "")))
        
  (define t9 (match-n-next "semicolon" t8))
  (list t9 (auto-add-transition auto from-state symbol-str pop-str to-state push-str dir-str)))

(define (parse-checks-section tokens auto)
  (if (and (not (empty? tokens)) (equal? (current-label tokens) "kw-check"))
      (let* ([res (parse-check tokens auto)])
        (parse-checks-section (first res) (second res)))
      (list tokens auto)))

(define (parse-check tokens auto)
  (define t1 (match-n-next "kw-check" tokens))
  (define t2 (match-n-next "equal" t1))
  (define res (match-n-get "text" t2))
  (list (match-n-next "semicolon" (first res)) (auto-add-check auto (clean-string (second res)))))

(define (parse-state-list tokens)
  (define res (match-n-get "stateId" tokens))
  (define res-tail (parse-state-list-prime (first res)))
  (list (first res-tail) (cons (second res) (second res-tail))))

(define (parse-state-list-prime tokens)
  (if (and (not (empty? tokens)) (equal? (current-label tokens) "comma"))
      (let* ([t1 (match-n-next "comma" tokens)]
             [res (match-n-get "stateId" t1)]
             [res-tail (parse-state-list-prime (first res))])
        (list (first res-tail) (cons (second res) (second res-tail))))
      (list tokens '())))