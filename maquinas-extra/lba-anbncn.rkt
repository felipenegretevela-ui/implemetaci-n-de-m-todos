#lang racket

; LBA para reconocer a^n b^n c^n
; Acepta cadenas con la misma cantidad de a's, b's y c's
; Ejemplos aceptados: abc, aabbcc, aaabbbccc
; Ejemplos rechazados: aabbc, abbcc, aaabbbcc

(define pruebas
  '("abc" "aabbcc" "aaabbbccc" "aabbc" "abbcc" "aaabbbcc" "abcc" ""))

(define (string->tape cadena)
  (map string (string->list cadena)))

(define (replace-at lista pos valor)
  (cond
    [(empty? lista) '()]
    [(= pos 0)
     (cons valor (cdr lista))]
    [else
     (cons (car lista)
           (replace-at (cdr lista) (- pos 1) valor))]))

(define (buscar-desde tape simbolo inicio)
  (define (buscar i lista)
    (cond
      [(empty? lista) #f]
      [(and (>= i inicio)
            (equal? (car lista) simbolo))
       i]
      [else
       (buscar (+ i 1) (cdr lista))]))
  (buscar 0 tape))

(define (solo-marcados? tape)
  (andmap
   (lambda (x)
     (or (equal? x "X")
         (equal? x "Y")
         (equal? x "Z")))
   tape))

(define (orden-correcto? cadena)
  (regexp-match? #rx"^a+b+c+$" cadena))

(define (simular-lba cadena)
  (if (not (orden-correcto? cadena))
      #f
      (let loop ([tape (string->tape cadena)])
        (cond
          [(solo-marcados? tape) #t]
          [else
           (define pos-a (buscar-desde tape "a" 0))

           (if (not pos-a)
               (solo-marcados? tape)
               (let* ([tape1 (replace-at tape pos-a "X")]
                      [pos-b (buscar-desde tape1 "b" pos-a)])
                 (if (not pos-b)
                     #f
                     (let* ([tape2 (replace-at tape1 pos-b "Y")]
                            [pos-c (buscar-desde tape2 "c" pos-b)])
                       (if (not pos-c)
                           #f
                           (loop (replace-at tape2 pos-c "Z")))))))]))))

(define (mostrar-resultado cadena)
  (displayln
   (string-append
    cadena
    " -> "
    (if (simular-lba cadena)
        "accepted"
        "rejected"))))

(displayln "LBA: reconoce a^n b^n c^n")
(for-each mostrar-resultado pruebas)