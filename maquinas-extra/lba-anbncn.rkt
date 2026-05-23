#lang racket

;----------------------------------------------------------
; LBA - Automata linealmente acotado
; Lenguaje: a^n b^n c^n
;----------------------------------------------------------

;-----------PRUEBAS-------------------
(define pruebas
  '("abc" "aabbcc" "aaabbbccc"
    "aabbc" "abbcc" "aaabbbcc" "abcc" ""))

;-----------CONVERTIR CADENA A CINTA-------------------
(define (string->tape cadena)
  (map string (string->list cadena)))

;-----------REEMPLAZAR ELEMENTO EN LISTA-------------------
(define (replace-at lista pos valor)
  (cond
    [(empty? lista) '()]
    [(= pos 0)
     (cons valor (cdr lista))]
    [else
     (cons (car lista)
           (replace-at (cdr lista) (- pos 1) valor))]))

;-----------BUSCAR SIMBOLO DESDE POSICION-------------------
(define (buscar-desde tape simbolo inicio)

  (define (buscar lista pos)
    (cond
      [(empty? lista) #f]
      [(and (>= pos inicio)
            (equal? (car lista) simbolo))
       pos]
      [else
       (buscar (cdr lista) (+ pos 1))]))

  (buscar tape 0))

;-----------REVISAR SI TODO ESTA MARCADO-------------------
(define (solo-marcados? tape)
  (andmap
   (lambda (simbolo)
     (or (equal? simbolo "X")
         (equal? simbolo "Y")
         (equal? simbolo "Z")))
   tape))

;-----------REVISAR ORDEN a+b+c+-------------------
(define (orden-correcto? cadena)
  (regexp-match? #rx"^a+b+c+$" cadena))

;-----------SIMULAR LBA-------------------
(define (validate-lba cadena)

  (cond
    [(not (orden-correcto? cadena))
     #f]

    [else
     (let loop ([tape (string->tape cadena)])

       (cond
         [(solo-marcados? tape)
          #t]

         [else
          (let ([pos-a (buscar-desde tape "a" 0)])

            (cond
              [(not pos-a)
               (solo-marcados? tape)]

              [else
               (let* ([tape1 (replace-at tape pos-a "X")]
                      [pos-b (buscar-desde tape1 "b" pos-a)])

                 (cond
                   [(not pos-b)
                    #f]

                   [else
                    (let* ([tape2 (replace-at tape1 pos-b "Y")]
                           [pos-c (buscar-desde tape2 "c" pos-b)])

                      (cond
                        [(not pos-c)
                         #f]

                        [else
                         (loop (replace-at tape2 pos-c "Z"))]))]))]))]))]))

;-----------MOSTRAR RESULTADOS-------------------
(define (mostrar-resultado cadena)
  (displayln
   (string-append
    cadena
    " -> "
    (if (validate-lba cadena)
        "accepted"
        "rejected"))))

;-----------EJECUCION-------------------
(displayln "LBA: reconoce a^n b^n c^n")
(for-each mostrar-resultado pruebas)