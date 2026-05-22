#lang racket

(provide tokenizer
         get-match
         get-max
         last-element)

(define re-match regexp-match-positions)

;-----------FUNCION BUSCA MATCH----------------------
(define (get-match label-regex str)
  (define label (first label-regex))
  (define regex (second label-regex))
  (define match (re-match regex str))

  (if match
      (let* ([len-match (cdr (first match))]
             [sub-str (substring str 0 len-match)])
        (list label len-match sub-str))
      (list "none" 0 "")))

;-----------ENCUENTRA MATCH MÁS GRANDE-------------------
(define (get-max all-matches)
  (let* ([lengths (map second all-matches)]
         [max-len (apply max lengths)]
         [is-max? (lambda (item) (= max-len (second item)))])
    (first (filter is-max? all-matches))))

;------IMPRIMIR LAST ELEMENT OF LIST------
(define (last-element lista)
  (cond
    [(empty? lista) '()]
    [(empty? (cdr lista)) (car lista)]
    [else (last-element (cdr lista))]))

;-----------TOKENIZER------------------
(define (tokenizer input tokens-table)

  (define (tokenizer-acc input tokens-table tokens-acc)
    (cond
      ;---------SI INPUT ES VACIO, DEVOLVER EL ACOMULADOR DE TOKENS-------
      [(equal? input "")
       tokens-acc]

      ;---------SI INPUT COMIENZA CON ESPACIO, QUITARSELO-------
      [(not (equal? '("none" 0 "")
                    (get-match '("space" #rx"^[ \t\n\r]+") input)))
       (define space-match
         (get-match '("space" #rx"^[ \t\n\r]+") input))

       (define len
         (second space-match))

       (tokenizer-acc (substring input len)
                      tokens-table
                      tokens-acc)]

      [else
       ;-----------PRUEBA TODOS LOS TOKENS----------------------
       (define all-matches
         (map (lambda (r) (get-match r input)) tokens-table))

       ;------ENCUENTRA EL MATCH MAS LARGO
       (define best-match
         (get-max all-matches))

       ;-----DEL MATCH MAS LARGO SACAR TOKENID, LONGITUD Y LEXEMA------
       (define token-id (first best-match))
       (define len (second best-match))
       (define lexeme (third best-match))
       (define rest-input (substring input len))

       ;----CREAR NUEVO TOKEN, CON TOKENID Y LEXEMA
       (define new-token
         (list token-id lexeme))

       ;--------SI LA LONGITUD ES CERO, MARCAR ERROR-------
       (if (equal? 0 len)
           (error "Not a recognized token, last token was:" (last-element tokens-acc))

           ;---SI NO ES CERO, RECORTAR EL INPUT LA LONGITUD DEL TOKEN
           (if (or (equal? token-id "commentBlock")
                   (equal? token-id "commentLine")
                   (equal? token-id "space"))
               ;---SI ES COMENTARIO O ESPACIO, NO SE AGREGA A tokens-acc
               (tokenizer-acc rest-input
                              tokens-table
                              tokens-acc)

               ;---SI ES TOKEN REAL, SE AGREGA AL ACUMULADOR
               (tokenizer-acc rest-input
                              tokens-table
                              (append tokens-acc (list new-token)))))]))

  ;---------LLAMADA INICIAL A LA FUNCION AUXILIAR-------
  (tokenizer-acc input tokens-table '()))