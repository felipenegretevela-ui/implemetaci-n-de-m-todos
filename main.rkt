#lang racket

(require "config.rkt")
(require "lexer.rkt")
(require "parser.rkt")
(require "validator.rkt")
(require "graphviz.rkt")
(require "html-generator.rkt")
(require "system-utils.rkt")

;-----------CREAR CARPETA files SI NO EXISTE-------------------
(ensure-output-folder)

;-----------LECTURA DE ARCHIVO INPUT-------------------
(define input
  (read-input-file input-file))

;-----------TOKENIZAR INPUT-------------------
(define tokens
  (tokenizer input tokens-table))

;-----------OBTENER EL N-ESIMO AUTOMATA-------------------
(define (tokens-automata-n tokens n)

  (define (cerrar-bloque bloques actual)
    (if (empty? actual)
        bloques
        (append bloques (list (reverse actual)))))

  (define (separar-bloques restantes bloques actual dentro?)
    (cond
      [(empty? restantes)
       (cerrar-bloque bloques actual)]

      [else
       (define token-actual (car restantes))
       (define tipo-token (first token-actual))

       (cond
         [(equal? tipo-token "kw-states")
          (if dentro?
              (separar-bloques
               (cdr restantes)
               (append bloques (list (reverse actual)))
               (list token-actual)
               #t)
              (separar-bloques
               (cdr restantes)
               bloques
               (list token-actual)
               #t))]

         [dentro?
          (separar-bloques
           (cdr restantes)
           bloques
           (cons token-actual actual)
           #t)]

         [else
          (separar-bloques
           (cdr restantes)
           bloques
           actual
           #f)])]))

  (define bloques
    (separar-bloques tokens '() '() #f))

  (list-ref bloques (- n 1)))

;-----------SELECCIONAR AUTOMATA PRINCIPAL-------------------
(define tokens-principal
  (tokens-automata-n tokens 2))

;-----------PARSEAR Y CONSTRUIR AUTOMATA-------------------
(define automaton
  (parse-start tokens-principal))

;-----------VALIDAR CHECKS-------------------
(define validations
  (validate-checks automaton))

;-----------GENERAR GRAFO CON GRAPHVIZ-------------------
(generate-graphviz automaton output-dot output-png)

;-----------GENERAR HTML-------------------
(write-html-file output-html tokens-principal validations)

;-----------CONFIRMACION EN CONSOLA-------------------
(displayln "Archivo creado: files/automata-highlight.html")
(displayln "Archivo creado: files/dfa.dot")
(displayln "Archivo creado: files/dfa.png")

;-----------ABRIR HTML GENERADO-------------------
(sys-show-file output-html)