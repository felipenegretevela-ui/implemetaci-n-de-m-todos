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

;-----------OBTENER SOLO EL PRIMER AUTOMATA-------------------
(define (tokens-primer-automata tokens)
  (define (aux restantes ya-vio-states? acumulado)
    (cond
      [(empty? restantes)
       (reverse acumulado)]

      [else
       (define token-actual
         (car restantes))

       (define tipo-token
         (first token-actual))

       (cond
         [(and ya-vio-states?
               (equal? tipo-token "kw-states"))
          (reverse acumulado)]

         [(equal? tipo-token "kw-states")
          (aux (cdr restantes)
               #t
               (cons token-actual acumulado))]

         [else
          (aux (cdr restantes)
               ya-vio-states?
               (cons token-actual acumulado))])]))

  (aux tokens #f '()))

(define tokens-principal
  (tokens-primer-automata tokens))

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