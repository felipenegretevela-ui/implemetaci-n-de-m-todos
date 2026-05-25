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

;-----------PARSEAR Y CONSTRUIR AUTOMATA-------------------
(define automaton
  (parse-start tokens))

;-----------VALIDAR CHECKS-------------------
(define validations
  (validate-checks automaton))

;-----------GENERAR GRAFO CON GRAPHVIZ-------------------
(define img-base64
  (generate-graphviz automaton output-dot output-png))

;-----------GENERAR HTML-------------------
(write-html-file output-html tokens validations img-base64)

;-----------CONFIRMACION EN CONSOLA-------------------
(displayln "Archivo creado: files/automata-highlight.html (Con gráfico incrustado)")
(displayln "Archivo creado: files/dfa.dot")
(displayln "Archivo creado: files/dfa.png")

;-----------VALIDAR CHECKS----------
(define validations
  (validate-checks automaton "NFA"))

;-----------ABRIR HTML GENERADO-------------------
(sys-show-file output-html)