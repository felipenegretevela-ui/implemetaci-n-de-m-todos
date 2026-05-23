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
(displayln "Antes de generar Graphviz")
(displayln output-dot)
(displayln output-png)

(generate-graphviz automaton output-dot output-png)

(displayln "Despues de generar Graphviz")
(displayln (file-exists? output-dot))
(displayln (file-exists? output-png))

;-----------GENERAR HTML-------------------
(write-html-file output-html tokens validations)

;-----------CONFIRMACION EN CONSOLA-------------------
(displayln "Archivo creado: files/automata-highlight.html")
(displayln "Archivo creado: files/dfa.dot")
(displayln "Archivo creado: files/dfa.png")

;-----------ABRIR HTML GENERADO-------------------
(sys-show-file output-html)