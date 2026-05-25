#lang racket

(require "config.rkt")
(require "lexer.rkt")
(require "parser.rkt")
(require "validator.rkt")
(require "graphviz.rkt")
(require "html-generator.rkt")
(require "system-utils.rkt")
(require net/base64 racket/file)

;-----------TIPO DE MAQUINA A PROBAR-------------------
; Cambiar a "DFA", "NFA", "PDA" o "LBA"
(define tipo-maquina "DFA")

;-----------CREAR CARPETA files SI NO EXISTE-------------------
(ensure-output-folder)

;-----------LECTURA DE ARCHIVO INPUT-------------------
(define input
  (read-input-file input-file))

;-----------TOKENIZAR INPUT-------------------
(define tokens
  (tokenizer input tokens-table))

;-----------QUITAR COMENTARIOS PARA EL PARSER-------------------
(define clean-tokens
  (filter
   (lambda (t)
     (not (or (equal? (first t) "commentBlock")
              (equal? (first t) "commentLine"))))
   tokens))

;-----------PARSEAR Y CONSTRUIR AUTOMATA-------------------
(define automaton
  (parse-start clean-tokens))

;-----------VALIDAR CHECKS-------------------
(define validations
  (validate-checks automaton tipo-maquina))

;-----------GENERAR GRAFO CON GRAPHVIZ-------------------
(generate-graphviz automaton output-dot output-png)

;-----------LEER IMAGEN COMO BASE64-------------------
(define img-base64
  (bytes->string/utf-8
   (base64-encode (file->bytes output-png) #"\n")))

;-----------GENERAR HTML-------------------
(write-html-file output-html tokens validations img-base64)

;-----------CONFIRMACION EN CONSOLA-------------------
(displayln "Archivo creado: files/automata-highlight.html")
(displayln "Archivo creado: files/dfa.dot")
(displayln "Archivo creado: files/dfa.png")
(displayln (string-append "Tipo de maquina evaluada: " tipo-maquina))

;-----------ABRIR HTML GENERADO-------------------
(sys-show-file output-html)