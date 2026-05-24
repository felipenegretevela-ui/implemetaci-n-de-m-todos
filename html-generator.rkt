#lang racket



(provide write-html-file
         build-full-html
         styler
         validation-styler
         style-validations
         tokens->html
         escape-html)

;-----------HTML HEADER------------------
(define html-start
  "<!DOCTYPE html>
<html lang='en'>
<head>
  <meta charset='UTF-8'>
  <meta name='viewport' content='width=device-width, initial-scale=1.0'>
  <title>Automata Syntax Highlight</title>

  <style>
    body {
      font-family: 'Courier New', Courier, monospace;
      background-color: #1F1F1F;
      color: #CBCBCB;
      line-height: 1.5;
      padding: 20px;
      white-space: pre-wrap;
    }

    .keyword {
      color: #A9DAFB;
    }

    .state {
      color: #679BD1;
    }

    .text {
      color: #C4947C;
    }

    .comment {
      color: #73985D;
    }

    .bracket {
      color: #F9D949;
    }

    .operator {
      color: #CBCBCB;
    }

    .symbol {
      color: #CBCBCB;
    }

    .quote {
      color: #C4947C;
    }

    .error {
      color: #F44747;
    }

    .accepted {
      color: #71C6B1;
    }

    .rejected {
      color: #E1544F;
    }

    img {
      max-width: 96vw;
      height: auto;
      margin-top: 1rem;
    }
  </style>
</head>
<body>")

;-----------HTML FOOTER------------------
(define html-end
  "<img src='dfa.png' alt='El gráfico del autómata no pudo ser compilado T_T'>
</body>
</html>")

;-----------ESCAPA CARACTERES ESPECIALES DE HTML------------------
(define (escape-html str)
  (define str1
    (string-replace str "&" "&amp;"))

  (define str2
    (string-replace str1 "<" "&lt;"))

  (define str3
    (string-replace str2 ">" "&gt;"))

  str3)

;-----------FUNCION STYLER------------------
(define (styler token)

  ;---------SACAR LABEL Y LEXEMA DEL TOKEN-------
  (define label
    (first token))

  (define lexeme
    (escape-html (second token)))

  (cond
    ;-----------PALABRAS RESERVADAS------------------
    [(or (equal? label "kw-states")
         (equal? label "kw-start")
         (equal? label "kw-alphabet")
         (equal? label "kw-transitions")
         (equal? label "kw-check")
         (equal? label "kw-finals"))
     (string-append "<span class='keyword'>" lexeme "</span>")]

    ;-----------IDENTIFICADORES DE ESTADOS------------------
    [(equal? label "stateId")
     (string-append "<span class='state'>" lexeme "</span>")]

    ;-----------STRINGS ENTRE COMILLAS------------------
    [(equal? label "string")
     (string-append "<span class='text'>" lexeme "</span>")]

    ;-----------COMENTARIOS------------------
    [(or (equal? label "commentBlock")
         (equal? label "commentLine"))
     (string-append "<span class='comment'>" lexeme "</span><br>")]

    ;-----------PARENTESIS------------------
    [(or (equal? label "lparen")
         (equal? label "rparen"))
     (string-append "<span class='bracket'>" lexeme "</span>")]

    ;-----------OPERADORES------------------
    [(or (equal? label "arrow")
         (equal? label "equal"))
     (string-append "<span class='operator'> " lexeme "</span> ")]

    ;-----------SIMBOLOS ESPECIALES------------------
    [(or (equal? label "colon")
         (equal? label "comma"))
     (string-append "<span class='symbol'>" lexeme "</span> ")]

    ;-----------PUNTO Y COMA------------------
    [(equal? label "semicolon")
     (string-append "<span class='symbol'>" lexeme "</span><br>")]

    ;-----------COMILLAS SIMPLES SUELTAS------------------
    [(equal? label "quotation-marks")
     (string-append "<span class='quote'>" lexeme "</span>")]

    ;-----------KEYWORD-END------------------
    [(equal? label "kw-end")
     (string-append "<span class='keyword'>" lexeme "</span><br>")]

    ;-----------ESPACIOS SI EXISTEN EN LA LISTA DE TOKENS------------------
    [(equal? label "space")
     lexeme]

    ;-----------CUALQUIER TOKEN NO CONTEMPLADO------------------
    [else
     (string-append "<span class='error'>" lexeme "</span>")]))

;-----------STYLER DE VALIDACIONES------------------
(define (validation-styler validation)
  (define check
    (first validation))

  (define result
    (second validation))

  (if result
      (string-append
       "<span class='accepted'>"
       (escape-html (format "~a" check))
       " -> accepted"
       "</span><br>")
      (string-append
       "<span class='rejected'>"
       (escape-html (format "~a" check))
       " -> rejected"
       "</span><br>")))

;-----------STYLING DE TODAS LAS VALIDACIONES------------------
(define (style-validations validations)
  (apply string-append
         (map validation-styler validations)))

;-----------CONVERTIR TOKENS A HTML------------------
(define (tokens->html tokens)
  (define styled-tokens
    (map styler tokens))

  (apply string-append styled-tokens))

;-----------CREAR HTML COMPLETO------------------
(define (build-full-html tokens validations img-base64)
  (define html-body
    (tokens->html tokens))

  (define html-checks
    (style-validations validations))

  (define html-img
    (string-append "<br><h2>Visualización del Autómata</h2>"
                   "<div style='background-color:#2A2A2A; padding:15px; border-radius:8px; display:inline-block;'>"
                   "  <img src=\"data:image/png;base64," img-base64 "\" alt=\"Automata Graph\" style=\"max-width:100%; height:auto;\">"
                   "</div><br><br>"))

  (string-append html-start
                 html-body
                 html-img
                 "<h2>Resultados de Validación (Checks)</h2>"
                 html-checks
                 html-end))

;-----------ESCRIBIR ARCHIVO HTML------------------
(define (write-html-file path tokens validations img-base64)
  (define full-html
    (build-full-html tokens validations img-base64))

  (call-with-output-file path #:exists 'replace
    (lambda (out) (display full-html out))))