#lang racket

(require racket/runtime-path)
(require racket/system)
(require racket/string)

;-----------RUTAS DE ARCHIVOS DE MAQUINAS------------------
(define-runtime-path dfa-file "dfa.rkt")
(define-runtime-path pda-file "pda.rkt")
(define-runtime-path lba-file "lba.rkt")
(define-runtime-path turing-file "turing.rkt")

;-----------RUTAS DE SALIDA------------------
(define-runtime-path output-file "../files/maquinas-extra.html")

(define-runtime-path dfa-dot "../files/dfa-extra.dot")
(define-runtime-path dfa-png "../files/dfa-extra.png")

(define-runtime-path pda-dot "../files/pda-extra.dot")
(define-runtime-path pda-png "../files/pda-extra.png")

(define-runtime-path lba-dot "../files/lba-extra.dot")
(define-runtime-path lba-png "../files/lba-extra.png")

(define-runtime-path turing-dot "../files/turing-extra.dot")
(define-runtime-path turing-png "../files/turing-extra.png")

;-----------CREAR CARPETA FILES------------------
(make-directory* "../files")

;-----------ESCAPAR CARACTERES HTML------------------
(define (escape-html str)
  (define str1
    (string-replace str "&" "&amp;"))

  (define str2
    (string-replace str1 "<" "&lt;"))

  (define str3
    (string-replace str2 ">" "&gt;"))

  str3)

;-----------EJECUTAR CADA MAQUINA Y CAPTURAR SALIDA------------------
(define (run-machine path)
  (with-output-to-string
    (lambda ()
      (dynamic-require path #f))))

;-----------CREAR ARCHIVO DOT Y PNG------------------
(define (crear-grafico dot-path png-path dot-text)
  (call-with-output-file dot-path
    (lambda (out)
      (display dot-text out))
    #:exists 'replace)

  (system*
   (find-executable-path "dot")
   "-Tpng"
   (path->string dot-path)
   "-o"
   (path->string png-path)))

;-----------DAR COLOR A LAS PALABRAS DE LA SINTAXIS------------------
(define (linea-sintaxis->html linea)
  (cond
    [(or (string-prefix? linea "Estados:")
         (string-prefix? linea "Estado inicial:")
         (string-prefix? linea "Estado final:")
         (string-prefix? linea "Alfabeto:")
         (string-prefix? linea "Alfabeto de entrada:")
         (string-prefix? linea "Simbolo inicial de pila:")
         (string-prefix? linea "Cinta:")
         (string-prefix? linea "Cinta limitada:")
         (string-prefix? linea "Simbolo blanco:")
         (string-prefix? linea "Lenguaje:")
         (string-prefix? linea "Funcion:")
         (string-prefix? linea "Funcionamiento:")
         (string-prefix? linea "Transiciones:")
         (string-prefix? linea "Marcas:"))
     (string-append "<span class='keyword'>" (escape-html linea) "</span>")]

    [(or (string-prefix? linea "q")
         (string-prefix? linea "s")
         (string-prefix? linea "start"))
     (string-append "<span class='transition'>" (escape-html linea) "</span>")]

    [(or (string-prefix? linea "a ->")
         (string-prefix? linea "b ->")
         (string-prefix? linea "c ->"))
     (string-append "<span class='transition'>" (escape-html linea) "</span>")]

    [(regexp-match? #rx"^[0-9]+\\." linea)
     (string-append "<span class='comment'>" (escape-html linea) "</span>")]

    [(string=? linea "")
     ""]

    [else
     (escape-html linea)]))

(define (sintaxis->html texto)
  (string-join
   (map linea-sintaxis->html
        (string-split texto "\n" #:trim? #f))
   "\n"))

;-----------DAR COLOR A RESULTADOS DE PRUEBAS------------------
(define (linea-prueba->html linea)
  (cond
    [(string-contains? linea "accepted")
     (string-append "<span class='accepted'>" (escape-html linea) "</span>")]

    [(string-contains? linea "rejected")
     (string-append "<span class='rejected'>" (escape-html linea) "</span>")]

    [(string-contains? linea "->")
     (string-append "<span class='transition'>" (escape-html linea) "</span>")]

    [(or (string-prefix? linea "AFD:")
         (string-prefix? linea "PDA:")
         (string-prefix? linea "LBA:")
         (string-prefix? linea "Maquina")
         (string-prefix? linea "Máquina"))
     (string-append "<span class='keyword'>" (escape-html linea) "</span>")]

    [else
     (escape-html linea)]))

(define (pruebas->html texto)
  (string-join
   (map linea-prueba->html
        (string-split texto "\n" #:trim? #f))
   "\n"))

;-----------SINTAXIS / DEFINICION DFA------------------
(define dfa-syntax
"Estados: q00, q10, q01, q11
Estado inicial: q00
Estado final: q11
Alfabeto: 0, 1

Lenguaje:
Cadenas binarias con numero impar de 0's y numero impar de 1's.

Transiciones:
q00 con 0 -> q10
q00 con 1 -> q01

q10 con 0 -> q00
q10 con 1 -> q11

q01 con 0 -> q11
q01 con 1 -> q00

q11 con 0 -> q01
q11 con 1 -> q10")

;-----------SINTAXIS / DEFINICION PDA------------------
(define pda-syntax
"Estados: start, s0, s1, s2
Estado inicial: start
Estado final: s2
Alfabeto de entrada: a, b
Simbolo inicial de pila: #

Lenguaje:
L = { a^n b^n | n >= 1 }

Transiciones:
start, epsilon, epsilon -> s0, #

s0, a, # -> s0, a#
s0, a, a -> s0, aa

s0, b, a -> s1, epsilon
s1, b, a -> s1, epsilon

s1, epsilon, # -> s2, #")

;-----------SINTAXIS / DEFINICION LBA------------------
(define lba-syntax
"Estados: q0, q1, q2, q3
Estado inicial: q0
Estado final: q3
Cinta limitada: mismo tamano que la entrada

Lenguaje:
L = { a^n b^n c^n | n >= 1 }

Marcas:
a -> X
b -> Y
c -> Z

Funcionamiento:
1. Busca una a y la marca como X.
2. Busca una b y la marca como Y.
3. Busca una c y la marca como Z.
4. Repite el proceso hasta que todo quede marcado.
5. Si no encuentra el orden o cantidad correcta, rechaza.")

;-----------SINTAXIS / DEFINICION TURING------------------
(define turing-syntax
"Estados: q0, qf
Estado inicial: q0
Estado final: qf
Simbolo blanco: B
Cinta: entrada + B

Funcion:
Transforma todos los simbolos 1 en X.

Transiciones:
q0 lee 1 -> escribe X, mueve R, sigue en q0
q0 lee B -> escribe B, se queda S, pasa a qf")

;-----------GRAFICO DFA------------------
(define dfa-dot-text
  "digraph G {
  rankdir=LR;
  node [fontname=\"Helvetica\", fontsize=11];
  edge [arrowsize=0.8, arrowhead=vee];

  qi [shape=point, width=0];

  q00 [shape=circle, style=filled, fillcolor=\"#E3F2FD\", color=\"#1E88E5\"];
  q10 [shape=circle, style=filled, fillcolor=\"#F5F5F5\", color=\"#757575\"];
  q01 [shape=circle, style=filled, fillcolor=\"#F5F5F5\", color=\"#757575\"];
  q11 [shape=doublecircle, style=filled, fillcolor=\"#DDF7E3\", color=\"#4E9F3D\", penwidth=2];

  qi -> q00;

  q00 -> q10 [label=\"0\"];
  q00 -> q01 [label=\"1\"];

  q10 -> q00 [label=\"0\"];
  q10 -> q11 [label=\"1\"];

  q01 -> q11 [label=\"0\"];
  q01 -> q00 [label=\"1\"];

  q11 -> q01 [label=\"0\"];
  q11 -> q10 [label=\"1\"];
}")

;-----------GRAFICO PDA------------------
(define pda-dot-text
  "digraph G {
  rankdir=LR;
  node [fontname=\"Helvetica\", fontsize=11];
  edge [arrowsize=0.8, arrowhead=vee];

  qi [shape=point, width=0];

  start [shape=circle, style=filled, fillcolor=\"#E3F2FD\", color=\"#1E88E5\"];
  s0 [shape=circle, style=filled, fillcolor=\"#F5F5F5\", color=\"#757575\"];
  s1 [shape=circle, style=filled, fillcolor=\"#F5F5F5\", color=\"#757575\"];
  s2 [shape=doublecircle, style=filled, fillcolor=\"#DDF7E3\", color=\"#4E9F3D\", penwidth=2];

  qi -> start;

  start -> s0 [label=\"epsilon, epsilon -> #\"];
  s0 -> s0 [label=\"a, # -> a# / a, a -> aa\"];
  s0 -> s1 [label=\"b, a -> epsilon\"];
  s1 -> s1 [label=\"b, a -> epsilon\"];
  s1 -> s2 [label=\"epsilon, # -> #\"];
}")

;-----------GRAFICO LBA------------------
(define lba-dot-text
  "digraph G {
  rankdir=LR;
  node [fontname=\"Helvetica\", fontsize=11];
  edge [arrowsize=0.8, arrowhead=vee];

  qi [shape=point, width=0];

  q0 [shape=circle, style=filled, fillcolor=\"#E3F2FD\", color=\"#1E88E5\"];
  q1 [shape=circle, style=filled, fillcolor=\"#F5F5F5\", color=\"#757575\"];
  q2 [shape=circle, style=filled, fillcolor=\"#F5F5F5\", color=\"#757575\"];
  q3 [shape=doublecircle, style=filled, fillcolor=\"#DDF7E3\", color=\"#4E9F3D\", penwidth=2];

  qi -> q0;

  q0 -> q1 [label=\"marcar a como X\"];
  q1 -> q2 [label=\"buscar b y marcar Y\"];
  q2 -> q0 [label=\"buscar c y marcar Z\"];
  q0 -> q3 [label=\"todo marcado\"];
}")

;-----------GRAFICO TURING------------------
(define turing-dot-text
  "digraph G {
  rankdir=LR;
  node [fontname=\"Helvetica\", fontsize=11];
  edge [arrowsize=0.8, arrowhead=vee];

  qi [shape=point, width=0];

  q0 [shape=circle, style=filled, fillcolor=\"#E3F2FD\", color=\"#1E88E5\"];
  qf [shape=doublecircle, style=filled, fillcolor=\"#DDF7E3\", color=\"#4E9F3D\", penwidth=2];

  qi -> q0;

  q0 -> q0 [label=\"1 / X, R\"];
  q0 -> qf [label=\"B / B, S\"];
}")

;-----------GENERAR GRAFICOS------------------
(crear-grafico dfa-dot dfa-png dfa-dot-text)
(crear-grafico pda-dot pda-png pda-dot-text)
(crear-grafico lba-dot lba-png lba-dot-text)
(crear-grafico turing-dot turing-png turing-dot-text)

;-----------EJECUTAR MAQUINAS------------------
(define dfa-output
  (run-machine dfa-file))

(define pda-output
  (run-machine pda-file))

(define lba-output
  (run-machine lba-file))

(define turing-output
  (run-machine turing-file))

;-----------HTML COMPLETO------------------
(define html-content
  (string-append
   "<!DOCTYPE html>\n"
   "<html lang='es'>\n"
   "<head>\n"
   "<meta charset='UTF-8'>\n"
   "<meta name='viewport' content='width=device-width, initial-scale=1.0'>\n"
   "<title>Máquinas Extra</title>\n"

   "<style>\n"
   "body { font-family: 'Courier New', Courier, monospace; background:#1f1f1f; color:#cbcbcb; padding:30px; max-width:1100px; margin:auto; line-height:1.5; }\n"
   "h1 { color:#9ADAFB; }\n"
   "h2 { color:#F9D949; margin-top:40px; border-top:1px solid #444; padding-top:25px; }\n"
   "h3 { color:#9ADAFB; }\n"
   "p { line-height:1.6; color:#dddddd; }\n"
   "pre { background:#111; padding:15px; border-radius:8px; color:#cbcbcb; overflow-x:auto; white-space:pre-wrap; }\n"
   "img { background:#ffffff; padding:15px; border-radius:8px; max-width:95%; margin:15px 0; }\n"
   ".card { background:#2b2b2b; border:1px solid #444; padding:20px; border-radius:10px; margin-top:15px; }\n"
   ".label { color:#F9D949; font-weight:bold; }\n"
   ".syntax-title { color:#9ADAFB; font-weight:bold; margin-top:20px; }\n"
   ".syntax { background:#1a1a1a; border:1px solid #444; color:#ffffff; }\n"
   ".keyword { color:#A9DAFB; font-weight:bold; }\n"
   ".state { color:#679BD1; font-weight:bold; }\n"
   ".text { color:#C4947C; }\n"
   ".comment { color:#73985D; font-style:italic; }\n"
   ".transition { color:#F9D949; }\n"
   ".accepted { color:#71C6B1; }\n"
   ".rejected { color:#E1544F; }\n"
   "a { color:#9ADAFB; }\n"
   "</style>\n"

   "</head>\n"
   "<body>\n"

   "<h1>Máquinas extra implementadas</h1>\n"
   "<p>Este archivo muestra las máquinas extra agregadas al proyecto. Cada sección identifica el tipo de máquina, el lenguaje o función que reconoce, una descripción de su funcionamiento, su sintaxis o definición, el gráfico generado con Graphviz y las pruebas ejecutadas.</p>\n"
   "<p><a href='automata-highlight.html'>Volver al autómata principal</a></p>\n"

   "<h2>1. DFA - Autómata Finito Determinista</h2>\n"
   "<div class='card'>\n"
   "<p><span class='label'>Tipo:</span> DFA, autómata finito determinista.</p>\n"
   "<p><span class='label'>Lenguaje:</span> cadenas binarias con número impar de 0's y número impar de 1's.</p>\n"
   "<p><span class='label'>Estructura:</span> usa cuatro estados: q00, q10, q01 y q11. Cada estado representa si la cantidad de 0's y 1's leídos es par o impar.</p>\n"
   "<p><span class='label'>Funcionamiento:</span> por cada símbolo leído, el autómata cambia de estado dependiendo de si cambia la paridad de los 0's o de los 1's. Acepta únicamente cuando termina en q11.</p>\n"
   "<p class='syntax-title'>Sintaxis / definición de la máquina</p>\n"
   "<pre class='syntax'>" (sintaxis->html dfa-syntax) "</pre>\n"
   "<h3>Gráfico</h3>\n"
   "<img src='dfa-extra.png' alt='Grafico DFA'>\n"
   "<h3>Pruebas</h3>\n"
   "<pre>" (pruebas->html dfa-output) "</pre>\n"
   "</div>\n"

   "<h2>2. PDA - Autómata con Pila</h2>\n"
   "<div class='card'>\n"
   "<p><span class='label'>Tipo:</span> PDA, autómata con pila.</p>\n"
   "<p><span class='label'>Lenguaje:</span> L = { a^n b^n | n >= 1 }.</p>\n"
   "<p><span class='label'>Estructura:</span> usa estados start, s0, s1 y s2, además de una pila con símbolo base #.</p>\n"
   "<p><span class='label'>Funcionamiento:</span> por cada a leída se apila un símbolo. Al comenzar a leer b, la máquina desapila un símbolo por cada b. Si la cantidad coincide y llega al estado final, acepta.</p>\n"
   "<p class='syntax-title'>Sintaxis / definición de la máquina</p>\n"
   "<pre class='syntax'>" (sintaxis->html pda-syntax) "</pre>\n"
   "<h3>Gráfico</h3>\n"
   "<img src='pda-extra.png' alt='Grafico PDA'>\n"
   "<h3>Pruebas</h3>\n"
   "<pre>" (pruebas->html pda-output) "</pre>\n"
   "</div>\n"

   "<h2>3. LBA - Autómata Linealmente Acotado</h2>\n"
   "<div class='card'>\n"
   "<p><span class='label'>Tipo:</span> LBA, autómata linealmente acotado.</p>\n"
   "<p><span class='label'>Lenguaje:</span> L = { a^n b^n c^n | n >= 1 }.</p>\n"
   "<p><span class='label'>Estructura:</span> usa una cinta limitada al tamaño de la entrada. Marca a como X, b como Y y c como Z.</p>\n"
   "<p><span class='label'>Funcionamiento:</span> busca una a, una b y una c para marcarlas como grupo. Repite el proceso hasta que todos los símbolos estén marcados o hasta encontrar un error.</p>\n"
   "<p class='syntax-title'>Sintaxis / definición de la máquina</p>\n"
   "<pre class='syntax'>" (sintaxis->html lba-syntax) "</pre>\n"
   "<h3>Gráfico</h3>\n"
   "<img src='lba-extra.png' alt='Grafico LBA'>\n"
   "<h3>Pruebas</h3>\n"
   "<pre>" (pruebas->html lba-output) "</pre>\n"
   "</div>\n"

   "<h2>4. Máquina de Turing</h2>\n"
   "<div class='card'>\n"
   "<p><span class='label'>Tipo:</span> Máquina de Turing.</p>\n"
   "<p><span class='label'>Función:</span> transforma todos los símbolos 1 en X.</p>\n"
   "<p><span class='label'>Estructura:</span> usa una cinta, una cabeza de lectura/escritura, un estado inicial q0, un estado final qf y el blanco B.</p>\n"
   "<p><span class='label'>Funcionamiento:</span> si lee 1, escribe X y se mueve a la derecha. Si lee B, se detiene en el estado final.</p>\n"
   "<p class='syntax-title'>Sintaxis / definición de la máquina</p>\n"
   "<pre class='syntax'>" (sintaxis->html turing-syntax) "</pre>\n"
   "<h3>Gráfico</h3>\n"
   "<img src='turing-extra.png' alt='Grafico Maquina de Turing'>\n"
   "<h3>Pruebas</h3>\n"
   "<pre>" (pruebas->html turing-output) "</pre>\n"
   "</div>\n"

   "</body>\n"
   "</html>\n"))

;-----------ESCRIBIR HTML------------------
(call-with-output-file output-file
  (lambda (out)
    (display html-content out))
  #:exists 'replace)

(displayln "Archivo creado: files/maquinas-extra.html")
(displayln "Graficos creados:")
(displayln "files/dfa-extra.png")
(displayln "files/pda-extra.png")
(displayln "files/lba-extra.png")
(displayln "files/turing-extra.png")