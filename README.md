<<<<<<< HEAD
# Automata Syntax Highlight

Este proyecto lee un archivo `input.txt` que contiene la definición de un autómata finito determinista, analiza su estructura, construye internamente el autómata, valida cadenas de prueba y genera una salida visual en HTML.

El programa está desarrollado en **Racket** y utiliza **Graphviz** para generar una imagen del autómata.

La salida principal del programa es un archivo HTML con:

- El contenido del autómata resaltado con colores.
- Los resultados de las cadenas evaluadas.
- Una imagen del grafo del autómata.

---

## Objetivo del proyecto

El objetivo principal es implementar un pequeño lenguaje para definir autómatas y procesarlo mediante varias etapas similares a las de un compilador básico:

1. Lectura del archivo fuente.
2. Análisis léxico mediante tokens.
3. Análisis sintáctico mediante descenso recursivo.
4. Construcción de una estructura interna del autómata.
5. Validación de cadenas.
6. Generación de una representación visual en HTML y Graphviz.

De esta forma, el proyecto no solo valida cadenas con un autómata, sino que también demuestra cómo se puede construir un procesador de lenguaje sencillo.

---

## Estructura general del proyecto

```text
proyecto/
│
├── input.txt
├── main.rkt
├── config.rkt
├── lexer.rkt
├── automaton.rkt
├── parser.rkt
├── validator.rkt
├── graphviz.rkt
├── html-generator.rkt
├── system-utils.rkt
│
└── files/
    ├── automata-highlight.html
    ├── dfa.dot
    └── dfa.png
=======
# implemetaci-n-de-m-todos
Proyecto de Racket para tokenizar, construir autómatas, validar cadenas y generar HTML.
>>>>>>> de47c65db521000f99741a315a7583969212dc62
