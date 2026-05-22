# Automata Syntax Highlight

Este proyecto lee un archivo `input.txt` que contiene la definición de un autómata finito, analiza su estructura, construye internamente el autómata, valida cadenas de prueba y genera una salida visual en HTML.

El programa está desarrollado en **Racket** y utiliza **Graphviz** para generar una imagen del autómata principal.

La salida principal del programa incluye archivos HTML con:

- El contenido del autómata resaltado con colores.
- Los resultados de las cadenas evaluadas.
- Una imagen del grafo del autómata principal.
- La ejecución de máquinas extra vistas durante el curso.

---

## Objetivo del proyecto

El objetivo principal es implementar un pequeño lenguaje para definir autómatas y procesarlo mediante varias etapas similares a las de un compilador básico:

1. Lectura del archivo fuente.
2. Análisis léxico mediante tokens.
3. Análisis sintáctico mediante descenso recursivo.
4. Construcción de una estructura interna del autómata.
5. Validación de cadenas.
6. Generación de una representación visual en HTML y Graphviz.

De esta forma, el proyecto no solo valida cadenas con un autómata, sino que también demuestra cómo se puede construir un procesador de lenguaje sencillo aplicando conceptos de lenguajes formales, análisis léxico, análisis sintáctico y simulación de máquinas de estados.

---

## Máquinas implementadas

Además del autómata principal, se agregaron máquinas extra vistas durante el curso:

1. **AFN con transición vacía**  
   Se encuentra integrado en `input.txt` y se muestra en `files/automata-highlight.html`. Este autómata reconoce cadenas binarias que terminan en `01`.

2. **PDA - Autómata con pila**  
   Se encuentra en `maquinas-extra/pda-anbn.rkt`. Reconoce el lenguaje `a^n b^n`, usando una pila para comparar la cantidad de `a` con la cantidad de `b`.

3. **Máquina de Turing**  
   Se encuentra en `maquinas-extra/turing-cambia-unos.rkt`. Simula una cinta y una cabeza lectora. El ejemplo implementado cambia todos los símbolos `1` por `X`.

4. **LBA - Autómata linealmente acotado**  
   Se encuentra en `maquinas-extra/lba-anbncn.rkt`. Reconoce el lenguaje `a^n b^n c^n`, usando una cinta limitada al tamaño de la entrada.

5. **AFD extra**  
   Se encuentra en `maquinas-extra/afd-impar-0-1.rkt`. Reconoce cadenas binarias con número impar de `0` y número impar de `1`.

Los resultados de las máquinas extra se muestran en:

```text
files/maquinas-extra.html

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
├── web-app.rkt
├── README.md
├── .gitignore
│
├── maquinas-extra/
│   ├── pda-anbn.rkt
│   ├── turing-cambia-unos.rkt
│   ├── lba-anbncn.rkt
│   ├── afd-impar-0-1.rkt
│   └── generar-html-maquinas-extra.rkt
│
└── files/
    ├── automata-highlight.html
    ├── maquinas-extra.html
    ├── dfa.dot
    └── dfa.png
    