#lang racket

(require racket/system)
(require racket/list)
(require racket/string)
(require racket/path)

(provide generate-graphviz
         automaton->dot
         collect-edges)

#|
Entiendo que meramente lo que se cambiaron fueron los atributos estéticos y 
de formato del archivo .dot, para que se vea más bonito y legible, con nodos redondos
y bordes más limpios para los estados normales, formas de doble círculo para los estados finales, 
y un estilo destacado para el estado inicial.
|#

;-----------REVISAR SI UN ESTADO ES FINAL------------------
(define (final-state? state finales)
  (member state finales))

;-----------CREAR NODO DOT------------------
(define (state->dot state start-state finales)
  (define is-final (final-state? state finales))
  (define is-start (equal? state start-state))
  
  (cond
    [is-final
     (string-append
      "  \"" state "\" [shape=doublecircle, style=filled, fillcolor=\"#DDF7E3\", color=\"#4E9F3D\", penwidth=2, fontname=\"Helvetica\"];\n")]
    
    [is-start
     (string-append
      "  \"" state "\" [shape=circle, style=filled, fillcolor=\"#E3F2FD\", color=\"#1E88E5\", penwidth=2.5, fontname=\"Helvetica-Bold\"];\n")]
    
    [else
     (string-append
      "  \"" state "\" [shape=circle, style=filled, fillcolor=\"#F5F5F5\", color=\"#757575\", penwidth=1.5, fontname=\"Helvetica\"];\n")]))

;-----------AGREGAR UNA ARISTA AGRUPADA------------------
(define (add-grouped-edge grouped from-state symbol to-state)
  (define key (list from-state to-state))
  (define old-symbols (hash-ref grouped key '()))
  (hash-set grouped key (remove-duplicates (cons symbol old-symbols))))

;-----------COLECTAR TODAS LAS TRANSICIONES DEL AUTOMATA------------------
(define (collect-edges automaton)
  (define estados (hash-ref automaton "estados"))
  
  (foldl
   (lambda (state grouped-acc)
     (if (hash-has-key? automaton state)
         (let ([transitions (hash-ref automaton state)])
           (foldl
            (lambda (symbol acc-inner)
              (define dest (hash-ref transitions symbol))
              
              ; dest puede ser un solo estado o una lista de estados
              (define dest-list
                (if (list? dest)
                    dest
                    (list dest)))
              
              (foldl
               (lambda (single-dest final-acc)
                 (add-grouped-edge final-acc state symbol single-dest))
               acc-inner
               dest-list))
            grouped-acc
            (hash-keys transitions)))
         grouped-acc))
   (hash)
   estados))

;-----------CONVERTIR EL HASH DE ARISTAS A SINTAXIS DOT------------------
(define (edges->dot grouped-edges)
  (foldl
   (lambda (key accum)
     (define from-state (first key))
     (define to-state (second key))
     (define symbols (hash-ref grouped-edges key))
     (define label (string-join symbols ", "))
     
     (string-append
      accum
      "  \"" from-state "\" -> \"" to-state "\" [label=\"  "
      label
      " \", fontname=\"Helvetica\", fontsize=10, color=\"#555555\", penwidth=1.2];\n"))
   ""
   (hash-keys grouped-edges)))

;-----------TRANSFORMAR AUTOMATA COMPLETO A CODIGO DOT------------------
(define (automaton->dot automaton)
  (define estados (hash-ref automaton "estados"))
  (define inicial (hash-ref automaton "inicial"))
  (define finales (hash-ref automaton "finales"))

  ; Construimos la definición de nodos
  (define states-dot
    (apply string-append
           (map (lambda (st)
                  (state->dot st inicial finales))
                estados)))

  ; Agrupamos y construimos las aristas
  (define grouped-edges (collect-edges automaton))
  (define edges-dot (edges->dot grouped-edges))

  ; Formateamos el archivo dot
  (string-append
   "digraph G {\n"
   "  rankdir=LR;\n"
   "  size=\"8,5\";\n"
   "  node [fontname=\"Helvetica\", fontsize=11];\n"
   "  edge [arrowsize=0.8, arrowhead=vee];\n"
   "  qi [shape=point, width=0];\n"
   states-dot
   "\n"
   "  qi -> \"" inicial "\" [color=\"#1E88E5\", penwidth=1.5];\n"
   edges-dot
   "}\n"))

;-----------BUSCAR EJECUTABLE DOT------------------
(define (get-dot-executable)
  (cond
    [(find-executable-path "dot") (find-executable-path "dot")]
    [(file-exists? "/opt/homebrew/bin/dot") "/opt/homebrew/bin/dot"]
    [(file-exists? "/usr/local/bin/dot") "/usr/local/bin/dot"]
    [(file-exists? "/usr/bin/dot") "/usr/bin/dot"]
    [else
     (error 'graphviz
            "No se encontró el comando 'dot' de Graphviz instalado en el sistema")]))

;-----------GENERAR ARCHIVOS GRAPHVIZ------------------
(define (generate-graphviz automaton dot-path png-path)
  (define dot-text (automaton->dot automaton))

  ; Aseguramos que la carpeta de salida exista
  (define dot-dir (path-only dot-path))
  (define png-dir (path-only png-path))

  (when (and dot-dir (not (directory-exists? dot-dir)))
    (make-directory* dot-dir))

  (when (and png-dir (not (directory-exists? png-dir)))
    (make-directory* png-dir))

  ; Guardamos el .dot
  (call-with-output-file dot-path
    (lambda (out)
      (display dot-text out))
    #:exists 'replace)

  ; Compilamos a imagen .png
  (define dot-exe (get-dot-executable))
  (define ok?
    (system* dot-exe
             "-Tpng"
             (path->string dot-path)
             "-o"
             (path->string png-path)))

  ; Mensajes de depuración
  (displayln (string-append "DOT creado en: " (path->string dot-path)))

  (if ok?
      (displayln (string-append "PNG creado en: " (path->string png-path)))
      (displayln "No se pudo generar el PNG con Graphviz"))

  (void))