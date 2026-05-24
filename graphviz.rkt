#lang racket

(require racket/system)
(require racket/list)
(require racket/string)
(require racket/runtime-path) ; Manejo dinámico de rutas del sistema
(require net/base64)          ; Codificación de la imagen a texto Base64

(provide generate-graphviz
         automaton->dot
         collect-edges)

; Establecemos el directorio actual del script para asegurar el entorno
(define-runtime-path here ".")

;-----------REVISAR SI UN ESTADO ES FINAL------------------
(define (final-state? state finales)
  (member state finales))

;-----------CREAR NODO DOT------------------
(define (state->dot state start-state finales)
  (define is-final (final-state? state finales))
  (define is-start (equal? state start-state))
  
  (cond
    [is-final
     (string-append "  \"" state "\" [shape=doublecircle, style=filled, fillcolor=\"#DDF7E3\", color=\"#4E9F3D\", penwidth=2, fontname=\"Helvetica\"];\n")]
    [is-start
     (string-append "  \"" state "\" [shape=circle, style=filled, fillcolor=\"#E3F2FD\", color=\"#1E88E5\", penwidth=2.5, fontname=\"Helvetica-Bold\"];\n")]
    [else
     (string-append "  \"" state "\" [shape=circle, style=filled, fillcolor=\"#F5F5F5\", color=\"#BDBDBD\", penwidth=1.5, fontname=\"Helvetica\"];\n")]))

;-----------RECOLECTAR Y AGRUPAR ARISTAS------------------
(define (collect-edges automaton)
  (define states (hash-keys automaton))
  (define filtered-states
    (filter (lambda (k)
              (not (member k '("estados" "inicial" "finales" "alfabeto" "checks"))))
            states))
  
  (define edges-hash (make-hash))

  (for ([from-state filtered-states])
    (define transitions (hash-ref automaton from-state))
    (for ([symbol (hash-keys transitions)])
      (define dests (hash-ref transitions symbol))
      (define dest-list (if (list? dests) dests (list dests)))
      
      (for ([to-state dest-list])
        (define key (cons from-state to-state))
        (define label-str (if (equal? symbol "") "ε" (format "~a" symbol)))
        
        (if (hash-has-key? edges-hash key)
            (hash-set! edges-hash key (append (hash-ref edges-hash key) (list label-str)))
            (hash-set! edges-hash key (list label-str))))))
  edges-hash)

;-----------CONVERTIR ARISTAS A FORMATO DOT------------------
(define (edges->dot edges-hash)
  (define edge-strings
    (hash-map edges-hash
              (lambda (key labels)
                (define from-state (car key))
                (define to-state (cdr key))
                (define joint-label (string-join labels ", "))
                (string-append "  \"" from-state "\" -> \"" to-state "\" [label=\"" joint-label "\", color=\"#37474F\", fontcolor=\"#263238\"];\n"))))
  (apply string-append edge-strings))

;-----------CONVERTIR OBJETO AUTOMATA A TEXTO DOT------------------
(define (automaton->dot automaton)
  (define estados (hash-ref automaton "estados"))
  (define inicial (hash-ref automaton "inicial"))
  (define finales (hash-ref automaton "finales"))

  (define states-dot
    (apply string-append
           (map (lambda (st) (state->dot st inicial finales)) estados)))

  (define grouped-edges (collect-edges automaton))
  (define edges-dot (edges->dot grouped-edges))

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

;-----------GENERAR ARCHIVOS Y RETORNAR BASE64------------------
; Ahora se ejecuta en el parámetro local de 'here', llama a 'dot' directamente
; y retorna los bytes codificados en string.
(define (generate-graphviz automaton dot-path png-path)
  (define dot-content 
    (automaton->dot automaton))

  ; Forzamos el contexto del directorio actual para mitigar errores de paths relativos
  (parameterize ([current-directory here])
    ; 1. Escribir el código .dot en disco
    (call-with-output-file dot-path #:exists 'replace
      (lambda (out) (display dot-content out)))

    ; 2. Ejecutar comando nativo delegando la búsqueda de 'dot' al PATH del sistema operativo
    (define command (format "dot -Tpng \"~a\" -o \"~a\"" dot-path png-path))
    (system command)

    ; 3. Leer los bytes de la imagen generada y codificar a Base64
    (if (file-exists? png-path)
        (let* ([png-bytes (file->bytes png-path)]
               [base64-bytes (base64-encode png-bytes)])
          (bytes->string/latin-1 base64-bytes))
        (error 'graphviz "No se pudo generar la imagen del autómata con Graphviz. Verifica que esté instalado en las variables de entorno."))))