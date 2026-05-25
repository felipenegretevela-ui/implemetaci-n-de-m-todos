#lang racket

(require racket/system racket/list racket/string racket/runtime-path)

(provide generate-graphviz
         automaton->dot
         collect-edges)

(define-runtime-path here ".")

;-----------CREAR NODO DOT SEGUN TIPO DE ESTADO------------------
(define (state->dot state start-state finales)
  (cond
    [(member state finales)
     (format "  \"~a\" [shape=doublecircle, style=filled, fillcolor=\"#DDF7E3\", color=\"#4E9F3D\", penwidth=2];\n" state)]

    [(equal? state start-state)
     (format "  \"~a\" [shape=circle, style=filled, fillcolor=\"#E3F2FD\", color=\"#1E88E5\", penwidth=2.5, fontname=\"Helvetica-Bold\"];\n" state)]

    [else
     (format "  \"~a\" [shape=circle, style=filled, fillcolor=\"#F5F5F5\", color=\"#BDBDBD\", penwidth=1.5];\n" state)]))

;-----------FORMATEAR ETIQUETA DE TRANSICION------------------
(define (format-edge-label joined-syms pop push dir)

  (define (clean str)
    (if (or (equal? (string-trim str) "")
            (equal? (string-trim str) "ε"))
        "ε"
        (string-trim str)))

  (match (list (clean pop) (clean push) (string-trim dir))
    [(list "ε" "ε" "")
     joined-syms]

    [(list p-pop p-push "")
     (format "~a, ~a → ~a" joined-syms p-pop p-push)]

    [(list _ p-push d)
     (format "~a → ~a, ~a" joined-syms p-push d)]))

;-----------RECOLECTAR Y AGRUPAR TRANSICIONES------------------
(define (collect-edges automaton)
  (for/fold ([out ""]) ([state (hash-ref automaton "estados")])

    (define grouped-edges
      (make-hash))

    ; Agrupar flechas que van al mismo estado con las mismas instrucciones
    (hash-for-each
     (hash-ref automaton state (hash))
     (lambda (sym dests)
       (for-each
        (lambda (dest-tuple)
          (hash-update! grouped-edges
                        dest-tuple
                        (lambda (syms) (cons sym syms))
                        '()))
        dests)))

    ; Generar strings de aristas
    (define edges-str
      (for/fold ([acc ""]) ([(dest-tuple syms) (in-hash grouped-edges)])
        (match dest-tuple
          [(list to-state pop push dir)
           (define joined-syms
             (string-join
              (sort
               (remove-duplicates
                (map
                 (lambda (s)
                   (if (equal? s "") "ε" s))
                 syms))
               string<?)
              ","))

           (string-append
            acc
            (format "  \"~a\" -> \"~a\" [label=\"~a\"];\n"
                    state
                    to-state
                    (format-edge-label joined-syms pop push dir)))])))

    (string-append out edges-str)))

;-----------CONVERTIR AUTOMATA A DOT------------------
(define (automaton->dot automaton)

  (define inicial
    (hash-ref automaton "inicial"))

  (define nodes
    (string-join
     (map
      (lambda (s)
        (state->dot s inicial (hash-ref automaton "finales")))
      (hash-ref automaton "estados"))
     ""))

  (string-append
   "digraph G {\n"
   "  rankdir=LR;\n"
   "  size=\"8,5\";\n"
   "  node [fontname=\"Helvetica\", fontsize=11, margin=0.1];\n"
   "  edge [arrowsize=0.7, arrowhead=vee, fontname=\"Courier New\", fontsize=12, fontcolor=\"#333333\", color=\"#777777\"];\n"
   "  qi [shape=point, width=0];\n"
   nodes
   "\n"
   "  qi -> \"" inicial "\" [color=\"#1E88E5\", penwidth=1.5];\n"
   (collect-edges automaton)
   "}\n"))

;-----------GENERAR ARCHIVO DOT Y PNG------------------
(define (generate-graphviz automaton dot-path png-path)
  (parameterize ([current-directory here])

    (call-with-output-file dot-path
      #:exists 'replace
      (lambda (out)
        (display (automaton->dot automaton) out)))

    (system
     (format "dot -Tpng \"~a\" -o \"~a\""
             dot-path
             png-path))))