#lang racket


(require web-server/servlet)
(require web-server/servlet-env)


(require "config.rkt")
(require "lexer.rkt")
(require "parser.rkt")
(require "validator.rkt")
(require "graphviz.rkt")
(require "html-generator.rkt")

;-----------HTML DEL FORMULARIO INICIAL------------------
(define form-html
  #<<EOS
<!DOCTYPE html>
<html lang='es'>
<head>
  <meta charset='UTF-8'>
  <title>Automata Web Studio</title>
  <style>
    body { font-family: 'Segoe UI', -apple-system, sans-serif; background-color: #121212; color: #E0E0E0; padding: 40px; max-width: 900px; margin: 0 auto; }
    h1 { color: #ffffff; font-weight: 300; letter-spacing: 1px; }
    p { color: #A0A0A0; font-size: 15px; }
    textarea { width: 100%; height: 380px; background-color: #1E1E1E; color: #ffffff; border: 1px solid #3A3A3A; padding: 15px; font-family: 'Fira Code', 'Courier New', monospace; font-size: 14px; border-radius: 6px; box-sizing: border-box; line-height: 1.5; }
    textarea:focus { outline: none; border-color: #ffffff; }
    button { background-color: #ffffff; color: #000000; border: none; padding: 12px 24px; font-size: 16px; cursor: pointer; margin-top: 15px; border-radius: 4px; font-family: inherit; font-weight: bold; transition: background 0.2s; }
    button:hover { background-color: #747171; }
    .footer { margin-top: 30px; font-size: 12px; color: #555; text-align: center; }
  </style>
</head>
<body>
  <h1>Automata DFA y NFA</h1>
  <p>Edita el automata con la sintaxis que se muestra en el cuadro de texto.</p>
  <form method='POST' action='/'>
    <textarea name='codigo'>states = q0, q1, q2;

start = q0;

finals = q2;

alphabet = "01";

transitions :
  q0 ("0") -> q1;
  q1 ("1") -> q2;
  q2 ("0") -> q2;
  q2 ("1") -> q2;
end

check = "01";
check = "00";
check = "0101";</textarea>
    <br>
    <button type='submit'>Analizar Automata</button>
  </form>
</body>
</html>
EOS
)

;-----------FUNCIÓN DE RESPUESTA HTML------------------
(define (enviar-html texto-html)
  (response/full
   200 #"OK"
   (current-seconds) #"text/html; charset=utf-8"
   empty
   (list (string->bytes/utf-8 texto-html))))

;-----------PUNTO DE ENTRADA DEL SERVIDOR------------------
(define (start request)
  (define bindings (request-bindings request))
  
  (if (exists-binding? 'codigo bindings)
      
      ; --- FLUJO FELIZ DIRECTO ---
      (let* ([codigo-crudo (extract-binding/single 'codigo bindings)]
             
             ;Limpiamos retornos
             [codigo-texto (string-replace codigo-crudo "\r\n" "\n")]
             
             ;Tokenizador léxico
             [tokens (tokenizer codigo-texto tokens-table)]
             
             ;Parser por descenso recursivo
             [automaton (parse-start tokens)]
             
             ;Simulador de autómata
             [validations (validate-checks automaton)]
             
             ;Generamos el HTML con el resaltado de colores
             [html-texto (build-full-html tokens validations)]
             
             ;Limpiamos las etiquetas de cierre para poder incrustar la visualización
             [html-abierto (string-replace 
                            (string-replace
                              (string-replace html-texto "</html>" "")
                              "</body>" "")
                              "dfa.png"
                              "web-dfa.png")]
             
             ;Operaciones de archivos
             [_ (ensure-output-folder)]
             [web-output-dot "files/web-dfa.dot"]
             [web-output-png "files/web-dfa.png"]
             [_ (generate-graphviz automaton web-output-dot web-output-png)])
        
        ;Fusionamos todo en el diseño web final limpio
        (define html-final
          (string-append
           html-abierto 
           "<div style='white-space: normal; margin-top: 40px; border-top: 1px solid #333; padding-top: 25px; font-family: sans-serif; text-align: center;'>"
           "  <a href='/' style='color: #121212; background-color: #ffffff; text-decoration:none; padding: 12px 24px; border-radius: 4px; font-weight: bold; display: inline-block; box-shadow: 0 2px 5px rgba(0,0,0,0.3); transition: background 0.2s;'>"
           "    Volver al Editor"
           "  </a>"
           "</div>"
           "</body>"
           "</html>"
           )
        )
        
        (enviar-html html-final))
      
      ; Formulario inicial
      (enviar-html form-html)))

;-----------INICIAR EL SERVIDOR WEB LOCAL------------------
(displayln "http://localhost:8080")

(serve/servlet start
               #:servlet-path "/"
               #:listen-ip "127.0.0.1"
               #:port 8080
               #:extra-files-paths (list (build-path (current-directory) "files"))
               #:command-line? #t)