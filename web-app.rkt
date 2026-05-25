#lang racket

(require web-server/servlet web-server/servlet-env net/base64 racket/file)
(require "config.rkt" "lexer.rkt" "parser.rkt" "validator.rkt" "graphviz.rkt" "html-generator.rkt")

(define default-code
  #<<EOS
states = q0, q1, q2, q3;
start = q0;
finals = q3;
alphabet = "axy01234567890-";
/* Empezamos a definir transiciones */
transitions:
  q0 ("axy") -> q1;
  q1 ("axy") -> q1;
  q1 ("-") -> q2;
  q2 ("0123456789") -> q3;
  q3 ("0123456789") -> q3;
end

check = "axy-012"; /* Se acepta */
check = "axy12"; /* NO se acepta */
check = "a-1"; /* Se acepta */
EOS
)

(define (render-page current-code current-type html-code html-checks img-base64)
  (define nfa-selected (if (equal? current-type "NFA") "selected" ""))
  (define dfa-selected (if (equal? current-type "DFA") "selected" ""))
  (define pda-selected (if (equal? current-type "PDA") "selected" ""))

  (response/full
   200 #"OK" (current-seconds) #"text/html; charset=utf-8" empty
   (list (string->bytes/utf-8
          (string-append
           #<<HTML
<!DOCTYPE html>
<html lang='es'>
<head>
  <meta charset='UTF-8'>
  <title>Analizador de lenguajes</title>
  <style>
    body { font-family: 'Segoe UI', sans-serif; background: #121212; color: #E0E0E0; padding: 30px; max-width: 1100px; margin: 0 auto; }
    h1, h2, h3 { color: #fff; font-weight: 400; margin-top: 0; }
    textarea { width: 100%; height: 250px; background: #1E1E1E; color: #fff; border: 1px solid #3A3A3A; padding: 15px; font-family: monospace; font-size: 14px; border-radius: 6px; box-sizing: border-box; }
    .selector-box { margin: 15px 0; background: #1A1A1A; padding: 12px; border-radius: 6px; border: 1px solid #2A2A2A; display: inline-block; }
    select { background: #2A2A2A; color: #fff; border: 1px solid #444; padding: 6px 12px; border-radius: 4px; font-size: 14px; cursor: pointer; font-family: sans-serif; }
    button { background: #fff; color: #000; border: none; padding: 10px 25px; font-size: 16px; cursor: pointer; border-radius: 4px; font-weight: bold; }
    .box { margin-top: 30px; background: #1A1A1A; border: 1px solid #2A2A2A; padding: 25px; border-radius: 8px; }
    .row { display: flex; flex-wrap: wrap; gap: 20px; margin-bottom: 25px; }
    .col { flex: 1; min-width: 300px; }
    .center { display: flex; flex-direction: column; align-items: center; border-top: 1px solid #2A2A2A; padding-top: 25px; }
    .code-view { background: #121212; padding: 15px; border-radius: 6px; font-family: monospace; white-space: pre-wrap; color: #CBCBCB; }
    .keyword { color: #A9DAFB; font-weight: bold; }
    .state { color: #679BD1; }
    .text, .quote { color: #C4947C; text-decoration: none !important; }
    .comment { color: #8E8E8E; font-style: italic; }
    .error { color: #F44747; text-decoration: none !important; }
    .accepted { color: #4E9F3D; font-weight: bold; }
    .rejected { color: #F44747; font-weight: bold; }
    .graph-img { max-width: 100%; width: 750px; background: #fff; padding: 15px; border-radius: 6px; margin-top: 10px; }
  </style>
</head>
<body>
  <h1>Automata Web Studio</h1>
  <form method='POST' action='/'>
    <textarea name='codigo'>
HTML
           current-code
           #<<HTML
</textarea><br>
    <div class='selector-box'>
      <label style='margin-right: 10px;'>Evaluar como:</label>
      <select name='tipo_automata'>
HTML
           "<option value='NFA' " nfa-selected ">NFA</option>"
           "<option value='DFA' " dfa-selected ">DFA</option>"
           "<option value='PDA' " pda-selected ">PDA</option>"
           "<option value='LBA' " (if (equal? current-type "LBA") "selected" "") ">LBA</option>"
           #<<HTML
      </select>
    </div>
    <div style='margin-top: 5px;'>
        <button type='submit'>Analizar Automata</button>
    </div>
  </form>
HTML
           (if html-code
               (string-append
                #<<HTML
  <div class='box'>
    <h2>Resultados</h2>
    <div class='row'>
      <div class='col'>
        <h3>Código de Entrada Resaltado:</h3>
        <div class='code-view'>
HTML
                html-code
                #<<HTML
</div>
      </div>
      <div class='col'>
        <h3>Simulación de Cadenas:</h3>
        <div style='line-height: 1.8;'>
HTML
                html-checks
                #<<HTML
</div>
      </div>
    </div>
    <div class='center'>
      <h3>Diagrama del Autómata:</h3>
      <img class='graph-img' src='data:image/png;base64,
HTML
                img-base64
                #<<HTML
' alt='Automata' />
    </div>
  </div>
HTML
                )
               "")
           #<<HTML
</body>
</html>
HTML
           )))))

(define (start request)
  (define bindings (request-bindings request))
  (if (exists-binding? 'codigo bindings)
      (let* ([code (string-replace (extract-binding/single 'codigo bindings) "\r\n" "\n")]
             [tipo (if (exists-binding? 'tipo_automata bindings)
                       (extract-binding/single 'tipo_automata bindings)
                       "NFA")]
             [all-tokens (tokenizer code tokens-table)]
             [clean-tokens (filter (lambda (t) 
                                     (not (or (equal? (first t) "commentBlock") 
                                              (equal? (first t) "commentLine")))) 
                                   all-tokens)]
             [automaton (parse-start clean-tokens)]
             [_ (ensure-output-folder)]
             [_ (generate-graphviz automaton output-dot output-png)]
             [img-b64 (bytes->string/utf-8 (base64-encode (file->bytes output-png) #"\n"))]
             [tokens-raw (tokens->html all-tokens)]
             [tokens-fixed (string-replace tokens-raw "class='error'>\"" "class='text'>\"")]
             [tokens-final (string-replace tokens-fixed "class='error'>&quot;" "class='text'>&quot;")])
        
        (render-page code tipo tokens-final (style-validations (validate-checks automaton tipo)) img-b64))
      
      (render-page default-code "NFA" #f #f "")))

(displayln "Servidor iniciado en: http://localhost:8080")
(serve/servlet start #:servlet-path "/" #:listen-ip "127.0.0.1" #:port 8080 #:command-line? #t)



