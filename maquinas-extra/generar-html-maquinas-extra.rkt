#lang racket

(require racket/runtime-path)

(define-runtime-path pda-file "pda.rkt")
(define-runtime-path turing-file "turing.rkt")
(define-runtime-path lba-file "lba.rkt")
(define-runtime-path afd-file "dfa.rkt")
(define-runtime-path output-file "../files/maquinas-extra.html")

(define (run-machine path)
  (with-output-to-string
    (lambda ()
      (dynamic-require path #f))))

(define pda-output (run-machine pda-file))
(define turing-output (run-machine turing-file))
(define lba-output (run-machine lba-file))
(define afd-output (run-machine afd-file))

(define html-content
  (string-append
   "<!DOCTYPE html>\n"
   "<html lang='es'>\n"
   "<head>\n"
   "<meta charset='UTF-8'>\n"
   "<meta name='viewport' content='width=device-width, initial-scale=1.0'>\n"
   "<title>Máquinas Extra</title>\n"
   "<style>\n"
   "body { font-family: Courier New, monospace; background:#1f1f1f; color:#f0f0f0; padding:20px; }\n"
   "h1 { color:#9ADAFB; }\n"
   "h2 { color:#F9D949; margin-top:30px; }\n"
   "pre { background:#111; padding:15px; border-radius:8px; color:#7CFFB2; }\n"
   "a { color:#9ADAFB; }\n"
   "</style>\n"
   "</head>\n"
   "<body>\n"

   "<h1>Máquinas extra implementadas</h1>\n"
   "<p>Este archivo muestra la ejecución de las máquinas adicionales agregadas al proyecto.</p>\n"
   "<p><a href='automata-highlight.html'>Ver autómata principal generado</a></p>\n"

   "<h2>1. PDA - Autómata con pila para a^n b^n</h2>\n"
   "<pre>" pda-output "</pre>\n"

   "<h2>2. Máquina de Turing - Cambia 1 por X</h2>\n"
   "<pre>" turing-output "</pre>\n"

   "<h2>3. LBA - Autómata linealmente acotado para a^n b^n c^n</h2>\n"
   "<pre>" lba-output "</pre>\n"

   "<h2>4. AFD extra - Número impar de 0's y número impar de 1's</h2>\n"
   "<pre>" afd-output "</pre>\n"

   "</body>\n"
   "</html>\n"))

(call-with-output-file output-file
  (lambda (out)
    (display html-content out))
  #:exists 'replace)

(displayln "Archivo creado: files/maquinas-extra.html")