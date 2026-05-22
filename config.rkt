#lang racket

(provide input-file
         output-folder
         output-html
         output-dot
         output-png
         tokens-table
         read-input-file
         ensure-output-folder)

;-----------ARCHIVOS PRINCIPALES-------------------
(define input-file "input.txt")

;-----------RUTA AUTOMÁTICA-------------------
(define output-folder (build-path (current-directory) "files"))

;-----------ARCHIVOS DE SALIDA-------------------
(define output-html
  (build-path output-folder "automata-highlight.html"))

(define output-dot
  (build-path output-folder "dfa.dot"))

(define output-png
  (build-path output-folder "dfa.png"))

;-----------CREAR CARPETA files SI NO EXISTE-------------------
(define (ensure-output-folder)
  (unless (directory-exists? output-folder)
    (make-directory output-folder)))

;-----------LECTURA DE ARCHIVO INPUT-------------------
(define (read-input-file archivo)
  (if (file-exists? archivo)
      (file->string archivo)
      (error "cannot access to file")))

;-----------ALL-REGEX TOKENS-------------------
(define tokens-table
  '(
    ("space"        #rx"^[ \t\n\r]+")

    ("commentBlock" #rx"^/\\*([^*]|\\*+[^*/])*\\*+/")
    ("commentLine"  #rx"^#[^\n\r]*")

    ("kw-states"      #px"^states\\b")
    ("kw-start"       #px"^start\\b")
    ("kw-end"         #px"^end\\b")
    ("kw-alphabet"    #px"^alphabet\\b")  
    ("kw-transitions" #px"^transitions\\b")
    ("kw-check"       #px"^check\\b")
    ("kw-finals"      #px"^finals\\b")

    ("arrow"       #rx"^->")
    ("equal"       #rx"^=")
    ("colon"       #rx"^:")
    ("comma"       #rx"^,")
    ("semicolon"   #rx"^;")
    ("lparen"      #rx"^\\(")
    ("rparen"      #rx"^\\)")
    ("quotation-marks" #rx"^'")

    ("stateId"     #rx"^q[0-9]+")
    
    ("text"        #rx"^\"[^\"]*\"")
    ("string"      #rx"^'[^']*'")
  ))