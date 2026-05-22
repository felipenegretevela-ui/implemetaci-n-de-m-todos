#lang racket

(provide sys-show-file)

;-----------ABRIR ARCHIVO SEGUN SISTEMA OPERATIVO------------------
(define (sys-show-file path)
  (cond
    [(equal? (system-type 'os) 'windows)
     (system (format "start \"\" \"~a\"" path))]

    [(equal? (system-type 'os) 'macosx)
     (system (format "open \"~a\"" path))]

    [else
     (system (format "xdg-open \"~a\"" path))]))