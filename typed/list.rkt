#lang typed/racket/base
(require racket/list)

(provide trimf)
(: trimf (All (A) ((Listof A) (A . -> . Boolean) -> (Listof A)))) 
(define (trimf xs test-proc)
  (dropf-right (dropf xs test-proc) test-proc))