#lang typed/racket/base
(require racket/list typed/sugar/define)
;; use fully-qualified paths in require,
;; so they'll work when this file is included elsewhere
(provide (all-defined-out))

(define/typed+provide (trimf xs test-proc)
  (All (A) ((Listof A) (A . -> . Boolean) -> (Listof A)))
  (dropf-right (dropf xs test-proc) test-proc))