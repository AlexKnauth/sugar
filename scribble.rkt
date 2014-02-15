#lang racket
(require (for-syntax racket/base))
(require "coerce.rkt")


(provide when/block)

;; improves the syntax for conditional blocks in templates
;; ordinarily it would be @when[condition]{@list{stuff ...}}
;; now it can be @when/block[condition]{stuff ...}
;; has to be a macro otherwise body expressions will be evaluated regardless of condition
;; this is bad: if condition is false, expression should exit
(define-syntax (when/block stx)
  (syntax-case stx ()
    [(_ condition body ...)
  #'(if condition (string-append* (map ->string (list body ...))) "")]))
