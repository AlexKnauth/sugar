#lang typed/racket/base
(require typed/sugar/define typed/sugar/coerce)

(define/typed+provide (starts-with? str starter)
  (String String . -> . Boolean)
  (and (<= (string-length starter) (string-length str)) 
       (equal? (substring str 0 (string-length starter)) starter)))

(define/typed+provide (ends-with? str ender)
  (String String . -> . Boolean)
  (and (<= (string-length ender) (string-length str)) 
       (equal? (substring str (- (string-length str) (string-length ender)) (string-length str)) ender)))

(define/typed+provide (capitalized? str)
  (String . -> . Boolean)
  (char-upper-case? (car (string->list str))))
