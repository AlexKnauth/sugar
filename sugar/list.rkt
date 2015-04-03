#lang racket/base
(require (for-syntax racket/base)
         racket/list racket/set racket/function sugar/define)
(require #;"len.rkt" #;"coerce.rkt")

(require-via-wormhole "../typed/sugar/list.rkt")

(define (list-of-lists? xs) (and (list? xs) (andmap list? xs)))

(provide+safe [trimf (list? procedure? . -> . list?)]
              [slicef-at ((list? procedure?) (boolean?) . ->* . list-of-lists?)]
              [slicef-after (list? procedure? . -> . list-of-lists?)]
              [slice-at ((list? (and/c integer? positive?)) (boolean?) . ->* . list-of-lists?)]
              [filter-split (list? predicate/c . -> . list-of-lists?)]
              [frequency-hash (list? . -> . hash?)]
              #;[members-unique? ((or/c list? vector? string?) . -> . boolean?)])



#|

;; for use inside quasiquote
;; instead of ,(when ...) use ,@(when/splice ...)
;; to avoid voids
(provide when/splice)
(define-syntax (when/splice stx)
  (syntax-case stx ()
    [(_ test body)
     #'(if test (list body) '())]))


(provide values->list)
(define-syntax (values->list stx)
  (syntax-case stx ()
    [(_ values-expr) #'(call-with-values (λ () values-expr) list)]))


(define+provide/contract (sublist xs i j)
  (list? (and/c integer? (not/c negative?)) (and/c integer? (not/c negative?)) . -> . list?)
  (cond
    [(> j (length xs)) (error 'sublist (format "ending index ~a exceeds length of list" j))]
    [(>= j i) (take (drop xs i) (- j i))]
    [else (error 'sublist (format "starting index ~a is larger than ending index ~a" i j))]))

(define increasing-nonnegative? (λ(xs) (apply < -1 xs)))
(define increasing-nonnegative-list? (and/c list? increasing-nonnegative?))

(define+provide/contract (break-at xs bps)
  (list? (and/c coerce/list? (or/c empty? increasing-nonnegative-list?)) . -> . list-of-lists?)
  (when (ormap (λ(bp) (>= bp (length xs))) bps)
    (error 'break-at (format "breakpoint in ~v is greater than or equal to input list length = ~a" bps (length xs))))
  ;; easier to do back to front, because then the list index for each item won't change during the recursion
  ;; cons a zero onto bps (which may already start with zero) and then use that as the terminating condition
  ;; because breaking at zero means we've reached the start of the list
  (reverse (let loop ([xs xs][bps (reverse (cons 0 bps))])
             (if (= (car bps) 0)
                 (cons xs null) ; return whatever's left, because no more splits are possible
                 (let-values ([(head tail) (split-at xs (car bps))])
                   (cons tail (loop head (cdr bps))))))))


(define (integers? x)
  (and (list? x) (andmap integer? x)))

(define+provide/contract (shift xs shift-amount-or-amounts [fill-item #f] [cycle? #f])
  ((list? (or/c integer? integers?)) (any/c boolean?) . ->* . list?)
  
  (define (do-shift xs how-far)
    (define abs-how-far (abs how-far))
    (cond 
      [(> abs-how-far (length xs)) (error 'shift "index is too large for list\nindex: ~a\nlist: ~v" how-far xs)]
      [(= how-far 0) xs]
      [(positive? how-far) (append (make-list abs-how-far fill-item) (drop-right xs abs-how-far))]
      ;; otherwise how-far is negative
      [else (append (drop xs abs-how-far) (make-list abs-how-far fill-item))]))
  
  (if (list? shift-amount-or-amounts)
      (map (curry do-shift xs) shift-amount-or-amounts)
      (do-shift xs shift-amount-or-amounts)))


(define+provide/contract (shift/values xs shift-amount-or-amounts [fill-item #f])
  ((list? (or/c integer? integers?)) (any/c) . ->* . any)
  (apply (if (list? shift-amount-or-amounts) 
             values
             (λ xs xs)) 
         (shift xs shift-amount-or-amounts fill-item)))


|#