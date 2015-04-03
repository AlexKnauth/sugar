#lang typed/racket/base
(require (except-in racket/list flatten) typed/sugar/define sugar/len)
;; use fully-qualified paths in require,
;; so they'll work when this file is included elsewhere
(provide (all-defined-out))

(define/typed+provide (trimf xs test-proc)
  (All (A) ((Listof A) (A . -> . Boolean) -> (Listof A)))
  (dropf-right (dropf xs test-proc) test-proc))

;; use case because polymorphic types don't do well with optional args
(define/typed+provide slicef-at
  (All (A) (case-> ((Listof A) (A . -> . Boolean) -> (Listof (Listof A)))
                   ((Listof A) (A . -> . Boolean) Boolean -> (Listof (Listof A)))))
  (case-lambda
    [(xs pred)
     (slicef-at xs pred #f)]
    [(xs pred force?)
     (define-values (last-list list-of-lists)
       (for/fold
        ([current-list : (Listof A) empty][list-of-lists : (Listof (Listof A)) empty])
        ([x (in-list xs)])
         (if (pred x)
             (values (cons x null) (if (not (empty? current-list))
                                       (cons (reverse current-list) list-of-lists)
                                       list-of-lists))
             (values (cons x current-list) list-of-lists))))
     (let ([list-of-lists (reverse (if (empty? last-list)
                                       list-of-lists
                                       (cons (reverse last-list) list-of-lists)))])
       (if (and force? (not (empty? list-of-lists)) (not (pred (caar list-of-lists))))
           (cdr list-of-lists)
           list-of-lists))]))

(define/typed+provide (slicef-after xs pred)
  (All (A) ((Listof A) (A . -> . Boolean) -> (Listof (Listof A))))
  (define-values (last-list list-of-lists)
    (for/fold ([current-list : (Listof A) empty][list-of-lists : (Listof (Listof A)) empty])
              ([x (in-list xs)])
      (if (pred x)
          (values empty (cons (reverse (cons x current-list)) list-of-lists))
          (values (cons x current-list) list-of-lists))))
  (reverse (if (empty? last-list)
               list-of-lists
               (cons (reverse last-list) list-of-lists))))


(define/typed+provide slice-at
  (All (A) (case-> ((Listof A) Positive-Integer -> (Listof (Listof A)))
                   ((Listof A) Positive-Integer Boolean -> (Listof (Listof A)))))
  (case-lambda
    [(xs len)
     (slice-at xs len #f)]
    [(xs len force?)
     (define-values (last-list list-of-lists)
       (for/fold ([current-list : (Listof A) empty][list-of-lists : (Listof (Listof A)) empty])
                 ([(x i) (in-indexed xs)])
         (if (= (modulo (add1 i) len) 0)
             (values empty (cons (reverse (cons x current-list)) list-of-lists))
             (values (cons x current-list) list-of-lists))))
     (reverse (if (or (empty? last-list) (and force? (not (= len (length last-list)))))
                  list-of-lists
                  (cons (reverse last-list) list-of-lists)))]))


(define/typed+provide (filter-split xs pred)
  (All (A) ((Listof A) (A . -> . Boolean) -> (Listof (Listof A))))
  (define-values (last-list list-of-lists)
    (for/fold ([current-list : (Listof A) empty][list-of-lists : (Listof (Listof A)) empty])
              ([x (in-list xs)])
      (if (pred x)
          (values empty (if (not (empty? current-list))
                            (cons (reverse current-list) list-of-lists)
                            list-of-lists))
          (values (cons x current-list) list-of-lists))))
  (reverse (if (not (empty? last-list))
               (cons (reverse last-list) list-of-lists)
               list-of-lists)))

(define/typed+provide (frequency-hash xs)
  (All (A) ((Listof A) -> (HashTable A Integer)))
  (define counter ((inst make-hash A Integer)))
  (for ([item (in-list xs)])
    (hash-update! counter item (λ([v : Integer]) (add1 v)) (λ _ 0)))
  counter)

(define/typed+provide (members-unique? x)
  (All (A) ((U (Listof A) (Vectorof A) String) -> Boolean))  
  (cond 
    [(list? x) (= (len (remove-duplicates x)) (len x))]
    [(vector? x) (->list x)]
    [(string? x) (string->list x)]
    [else (error (format "members-unique? cannot be determined for ~a" x))]))

