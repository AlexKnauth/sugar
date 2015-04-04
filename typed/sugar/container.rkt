#lang typed/racket/base
(require typed/sugar/define)
(require typed/sugar/coerce typed/sugar/len #;racket/list racket/set #;racket/sequence #;racket/stream #;racket/dict)


(define-type (Containerof A) (U (Listof A) (Vectorof A) String Symbol HashTableTop Path (Sequenceof A)))

(define/typed+provide (gettable-container? x)
  (Any . -> . Boolean)
  (for/or ([proc (list sliceable-container? hash?)])
    (proc x)))


(define/typed+provide (sliceable-container? x)
  (Any . -> . Boolean)
  (for/or ([proc (list list? string? symbol? vector? path? (λ(i) (and (not (hash? i)) (sequence? i))))])
    (proc x)))

(define-predicate index? Index)

#|
(define/typed+provide get
  ;; (U Index A) = get key. Either a numerical index, or hashkey of type A
  (All (A) (case-> ((Containerof A) (U Index A) (Option Index) . -> . (U A (Containerof A)))
                   ((Containerof A) (U Index A) . -> . (U A (Containerof A)))))
  (case-lambda
    [(container start)
     (get container start #f)]
    [(container start end)
     ;; use handler to capture error & print localized error message
     (with-handlers ([exn:fail? (λ(exn) (error (format "get: couldn’t retrieve ~a from ~a" (if end (format "items ~a through ~a" start end) (format "item ~a" start)) container)))])
       (if (hash? container)
           (hash-ref container start)
           (let ()
             (define result 
               (let ([end (if (number? start) (add1 start) end)])
                 (cond
                   [(list? container) (for/list : (Listof A) ([i (in-range start end)]) (list-ref container i))]
                   [(vector? container) (for/vector ([i (in-range start end)]) (vector-ref container i))]
                   [(string? container) (substring container start end)]
                   [(symbol? container) (->symbol (get (->string container) start end))] 
                   [(path? container) (get (explode-path container) start end)] 
                   [(sequence? container) (get (->list container) start end)]
                   [else (error)])))
             ;; don't return single-item results inside a list
             ;; check for integer because integers don't have length
             (if (and (not (integer? result)) (= (len result) 1) (sliceable-container? container))
                 (car (->list result))
                 result))))]))

|#

(define (listlike-container? container)
  (ormap (λ([pred : (Any . -> . Boolean)]) (pred container)) (list vector? set? sequence?)))

(define/typed+provide (in? item container)
  (All (A) (A (U (Listof A) (Vectorof A)) -> Boolean))
  (cond
    [(list? container) (member item container)]
    [(hash? container) (hash-has-key? container item)]
    [(path? container) (in? (->path item) (explode-path container))]
    [(stringish? container) (regexp-match (->string item) (->string container))]
    ;; location relevant because dicts and strings are also listlike (= sequences)
    [(listlike-container? container) (in? item (->list container))]
    [else #f]))

