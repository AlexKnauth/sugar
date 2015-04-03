#lang typed/racket/base
(require (for-syntax typed/racket/base racket/syntax))
(require typed/net/url racket/set racket/sequence racket/stream racket/dict)
(require typed/sugar/len typed/sugar/define)

(define-syntax-rule (make-coercion-error-handler target-format x)
  (λ(e) (error (format "Can’t convert ~a to ~a" x target-format))))


(define/typed+provide (->int x)
  (Any . -> . Integer)
  (with-handlers ([exn:fail? (make-coercion-error-handler 'integer x)])
    (cond
      [(or (integer? x) (real? x)) (inexact->exact (floor x))] 
      [(and (string? x) (> (len x) 0)) (->int (string->number x))]
      [(symbol? x) (->int (->string x))]
      [(char? x) (char->integer x)]
      [(path? x) (->int (->string x))]
      [else (len x)])))


(provide ->macrostring)
(define-syntax-rule (->macrostring x)
  (if (string? x)
      x ; fast exit for strings
      (with-handlers ([exn:fail? (make-coercion-error-handler 'string (format "~a (result of ~a" x 'x))])
        (cond
          [(or (equal? '() x) (void? x)) ""]
          [(symbol? x) (symbol->string x)]
          [(number? x) (number->string x)]
          [(path? x) (path->string x)]
          [(char? x) (format "~a" x)]
          [else (error)]))))

(define/typed+provide (->string x)
  (Any . -> . String)
  (if (string? x)
      x ; fast exit for strings
      (with-handlers ([exn:fail? (make-coercion-error-handler 'string x)])
        (cond
          [(or (equal? '() x) (void? x)) ""]
          [(symbol? x) (symbol->string x)]
          [(number? x) (number->string x)]
          [(path? x) (path->string x)]
          [(char? x) (format "~a" x)]
          [else (error)]))))


(define/typed+provide (->symbol x)
  (Any . -> . Symbol)
  (if (symbol? x)
      x
      (with-handlers ([exn:fail? (make-coercion-error-handler 'symbol x)])
        (string->symbol (->string x)))))


(define/typed+provide (->path x)
  (Any . -> . Path)
  (if (path? x)
      x 
      (with-handlers ([exn:fail? (make-coercion-error-handler 'path x)])
        (cond 
          [(url? x) (apply build-path (map path/param-path (url-path x)))]
          [else (string->path (->string x))]))))


(define/typed+provide (->url x)
  (Any . -> . URL) 
  (with-handlers ([exn:fail? (make-coercion-error-handler 'url x)])
    (string->url (->string x))))


(define/typed+provide (->complete-path x)
  (Any . -> . Path)
  (with-handlers ([exn:fail? (make-coercion-error-handler 'complete-path x)])
    (path->complete-path (->path x))))


(define/typed+provide (->list x)
  (Any . -> . (Listof Any))
  (if (list? x)
      x
      (with-handlers ([exn:fail? (make-coercion-error-handler 'list x)])
        (cond 
          [(string? x) (list x)]
          [(vector? x) (vector->list x)]
          [(set? x) (set->list x)]
          ;; location relevant because hash or dict are also sequences
          [(dict? x) (dict->list x)] 
          [(integer? x) (list x)] ; because an integer tests #t for sequence?
          [(sequence? x) (sequence->list x)]
          [(stream? x) (stream->list x)]
          [else (list x)]))))


(define/typed+provide (->vector x)
  (Any . -> . VectorTop)
  (if (vector? x)
      x
      (with-handlers ([exn:fail? (make-coercion-error-handler 'vector x)])
        (list->vector (->list x)))))


(define/typed+provide (->boolean x)
  (Any . -> . Boolean)
  (and x #t))


(define-syntax (make-*ish-predicate stx)
  (syntax-case stx ()
    [(_ stem Type)
     (with-syntax ([stemish? (format-id stx "~aish?" #'stem)]
                   [->stem (format-id stx "->~a" #'stem)])
       #`(begin
           (define/typed+provide (stemish? x)
             (Any . -> . Boolean : Type)
             (with-handlers ([exn:fail? (λ(e) #f)]) (and (->stem x) #t)))))]))

(make-*ish-predicate int Integer)
(make-*ish-predicate string String)
(make-*ish-predicate symbol Symbol)
(make-*ish-predicate url URL)
(make-*ish-predicate complete-path Path)
(make-*ish-predicate path Path)
(make-*ish-predicate list (Listof Any))
(make-*ish-predicate vector VectorTop)
;; no point to having list and vector here; they work with everything


