#lang racket/base
(require typed/sugar/define)

(define/typed+provide (bytecount->string bytecount)
  (Nonnegative-Integer . -> . String)
  (define (format-with-threshold threshold suffix)
    ;; upconvert by factor of 100 to get two digits after decimal
    (format "~a ~a" (exact->inexact (/ (round ((* bytecount 100) . / . threshold)) 100)) suffix))
  
  (define threshold-kilobyte 1000)
  (define threshold-megabyte (threshold-kilobyte . * . threshold-kilobyte))
  (define threshold-gigabyte (threshold-megabyte . * . threshold-kilobyte))
  (define threshold-terabyte (threshold-gigabyte . * . threshold-kilobyte))
  
  (cond
    [(bytecount . >= . threshold-terabyte) (format-with-threshold threshold-terabyte "TB")]
    [(bytecount . >= . threshold-gigabyte) (format-with-threshold threshold-gigabyte "GB")]
    [(bytecount . >= . threshold-megabyte) (format-with-threshold threshold-megabyte "MB")]
    [(bytecount . >= . threshold-kilobyte) (format-with-threshold threshold-kilobyte "KB")]
    [else (format "~a bytes" bytecount)]))


