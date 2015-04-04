#lang racket/base
(require sugar/define)

(require-via-wormhole "../typed/sugar/container.rkt")

(provide+safe [get ((gettable-container? any/c) ((or/c (and/c integer? positive?) #f)) . ->* . any/c)])



