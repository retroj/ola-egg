
(define-library (ola)
  (export dmxbuffer dmxbuffer-blackout
          streamingclient streamingclient-setup)
  (import (scheme base)
          (srfi 99)
          (only (chicken) case-lambda getter-with-setter)
          (only (data-structures) alist-ref)
          foreign
          matchable
          records)
  (include "ola-impl"))
