
(define-library (libola)
  (export DmxBuffer DmxBuffer.Blackout
          StreamingClient StreamingClient.Setup)
  (import (scheme base)
          (srfi 99)
          (only (chicken) case-lambda getter-with-setter)
          (only (data-structures) alist-ref)
          foreign
          matchable
          records)
  (include "libola-impl"))
