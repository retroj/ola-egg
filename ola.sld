
(define-library (ola)
  (export dmxbuffer
          dmxbuffer-set! dmxbuffer-set-channel
          dmxbuffer-blackout dmxbuffer-size
          streamingclient streamingclient-setup
          streamingclient-send-dmx)
  (import (scheme base)
          (scheme case-lambda)
          (srfi 4)
          (srfi 99)
          (only (chicken) getter-with-setter)
          (only (data-structures) alist-ref)
          foreign
          matchable)
  (include "ola-impl"))
