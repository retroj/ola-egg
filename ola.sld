
(define-library (ola)
  (export dmxbuffer
          dmxbuffer-set-channel
          dmxbuffer-blackout dmxbuffer-size
          streamingclient streamingclient-setup
          streamingclient-send-dmx)
  (import (scheme base)
          (srfi 99)
          (only (chicken) case-lambda getter-with-setter)
          (only (data-structures) alist-ref)
          foreign
          matchable)
  (include "ola-impl"))
