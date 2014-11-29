
(foreign-declare "#include <ola/DmxBuffer.h>")
(foreign-declare "#include <ola/Logging.h>")
(foreign-declare "#include <ola/client/StreamingClient.h>")


;;
;; Foreign Instance Interface
;;
;; The 'instance' foreign type requires an object that can be passed to
;; (make t 'this pointer) and (slot-ref obj 'this).  We will be using
;; srfi-99 record-types for the scheme types, so we need to define these
;; two procedures.
;;

(define (make recordtype thisfield thisarg)
  (recordtype thisarg))

(define slot-ref
  (getter-with-setter
   (lambda (instance field)
     ((rtd-accessor (record-rtd instance) field) instance))
   (lambda (instance field value)
     ((rtd-mutator (record-rtd instance) field) instance value))))


;;
;; DmxBuffer
;;

(define-record-type dmxbuffer
  (%dmxbuffer this)
  dmxbuffer?
  (this dmxbuffer-this dmxbuffer-this-set!))

(define (dmxbuffer)
  (%dmxbuffer
   ((foreign-lambda (c-pointer "ola::DmxBuffer")
                    "new ola::DmxBuffer"))))

(define-foreign-type dmxbuffer
  (instance ola::DmxBuffer dmxbuffer))

(define dmxbuffer-set-channel
  (foreign-lambda* void
      ((dmxbuffer buffer) (unsigned-int channel) (unsigned-byte data))
    "buffer->SetChannel(channel, data);"))

(define dmxbuffer-blackout
  (foreign-lambda* bool
      ((dmxbuffer data))
    "C_return(data->Blackout());"))

(define dmxbuffer-size
  (foreign-lambda* unsigned-int
      ((dmxbuffer buffer))
    "C_return(buffer->Size());"))

;; dmxbuffer-set raw data + length, a string, or another dmxbuffer

;; dmxbuffer-set-from-string - comma-separated string giving channel values

;; dmxbuffer-set-range-to-value - offset + value + length

;; dmxbuffer-set-range - offset + data + length

;; dmxbuffer-set-channel - channel + value

;; dmxbuffer-get - dataptr + length
;; dmxbuffer-get - channel
;; dmxbuffer-get() -> string

;; dmxbuffer-get-range - channel + dataptr + length

;; dmxbuffer-reset

;; dmxbuffer-to-string



;;
;; StreamingClient
;;

(define-record-type streamingclient
  (%streamingclient this)
  streamingclient?
  (this streamingclient-this streamingclient-this-set!))

(define streamingclient
  (let ((constructor1
         (foreign-lambda (c-pointer "ola::client::StreamingClient")
                         "new ola::client::StreamingClient"
                         bool)))
    (match-lambda*
     (() (%streamingclient (constructor1 #t)))
     ((#t) (%streamingclient (constructor1 #t)))
     ((#f) (%streamingclient (constructor1 #f))))))

(define-foreign-type streamingclient
  (instance ola::client::StreamingClient streamingclient))

(define streamingclient-setup
  (foreign-lambda* bool
      ((streamingclient client))
    "C_return(client->Setup());"))

(define streamingclient-send-dmx
  (foreign-lambda* bool
      ((streamingclient client) (unsigned-int universe) (dmxbuffer data))
    "C_return(client->SendDmx(universe, *data));"))
