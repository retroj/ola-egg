
;; This file is part of ola-egg.
;; Copyright (C) 2014-2015  John J. Foerch
;;
;; ola-egg is free software: you can redistribute it and/or modify it
;; under the terms of the GNU Lesser General Public License as published
;; by the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; ola-egg is distributed in the hope that it will be useful, but WITHOUT
;; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
;; FITNESS FOR A PARTICULAR PURPOSE.  See the GNU Lesser General Public
;; License for more details.
;;
;; You should have received a copy of the GNU Lesser General Public
;; License along with ola-egg.  If not, see
;; <http://www.gnu.org/licenses/>.

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

(define dmxbuffer-set!
  (case-lambda
   ((dmxbuffer data offset size)
    ((foreign-lambda* bool
         ((dmxbuffer buffer) (nonnull-blob data)
          (unsigned-int offset) (unsigned-int size))
       "C_return(buffer->Set(&data[offset], size));")
     dmxbuffer data offset size))
   ((dmxbuffer data)
    (dmxbuffer-set! dmxbuffer data 0 (blob-size data)))))

;; dmxbuffer-set-range-to-value - offset + value + length

(define dmxbuffer-set-range!
  (case-lambda
   ((dmxbuffer i data offset length)
    ((foreign-lambda* bool
         ((dmxbuffer buffer) (unsigned-int i)
          (nonnull-blob data) (unsigned-int offset)
          (unsigned-int length))
       "C_return(buffer->SetRange(i, (uint8_t*)&data[offset], length));")
     dmxbuffer i data offset length))
   ((dmxbuffer i data)
    (dmxbuffer-set-range! dmxbuffer i data 0 (blob-size data)))))


;; dmxbuffer-set-channel - channel + value

;; dmxbuffer-get - dataptr + length
;; dmxbuffer-get() -> string

(define dmxbuffer-get
  (foreign-lambda* unsigned-byte
      ((dmxbuffer buffer) (unsigned-int channel))
    "C_return(buffer->Get(channel));"))

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
