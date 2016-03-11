
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
;; Logging
;;

(define-foreign-enum-type (log-level (enum ola::log_level))
  (log-level->int int->log-level)
  (log-level/none ola::OLA_LOG_NONE)
  (log-level/fatal ola::OLA_LOG_FATAL)
  (log-level/warn ola::OLA_LOG_WARN)
  (log-level/info ola::OLA_LOG_INFO)
  (log-level/debug ola::OLA_LOG_DEBUG)
  (log-level/max ola::OLA_LOG_MAX))

(define-foreign-enum-type (log-output int)
  (log-output->int int->log-output)
  (log-output/stderr ola::OLA_LOG_STDERR)
  (log-output/syslog ola::OLA_LOG_SYSLOG)
  (log-output/null ola::OLA_LOG_NULL))

(foreign-declare
 "bool InitLoggingWrapper1 (ola::log_level level, int output) {"
 "  return(ola::InitLogging(level, static_cast<ola::log_output>(output)));"
 "}")

(define init-logging
  (case-lambda
   ((level output)
    ((foreign-lambda bool "InitLoggingWrapper1" log-level log-output)
     level output))
   ((level)
    (init-logging level 'log-output/stderr))
   (()
    (init-logging 'log-level/warn 'log-output/stderr))))


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
       "C_return(buffer->SetRange(i, &data[offset], length));")
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

(define-record-type :streamingclient-options
  (%streamingclient-options this)
  streamingclient-options?
  (this streamingclient-options-this streamingclient-options-this-set!))

(define streamingclient-options
  (let ((constructor
         (foreign-lambda* (c-pointer "ola::client::StreamingClient::Options")
             ((bool auto_start) (unsigned-short server_port))
           "ola::client::StreamingClient::Options *o = new ola::client::StreamingClient::Options();"
           "o->auto_start = auto_start;"
           "o->server_port = (uint16_t)server_port;"
           "C_return(o);")))
    (case-lambda
     (() (%streamingclient-options (constructor #f 9010)))
     ((auto-start) (%streamingclient-options (constructor auto-start 9010)))
     ((auto-start server-port) (%streamingclient-options (constructor auto-start server-port))))))

(define-foreign-type streamingclient-options
  (instance ola::client::StreamingClient::Options :streamingclient-options))

(define-record-type :streamingclient
  (%streamingclient this)
  streamingclient?
  (this streamingclient-this streamingclient-this-set!))

(define streamingclient
  (let ((constructor
         (foreign-lambda* (c-pointer "ola::client::StreamingClient")
             ((streamingclient-options options))
           "C_return(new ola::client::StreamingClient(*options));")))
    (match-lambda*
     (() (%streamingclient (constructor (streamingclient-options))))
     (((? boolean? auto-start))
      (%streamingclient (constructor (streamingclient-options auto-start))))
     (((? boolean? auto-start) (? number? server-port))
      (%streamingclient (constructor (streamingclient-options auto-start server-port))))
     (((? streamingclient-options? options))
      (%streamingclient (constructor options))))))

(define-foreign-type streamingclient
  (instance ola::client::StreamingClient :streamingclient))

(define streamingclient-setup
  (foreign-lambda* bool
      ((streamingclient client))
    "C_return(client->Setup());"))

(define streamingclient-stop
  (foreign-lambda* void
      ((streamingclient client))
    "client->Stop();"))

(define streamingclient-send-dmx
  (foreign-lambda* bool
      ((streamingclient client) (unsigned-int universe) (dmxbuffer data))
    "C_return(client->SendDmx(universe, *data));"))
