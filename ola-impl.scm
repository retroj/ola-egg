
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
;; Errors
;;

(define (ola-error loc msg . args)
  (signal
   (make-composite-condition
    (make-property-condition 'ola)
    (make-property-condition 'exn
                             'location loc
                             'message msg
                             'arguments args))))


;;
;; Foreign Instance Interface
;;
;; The 'instance' foreign type requires an object that can be passed to
;; (make t 'this pointer) and (slot-ref obj 'this).  We will be using
;; srfi-99 record-types for the scheme types, so we need to define these
;; two procedures.
;;

(define (make recordtype thisfield thisarg)
  ((rtd-constructor recordtype) thisarg))

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

(define-record-type :dmxbuffer
  (%dmxbuffer this)
  dmxbuffer?
  (this dmxbuffer-this dmxbuffer-this-set!))

(define-foreign-type dmxbuffer
  (instance ola::DmxBuffer :dmxbuffer))

(define (dmxbuffer . args)
  (let ((buffer
         (match args
           (() ((foreign-lambda dmxbuffer "new ola::DmxBuffer")))
           (((? dmxbuffer? buffer))
            ((foreign-lambda* dmxbuffer ((dmxbuffer buffer))
               "C_return(new ola::DmxBuffer(*buffer));")
             buffer))
           (((? blob? blob))
            ((foreign-lambda* dmxbuffer ((nonnull-blob data) (unsigned-int length))
               "C_return(new ola::DmxBuffer(data, length));")
             blob (blob-size blob))))))
    (set-finalizer! buffer (foreign-lambda* void ((dmxbuffer buffer))
                             "delete buffer;"))
    buffer))

(define dmxbuffer=?
  (foreign-lambda* bool ((dmxbuffer a) (dmxbuffer b))
    "C_return(*a == *b);"))

(define dmxbuffer-size
  (foreign-lambda* unsigned-int
      ((dmxbuffer buffer))
    "C_return(buffer->Size());"))

(define (dmxbuffer-get buffer)
  (let* ((size (dmxbuffer-size buffer))
         (blob (make-blob size)))
    ((foreign-lambda* void ((dmxbuffer buffer)
                            (nonnull-blob data)
                            (unsigned-int length))
       "buffer->Get(data, &length);")
     buffer blob size)
    blob))

(define dmxbuffer-get-channel
  (foreign-lambda* unsigned-byte
      ((dmxbuffer buffer) (unsigned-int channel))
    "C_return(buffer->Get(channel));"))

(define (dmxbuffer-get-range buffer offset length)
  (let ((blob (make-blob length)))
    ((foreign-lambda* void ((dmxbuffer buffer)
                            (nonnull-blob data)
                            (unsigned-int offset)
                            (unsigned-int length))
       "buffer->GetRange(offset, data, &length);")
     buffer blob offset length)
    blob))

(define dmxbuffer-set!
  (match-lambda*
   ((buffer (? blob? data) offset size)
    ((foreign-lambda* bool
         ((dmxbuffer buffer) (nonnull-blob data)
          (unsigned-int offset) (unsigned-int size))
       "C_return(buffer->Set(&data[offset], size));")
     buffer data offset size))
   ((buffer (? blob? data))
    (dmxbuffer-set! buffer data 0 (blob-size data)))
   ((buffer (? dmxbuffer? other))
    ((foreign-lambda* bool ((dmxbuffer buffer) (dmxbuffer other))
       "C_return(buffer->Set(*other));")
     buffer other))))

(define dmxbuffer-set-channel!
  (foreign-lambda* void
      ((dmxbuffer buffer) (unsigned-int channel) (unsigned-byte data))
    "buffer->SetChannel(channel, data);"))

(define dmxbuffer-set-from-string!
  (foreign-lambda* bool ((dmxbuffer buffer) (nonnull-c-string s))
    "std::string ss = s;"
    "C_return(buffer->SetFromString(ss));"))

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

(define dmxbuffer-set-range-to-value!
  (foreign-lambda* bool
      ((dmxbuffer buffer) (unsigned-int offset)
       (unsigned-byte data) (unsigned-int length))
    "C_return(buffer->SetRangeToValue(offset, data, length));"))

(define dmxbuffer-htp-merge!
  (foreign-lambda* bool
      ((dmxbuffer buffer) (dmxbuffer other))
    "C_return(buffer->HTPMerge(*other));"))

(define dmxbuffer-blackout!
  (foreign-lambda* bool
      ((dmxbuffer buffer))
    "C_return(buffer->Blackout());"))

(define dmxbuffer-reset!
  (foreign-lambda* void ((dmxbuffer buffer))
    "buffer->Reset();"))

(define dmxbuffer->string
  (foreign-lambda* c-string* ((dmxbuffer buffer))
    "std::string s = buffer->ToString();"
    "char* c = new char[s.size() + 1];"
    "std::copy(s.begin(), s.end(), c);"
    "c[s.size()] = 0;"
    "C_return(c);"))


;;
;; StreamingClient
;;

(define-record-type :streamingclient-options
  (%streamingclient-options this)
  streamingclient-options?
  (this streamingclient-options-this streamingclient-options-this-set!))

(define-foreign-type streamingclient-options
  (instance ola::client::StreamingClient::Options :streamingclient-options))

(define (streamingclient-options . keys)
  (let* ((constructor
          (foreign-lambda streamingclient-options
                          "new ola::client::StreamingClient::Options"))
         (options (constructor))
         (keys (plist->alist keys)))
    (set-finalizer! options (foreign-lambda* void ((streamingclient-options options))
                              "delete options;"))
    (and-let* ((auto-start (assq auto-start: keys)))
      ((foreign-lambda* void
           ((streamingclient-options options)
            (bool auto_start))
         "options->auto_start = auto_start;")
       options (cdr auto-start)))
    (and-let* ((server-port (assq server-port: keys)))
      ((foreign-lambda* void
           ((streamingclient-options options)
            (unsigned-short server_port))
         "options->server_port = (uint16_t)server_port;")
       options (cdr server-port)))
    options))

(define-record-type :streamingclient
  (%streamingclient this)
  streamingclient?
  (this streamingclient-this streamingclient-this-set!))

(define-foreign-type streamingclient
  (instance ola::client::StreamingClient :streamingclient))

(define (streamingclient . options)
  (let ((client ((foreign-lambda* streamingclient
                     ((streamingclient-options options))
                   "C_return(new ola::client::StreamingClient(*options));")
                 (apply streamingclient-options options))))
    (set-finalizer! client (foreign-lambda* void ((streamingclient client))
                             "delete client;"))
    (unless (streamingclient-setup client)
      (ola-error 'streamingclient "OLA StreamingClient setup failed"))
    client))

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
