
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

(define dmxbuffer-blackout
  (foreign-lambda* bool
      ((dmxbuffer data))
    "C_return(data->Blackout());"))


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


;;; Example Program

;; ola::InitLogging(ola::OLA_LOG_WARN, ola::OLA_LOG_STDERR)

;; ola::DmxBuffer buffer
;; buffer.Blackout();

;; ola::client::StreamingClient ola_client(
;;     ola::client::StreamingClient::Options())

;; if (! ola_client.Setup()) {
;;     std::cerr << "Setup failed" << endl;
;;     exit(1);
;; }

;; for (unsigned int i = 0; i < 100; ++i) {
;;     buffer.SetChannel(0, i);
;;     if (! ola_client.SendDmx(universe, buffer)) {
;;         cout << "Send DMX failed" << endl;
;;         exit(1);
;;     }
;;     usleep(25000); // 25ms between frames
;; }
;; return 0;
