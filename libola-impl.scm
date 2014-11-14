
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

(define-record-type DmxBuffer
  (%DmxBuffer this)
  DmxBuffer?
  (this DmxBuffer-this DmxBuffer-this-set!))

(define (DmxBuffer)
  (%DmxBuffer
   ((foreign-lambda (c-pointer "ola::DmxBuffer")
                    "new ola::DmxBuffer"))))

(define-foreign-type DmxBuffer
  (instance ola::DmxBuffer DmxBuffer))

(define DmxBuffer.Blackout
  (foreign-lambda* bool
      ((DmxBuffer data))
    "C_return(data->Blackout());"))


;;
;; StreamingClient
;;

(define-record-type StreamingClient
  (%StreamingClient this)
  StreamingClient?
  (this StreamingClient-this StreamingClient-this-set!))

(define StreamingClient
  (let ((constructor1
         (foreign-lambda (c-pointer "ola::client::StreamingClient")
                         "new ola::client::StreamingClient"
                         bool)))
    (match-lambda*
     (() (%StreamingClient (constructor1 #t)))
     ((#t) (%StreamingClient (constructor1 #t)))
     ((#f) (%StreamingClient (constructor1 #f))))))

(define-foreign-type StreamingClient
  (instance ola::client::StreamingClient StreamingClient))

(define StreamingClient.Setup
  (foreign-lambda* bool
      ((StreamingClient client))
    "C_return(client->Setup());"))

(define StreamingClient.SendDmx
  (foreign-lambda* bool
      ((StreamingClient client) (unsigned-int universe) (DmxBuffer data))
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
