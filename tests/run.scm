
(use (srfi 1 4) test)

(define (load-ola-libs libs)
  (for-each
   (lambda (lib)
     (print "Loading " lib)
     (load lib))
   libs))

(cond
 ((find (lambda (libs) (every file-exists? libs))
        (list '("./ola.so" "./ola.import.so")
              '("../ola.so" "../ola.import.so")))
  => load-ola-libs)
 (else
  (print "Warning: testing system-installed ola egg")
  (use ola)))

(import ola)

(test-begin "ola")

(test-group
 "dmxbuffer"
 (test-assert "constructs dmxbuffer" (dmxbuffer? (dmxbuffer)))
 (test-assert "clone dmxbuffer" (dmxbuffer? (dmxbuffer (dmxbuffer))))
 (test-assert "from bytevector" (dmxbuffer? (dmxbuffer (u8vector #x20))))
 (test "from bytevector (2)" #x20 (dmxbuffer-get-channel (dmxbuffer (u8vector #x20)) 0)))

(test-group
 "dmxbuffer-htp-merge!"
 (let ((a (dmxbuffer))
       (b (dmxbuffer)))
   (dmxbuffer-set-channel! a 0 255)
   (dmxbuffer-set-channel! a 1 0)
   (dmxbuffer-set-channel! b 0 255)
   (dmxbuffer-set-channel! b 1 255)
   (dmxbuffer-htp-merge! a b)
   (test "does merge" 255 (dmxbuffer-get-channel a 1))))

(test-group
 "dmxbuffer-set-range-to-value!"
 (let ((a (dmxbuffer)))
   (dmxbuffer-set-range-to-value! a 0 3 2)
   (test "works" 3 (dmxbuffer-get-channel a 1))))

(test-group
 "dmxbuffer->string"
 (test-assert "returns string" (string? (dmxbuffer->string (dmxbuffer)))))

(test-group
 "dmxbuffer-get"
 (test-assert "returns bytevector" (u8vector? (dmxbuffer-get (dmxbuffer))))
 (test "has expected contents" '(#x20) (u8vector->list (dmxbuffer-get (dmxbuffer (u8vector #x20))))))

(test-group
 "dmxbuffer-get-range"
 (test-assert "returns bytevector" (u8vector? (dmxbuffer-get-range (dmxbuffer) 0 1)))
 (test "has expected contents" '(#x20)
       (u8vector->list (dmxbuffer-get-range (dmxbuffer (u8vector #x20 #x21 #x22)) 0 1))))

(test-group
 "dmxbuffer=?"
 (let ((a (dmxbuffer (u8vector #x20 #x21 #x22)))
       (b (dmxbuffer (u8vector #x20 #x21 #x22)))
       (c (dmxbuffer (u8vector #x20 #x21 #x23))))
   (test-assert "identity" (dmxbuffer=? a a))
   (test-assert "contents" (dmxbuffer=? a b))
   (test-assert "contents !=" (not (dmxbuffer=? a c)))))

(test-group
 "dmxbuffer-set-from-string!"
 (let ((a (dmxbuffer)))
   (dmxbuffer-set-from-string! a "123")
   (test "produces expected contents" 123 (dmxbuffer-get-channel a 0))))

(test-group
 "ola-version"
 (test-assert "ola-version-string returns a string" (string? (ola-version-string)))
 (test-assert "returns a list" (list? (ola-version))))

(test-end "ola")

(test-exit)
