
(use (srfi 1) test)

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
 (test-assert "from blob" (dmxbuffer? (dmxbuffer (string->blob " "))))
 (test "from blob (2)" #x20 (dmxbuffer-get-channel (dmxbuffer (string->blob " ")) 0)))

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
 (test-assert "returns blob" (blob? (dmxbuffer-get (dmxbuffer))))
 (test "has expected contents" " " (blob->string (dmxbuffer-get (dmxbuffer (string->blob " "))))))

(test-group
 "dmxbuffer-get-range"
 (test-assert "returns blob" (blob? (dmxbuffer-get-range (dmxbuffer) 0 1)))
 (test "has expected contents" "1"
       (blob->string (dmxbuffer-get-range (dmxbuffer (string->blob "123")) 0 1))))

(test-group
 "dmxbuffer=?"
 (let ((a (dmxbuffer (string->blob "123")))
       (b (dmxbuffer (string->blob "123")))
       (c (dmxbuffer (string->blob "124"))))
   (test-assert "identity" (dmxbuffer=? a a))
   (test-assert "contents" (dmxbuffer=? a b))
   (test-assert "contents !=" (not (dmxbuffer=? a c)))))

(test-group
 "dmxbuffer-set-from-string!"
 (let ((a (dmxbuffer)))
   (dmxbuffer-set-from-string! a "123")
   (test "produces expected contents" 123 (dmxbuffer-get-channel a 0))))

(test-end "ola")

(test-exit)
