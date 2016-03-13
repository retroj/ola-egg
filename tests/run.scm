
(use test)

(load "./ola.so")
(import ola)

(test-begin "ola")

(test-group
 "dmxbuffer"
 (test-assert "constructs dmxbuffer" (dmxbuffer? (dmxbuffer)))
 (test-assert "clone dmxbuffer" (dmxbuffer? (dmxbuffer (dmxbuffer)))))

(test-group
 "dmxbuffer-htp-merge!"
 (let ((a (dmxbuffer))
       (b (dmxbuffer)))
   (dmxbuffer-set-channel! a 0 255)
   (dmxbuffer-set-channel! a 1 0)
   (dmxbuffer-set-channel! b 0 255)
   (dmxbuffer-set-channel! b 1 255)
   (dmxbuffer-htp-merge! a b)
   (test "does merge" 255 (dmxbuffer-get a 1))))

(test-group
 "dmxbuffer-set-range-to-value!"
 (let ((a (dmxbuffer)))
   (dmxbuffer-set-range-to-value! a 0 3 2)
   (test "works" 3 (dmxbuffer-get a 1))))

(test-end "ola")

(test-exit)
