
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

(define-library (ola)
  (export init-logging
          dmxbuffer
          dmxbuffer-get
          dmxbuffer-set! dmxbuffer-set-channel
          dmxbuffer-set-range!
          dmxbuffer-blackout dmxbuffer-size
          streamingclient-options
          streamingclient streamingclient-setup
          streamingclient-stop
          streamingclient-send-dmx)
  (import (scheme base)
          (scheme case-lambda)
          (srfi 4)
          (srfi 99)
          (only (chicken) and-let* blob-size getter-with-setter)
          (only (data-structures) alist-ref)
          (foreign)
          (foreigners)
          (only (list-utils) plist->alist)
          (matchable))
  (include "ola-impl"))
