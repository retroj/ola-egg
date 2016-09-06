
# ola


## Description

Basic bindings for libola, Open Lighting Architecture.  Classes covered so
far are DmxBuffer and StreamingClient.

For bug reports, feature requests, and development versions, visit the
[github project page](https://github.com/retroj/ola-egg/).

## Authors

* John J Foerch


## Requirements
### Chicken Eggs

* r7rs
* srfi-99
* foreigners
* list-utils
* matchable

### C Headers

* ola (libola-dev)


## API
### Version

* **(ola-version) => (major minor revision)**

Return ola's version as a list.

* **(ola-version-string) => string**

Return ola's version as a string.

### Logging

* **(init-logging [level] [output]) => bool**

    Level may be one of the following symbols:

    * log-level/none
    * log-level/fatal
    * log-level/warn
    * log-level/info
    * log-level/debug
    * log-level/max

    Output may be one of the following symbols:

    * log-output/stderr
    * log-output/syslog
    * log-output/null

### DmxBuffer

* **(dmxbuffer) => dmxbuffer**

    Construct a new empty dmxbuffer.

* **(dmxbuffer other-dmxbuffer) => dmxbuffer**

    Construct a new dmxbuffer with the same contents as other-dmxbuffer.

* **(dmxbuffer bytevector) => dmxbuffer**

    Constructs a new dmxbuffer with the same contents as bytevector.

* **(dmxbuffer? dmxbuffer) => bool**

    Dmxbuffer predicate.

* **(dmxbuffer=? dmxbuffer-a dmxbuffer-b) => bool**

    Test whether dmxbuffer-a and dmxbuffer-b have equal contents.

* **(dmxbuffer-size dmxbuffer) => size**

    Size of dmxbuffer.

* **(dmxbuffer-get dmxbuffer) => bytevector**

    Return contents of dmxbuffer as a bytevector.

* **(dmxbuffer-get-channel dmxbuffer channel) => value**

    Return the value of the given channel in dmxbuffer.

* **(dmxbuffer-get-range dmxbuffer offset length) => bytevector**

    Return a bytevector of the requested range in dmxbuffer.

* **(dmxbuffer-set! dmxbuffer bytevector offset size) => bool**

    Set the contents of dmxbuffer to contents of bytevector at given offset and
    size.

* **(dmxbuffer-set! dmxbuffer bytevector) => bool**

    Set the contents of dmxbuffer to contents of bytevector.

* **(dmxbuffer-set! dmxbuffer dmxbuffer-other) => bool**

    Set the contents of dmxbuffer to contents of dmxbuffer-other.

* **(dmxbuffer-set-channel! dmxbuffer channel value) => undefined**

    Set dmxbuffer channel to value.

* **(dmxbuffer-set-from-string! dmxbuffer str) => bool**

    Complement of dmxbuffer->string.  Sets the contents of dmxbuffer
    according to the specially formatted string str.  The format of the
    string is integers separated by commas, where 0's may be omitted,
    e.g. "1,2,,255"

* **(dmxbuffer-set-range! dmxbuffer dst-offset bytevector) => bool**

    Set contents of dmxbuffer from dst-offset to contents of bytevector.

* **(dmxbuffer-set-range! dmxbuffer dst-offset bytevector src-offset src-length) => bool**

    Set contents of dmxbuffer from dst-offset to contents of bytevector, from
    src-offset, src-length bytes.

* **(dmxbuffer-set-range-to-value! dmxbuffer offset value length) => bool**

    Set contents of dmxbuffer from offset for length to value.

* **(dmxbuffer-htp-merge! dmxbuffer other-dmxbuffer) => bool**

    Perform an HTP merge (high value merge) with other-dmxbuffer into
    dmxbuffer.

* **(dmxbuffer-blackout! dmxbuffer) => bool**

    Set all channels to 0 in dmxbuffer.

* **(dmxbuffer-reset! dmxbuffer) => undefined**

    Reset dmxbuffer size to 0.

* **(dmxbuffer->string dmxbuffer) => string**

    Complement of 'dmxbuffer-set-from-string!'.  Returns a human-readable
    string representing the contents of dmxbuffer - comma separated
    decimal values.

### StreamingClient

* **(streamingclient [auto-start: bool] [server-port: port]) => streamingclient**

    Constructs a client and attempts to establish a connection to the ola
    daemon.  Auto-start is whether to start olad if it is not already
    running, default true.  Server-port is the port to use, default 9010.
    Signals an '(exn ola)' condition if it fails to connect to the ola
    daemon.

* **(streamingclient-stop streamingclient) => undefined**

    Stops a streamingclient.

* **(streamingclient-send-dmx streamingclient universe dmxbuffer) => bool**

    Sends contents of dmxbuffer to the given universe on streamingclient.


## Examples

```scheme
(use ola r7rs)
(let ((client (streamingclient auto-start: #f)))
  (streamingclient-send-dmx client 0 (dmxbuffer (string->utf8 "AeIoUaEiO"))))
```


## License

LGPL-3


## Version History

* 0.1 (2016-03-12) initial release
* 0.2 (2016-03-13) meta and documentation
* 0.3 (2016-03-13) streamingclient-setup removed, ola-version added
* 0.4 (2016-09-05) use bytevectors instead of blobs
