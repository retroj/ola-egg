
# ola


## Description

Basic bindings for libola, Open Lighting Architecture.  Classes covered so
far are DmxBuffer and StreamingClient.


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

* **(dmxbuffer blob) => dmxbuffer**

    Contents a new dmxbuffer with the same contents as blob.

* **(dmxbuffer? dmxbuffer) => bool**

    Dmxbuffer predicate.

* **(dmxbuffer=? dmxbuffer-a dmxbuffer-b) => bool**

    Test whether dmxbuffer-a and dmxbuffer-b have equal contents.

* **(dmxbuffer-size dmxbuffer) => size**

    Size of dmxbuffer.

* **(dmxbuffer-get dmxbuffer) => blob**

    Return contents of dmxbuffer as a blob.

* **(dmxbuffer-get-channel dmxbuffer channel) => value**

    Return the value of the given channel in dmxbuffer.

* **(dmxbuffer-get-range dmxbuffer offset length) => blob**

    Return a blob of the requested range in dmxbuffer.

* **(dmxbuffer-set! dmxbuffer blob offset size) => bool**

    Set the contents of dmxbuffer to contents of blob at given offset and
    size.

* **(dmxbuffer-set! dmxbuffer blob) => bool**

    Set the contents of dmxbuffer to contents of blob.

* **(dmxbuffer-set! dmxbuffer dmxbuffer-other) => bool**

    Set the contents of dmxbuffer to contents of dmxbuffer-other.

* **(dmxbuffer-set-channel! dmxbuffer channel value) => undefined**

    Set dmxbuffer channel to value.

* **(dmxbuffer-set-from-string! dmxbuffer str) => bool**

    Complement of dmxbuffer->string.  Sets the contents of dmxbuffer
    according to the specially formatted string str.  The format of the
    string is integers separated by commas, where 0's may be omitted,
    e.g. "1,2,,255"

* **(dmxbuffer-set-range! dmxbuffer dst-offset blob) => bool**

    Set contents of dmxbuffer from dst-offset to contents of blob.

* **(dmxbuffer-set-range! dmxbuffer dst-offset blob src-offset src-length) => bool**

    Set contents of dmxbuffer from dst-offset to contents of blob, from
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
(use ola)
(let ((client (streamingclient auto-start: #f)))
  (streamingclient-send-dmx client 0 (dmxbuffer (string->blob "AeIoUaEiO"))))
```


## License

LGPL-3


## Version History

* 0.1 (2016-03-12) initial release
* 0.2 (2016-03-13) meta and documentation
