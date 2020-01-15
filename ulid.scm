;;; ulid.scm
;;
;; Copyright (c) 2019 OOHASHI Daichi
;;
;; Permission is hereby granted, free of charge, to any person obtaining a copy
;; of this software and associated documentation files (the "Software"), to deal
;; in the Software without restriction, including without limitation the rights
;; to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
;; copies of the Software, and to permit persons to whom the Software is
;; furnished to do so, subject to the following conditions:
;; The above copyright notice and this permission notice shall be included in
;; all copies or substantial portions of the Software.
;;
;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
;; IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
;; FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
;; AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
;; LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
;; OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
;; THE SOFTWARE.

(define-module ulid
  (export <ulid> make-ulid ulid?
          ulid-timestamp ulid-random
          ulid-time
          ulid->string string->ulid
          ulid->bytevector bytevector->ulid
          ulid:current-time
          ulid:urandom-read-integer ulid:random-integer
          &ulid-error ulid-error?
          ulid-error-who ulid-error-irritants
          &ulid-parse-error ulid-parse-error?
          &ulid-range-error ulid-range-error?
          ulid-range-error-component ulid-range-error-min ulid-range-error-max
          )
  (use gauche.record)
  (use gauche.uvector)
  (use srfi-13)
  (use srfi-19)
  (use srfi-27)
  (use srfi-141)
  (use srfi-146.hash)
  (use srfi-151)
  (use binary.io)
  )

(select-module ulid)

(define-record-type <ulid> %make-ulid ulid?
  (timestamp ulid-timestamp)
  (random ulid-random))

(define-constant ulid-timestamp-byte-size 6)

(define-constant ulid-timestamp-bit-size (* 8 ulid-timestamp-byte-size))

(define-constant ulid-random-byte-size 10)

(define-constant ulid-random-bit-size (* 8 ulid-random-byte-size))

(define-constant ulid-byte-size
  (+ ulid-timestamp-byte-size ulid-random-byte-size))

(define-condition-type &ulid-error
    &error
    ulid-error?
  (who ulid-error-who)
  (irritants ulid-error-irritants))

(define-condition-type &ulid-parse-error
    &ulid-error
    ulid-parse-error?)

(define (ulid-parse-error who msg . irritants)
  (raise (condition
          (&message
           (message (format "~A: ~A: ~S" who msg irritants)))
          (&ulid-parse-error
           (who who)
           (irritants irritants)))))

(define-condition-type &ulid-range-error
    &ulid-error
    ulid-range-error?
  (component ulid-range-error-component)
  (min ulid-range-error-min)
  (max ulid-range-error-max))

(define (check-ulid-component-range component min max)
  (lambda (who v)
    (unless (and (<= min v) (< v max))
      (raise (condition
              (&message
               (message (format "~A: ~A out of range: ~A is not in [~A, ~A)"
                                who component v min max)))
              (&ulid-range-error
               (who who)
               (irritants (list v))
               (component component)
               (min min)
               (max max)))))))

(define check-valid-ulid-timestamp
  (check-ulid-component-range 'timestamp 0 (expt 2 ulid-timestamp-bit-size)))

(define check-valid-ulid-random
  (check-ulid-component-range 'random 0 (expt 2 ulid-random-bit-size)))

(define make-ulid
  (case-lambda
    (() (make-ulid #f))
    ((timestamp) (make-ulid timestamp #f))
    ((timestamp random)
     (let* ((t (or timestamp (ulid:current-time)))
            (r (if random
                   (or (and (integer? random) random)
                       (and (procedure? random)
                            (random ulid-random-byte-size t)))
                   (or (ulid:urandom-read-integer ulid-random-byte-size t)
                       (ulid:random-integer ulid-random-byte-size t)))))
       (assume-type t <integer>)
       (assume-type r <integer>)
       (check-valid-ulid-timestamp 'make-ulid t)
       (check-valid-ulid-random 'make-ulid r)
       (%make-ulid t r)))))

(define-constant milli #e1e-3)

(define-constant nano #e1e-9)

(define (ulid:current-time)
  (time-millisecond (current-time time-utc)))

(define (time-millisecond time)
  (+ (* (time-second time) (/ milli))
     (round-quotient (time-nanosecond time) (/ milli nano))))

(define (ulid-time ulid)
  (assume-type ulid <ulid>)
  (let-values (((sec msec) (quotient&remainder (ulid-timestamp ulid) (/ milli))))
    (make-time time-utc
               (* msec (/ milli nano))
               sec)))

(define (ulid->string ulid)
  (assume-type ulid <ulid>)
  (string-append (base32-encode (ulid-timestamp ulid)
                                ulid-timestamp-bit-size)
                 (base32-encode (ulid-random ulid)
                                ulid-random-bit-size)))

(define (string->ulid str)
  (assume-type str <string>)
  (unless (= (string-length str) 26)
    (ulid-parse-error 'string->ulid "length of ULID string is not 26" str))
  (let ((t (base32-decode (substring str 0 10)))
        (r (base32-decode (substring str 10 26))))
    (make-ulid t r)))

(define (ulid->bytevector ulid)
  (assume-type ulid <ulid>)
  (let ((v (bitwise-ior (arithmetic-shift (ulid-timestamp ulid)
                                          ulid-random-bit-size)
                        (ulid-random ulid))))
    (rlet1 res (make-u8vector ulid-byte-size)
      (put-uint! ulid-byte-size res 0 v 'big-endian))))

(define (bytevector->ulid vec)
  (assume-type vec <u8vector>)
  (unless (= (u8vector-length vec) ulid-byte-size)
    (ulid-parse-error 'bytevector->ulid
                      (format "byte-size of ULID binary is not ~A"
                              ulid-byte-size)
                      vec))
  (let ((v (get-uint ulid-byte-size vec 0 'big-endian)))
    (make-ulid (bit-field v
                          ulid-random-bit-size
                          (+ ulid-random-bit-size
                             ulid-timestamp-bit-size))
               (bit-field v
                          0
                          ulid-random-bit-size))))

(define (ulid:urandom-read-integer size _t)
  (assume-type size <integer>)
  (call-with-input-file "/dev/urandom"
    (lambda (iport)
      (and iport
           (let loop ((res 0)
                      (n size))
             (if (<= n 0)
                 res
                 (loop (bitwise-ior (arithmetic-shift res 8)
                                    (read-byte iport))
                       (- n 1))))))
    :if-does-not-exist #f))

(define (ulid:random-integer size _t)
  (assume-type size <integer>)
  (random-integer (expt 2 (* 8 size))))

(define-constant base32-alphabets
  "0123456789ABCDEFGHJKMNPQRSTVWXYZ")

(define-constant base32-rev-mapping-length 128)

(define-constant base32-rev-mapping-vector
  (rlet1 vec (make-vector base32-rev-mapping-length #f)
    (let ((i 0))
      (string-for-each
       (lambda (c)
         (vector-set! vec (char->integer c) i)
         (set! i (+ i 1)))
       base32-alphabets))))

(define (base32-decode-char c)
  (let ((i (char->integer c)))
    (when (>= i base32-rev-mapping-length)
      (ulid-parse-error "invalid ULID char" c))
    (let ((res (vector-ref base32-rev-mapping-vector i)))
      (unless res
        (ulid-parse-error "invalid ULID char" c))
      res)))

(define-constant base32-bit-size 5)

(define (base32-encode n bit-size)
  (assume-type n <integer>)
  (assume-type bit-size <integer>)
  (let ((bit-size (* base32-bit-size
                     (exact (ceiling (/ bit-size base32-bit-size))))))
    (call-with-port (open-output-string)
      (lambda (oport)
        (let loop ((pos bit-size))
          (when (> pos 0)
            (write-char (string-ref base32-alphabets
                                    (bit-field n
                                               (- pos base32-bit-size)
                                               pos))
                        oport)
            (loop (- pos base32-bit-size))))
        (get-output-string oport)))))

(define (base32-decode str)
  (assume-type str <string>)
  (call-with-port (open-input-string str)
    (lambda (iport)
      (let loop ((res 0))
        (let ((c (read-char iport)))
          (if (eof-object? c)
              res
              (loop (bitwise-ior
                     (arithmetic-shift res base32-bit-size)
                     (base32-decode-char c)))))))))
