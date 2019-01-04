;;;
;;; Test ulid
;;;
(use gauche.test)
(use gauche.uvector)

(test-start "ulid")
(use ulid)
(test-module 'ulid)

(define test-count 100)

(test* "(make-ulid 1 0)"
       "00000000010000000000000000"
       (ulid->string (make-ulid 1 0)))

(let ((s "01AN4Z07BY79KA1307SR9X4MV3"))
  (test* "ulid->string o string->ulid is identity"
         s
         ($ ulid->string
            $ string->ulid s))
  (test* "ulid->bytevector o bytevector->ulid is identity"
         s
         ($ ulid->string
            $ bytevector->ulid
            $ ulid->bytevector
            $ string->ulid s)))

(test* "make-ulid should raise &ulid-range-error for negative timestamp"
       (test-error &ulid-range-error)
       (make-ulid -1))

(test* "make-ulid should raise &ulid-range-error for timestamp that does not fit in 48bits"
       (test-error &ulid-range-error)
       (make-ulid (expt 2 48)))

(test* "make-ulid should raise &ulid-range-error for negative randomness"
       (test-error &ulid-range-error)
       (make-ulid #f -1))


(test* "make-ulid should raise &ulid-range-error for randomness that does not fit in 80"
       (test-error &ulid-range-error)
       (make-ulid #f (expt 2 80)))

(test* "string->ulid should raise &ulid-parse-error for an empty string"
       (test-error &ulid-parse-error)
       (string->ulid ""))

(test* "string->ulid should raise &ulid-parse-error for a non-base32-encoded string"
       (test-error &ulid-parse-error)
       (string->ulid "some randome string"))

(test* "string->ulid should raise &ulid-range-error if timestamp is out of range"
       (test-error &ulid-range-error)
       (string->ulid "ZZZZZZZZZZZZZZZZZZZZZZZZZZ"))

(test* "bytevector->ulid should raise &ulid-parse-error for an empty bytevector"
       (test-error &ulid-parse-error)
       (bytevector->ulid #u8()))

(test* "bytevector->ulid should raise &ulid-parse-error for a non-128bits bytevector"
       (test-error &ulid-parse-error)
       (bytevector->ulid (make-u8vector 12 1)))

(dotimes (i test-count)
  (let* ((u (make-ulid))
         (s (ulid->string u)))
    (test* "ulid->string o string->ulid is identity"
           s
           (ulid->string (string->ulid s))))
  (sys-nanosleep (* 1000 1000)))

(dotimes (i test-count)
  (let* ((u (make-ulid))
         (v (ulid->bytevector u)))
    (test* "ulid->bytevector o bytevector->ulid is identity"
           v
           (ulid->bytevector (bytevector->ulid v))))
  (sys-nanosleep (* 1000 1000)))

;; If you don't want `gosh' to exit with nonzero status even if
;; the test fails, pass #f to :exit-on-failure.
(test-end :exit-on-failure #t)
