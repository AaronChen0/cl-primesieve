(in-package :cl-primesieve)

(define-foreign-library libprimesieve
  (:darwin (:or "libprimesieve.9.dylib" "libprimesieve.dylib"))
  (:unix (:or "libprimesieve.so.9" "libprimesieve.so"))
  (t (:default "libprimesieve")))

(defun load-lib ()
  (handler-case
      (use-foreign-library libprimesieve)
    (error (e)
      (declare (ignore e))
      (warn "libprimesieve not loaded."))))

(load-lib)

(defcenum int-type
  :short-primes :ushort-primes :int-primes :uint-primes
  :long-primes :ulong-primes
  :longlong-primes :ulonglong-primes
  :int16-primes :uint16-primes :int32-primes :uint32-primes
  :int64-primes :uint64-primes)

;; Get an array with the primes inside the interval [start, stop].
(defun generate-primes (start stop)
  (declare (type (unsigned-byte 64) start stop))
  (let* ((size (foreign-alloc :int))
         (array (foreign-funcall "primesieve_generate_primes" :uint64 start
                                 :uint64 stop :pointer size int-type :uint64-primes :pointer))
         (n  (mem-ref size :int))
         (ret (make-array n :element-type '(unsigned-byte 64))))
    (loop for i below n
       do (setf (aref ret i) (mem-aref array :uint64 i)))
    (foreign-free size)
    (foreign-funcall "primesieve_free" :pointer array :void)
    ret))

;; Get an array with the first n primes >= start.
(defun generate-n-primes (n &optional (start 0))
  (declare (type (unsigned-byte 64) n start))
  (let ((array (foreign-funcall "primesieve_generate_n_primes" :uint64 n
                                :uint64 start int-type :uint64-primes :pointer))
        (ret (make-array n :element-type '(unsigned-byte 64))))
    (loop for i below n
       do (setf (aref ret i) (mem-aref array :uint64 i)))
    (foreign-funcall "primesieve_free" :pointer array :void)
    ret))

;; Find the nth prime.
;; @param n  if n = 0 finds the 1st prime >= start,
;;           if n > 0 finds the nth prime > start,
;;           if n < 0 finds the nth prime < start (backwards).
(defcfun ("primesieve_nth_prime" nth-prime) :uint64
  (n :uint64)
  (start :uint64))

(defmacro defcount (name)
  (let ((funcall-string
         (concatenate 'string "primesieve_"
                      (substitute #\_ #\-
                                  (string-downcase (string name))))))
    `(defun ,name (start stop)
       (declare (type (unsigned-byte 64) start stop))
       (foreign-funcall ,funcall-string :uint64 start
                        :uint64 stop :uint64))))

;; Count the primes/twins/triplets/quadruplets/quintuplets/sextuplets
;; within the interval [start, stop].
(defcount count-primes)
(defcount count-twins)
(defcount count-triplets)
(defcount count-quadruplets)
(defcount count-quintuplets)
(defcount count-sextuplets)

;; When used in slime, fflush has to be applied,
;; in order to see number print out.
(defcvar ("stdout" stdout) :pointer)

(defmacro defprint (name)
  (let ((funcall-string
         (concatenate 'string "primesieve_"
                      (substitute #\_ #\-
                                  (string-downcase (string name))))))
    `(defun ,name (start stop)
       (declare (type (unsigned-byte 64) start stop))
       (foreign-funcall ,funcall-string :uint64 start
                        :uint64 stop :void)
       (foreign-funcall "fflush" :pointer stdout :int)
       nil)))

;; Print the primes/twins/triplets/quadruplets/quintuplets/sextuplets
;; within the interval [start, stop]
;; to the standard output.
(defprint print-primes)
(defprint print-twins)
(defprint print-triplets)
(defprint print-quadruplets)
(defprint print-quintuplets)
(defprint print-sextuplets)

(defctype size :ulong)

(defcstruct iterator
  (i size)
  (last-idx size)
  (start :uint64)
  (stop :uint64)
  (stop-hint :uint64)
  (dist :uint64)
  (primes :pointer)
  (vector :pointer)
  (prime-generator :pointer)
  (is-error :int))

(defun make-iterator ()
  (foreign-alloc 'iterator))

(defcfun ("primesieve_init" init) :void
  (it :pointer))

(defcfun ("primesieve_free_iterator" free-iterator) :void
  (it :pointer))

(defcfun ("primesieve_skipto" skipto) :void
  (it :pointer)
  (start :uint64)
  (stop-hint :uint64))

(defcfun ("primesieve_generate_next_primes" generate-next-primes) :void
  (it :pointer))

(defcfun ("primesieve_generate_prev_primes" generate-prev-primes) :void
  (it :pointer))

(defun next-prime (it)
  (with-foreign-slots ((i last-idx start stop stop-hint dist primes)
                       it (:struct iterator))
    (declare (ignore start stop stop-hint dist))
    (when (= (prog1 i (incf i)) last-idx)
      (generate-next-primes it))
    (mem-aref primes :uint64 i)))

(defun prev-prime (it)
  (with-foreign-slots ((i last-idx start stop stop-hint dist primes)
                       it (:struct iterator))
    (declare (ignore last-idx start stop stop-hint dist))
    (when (zerop (prog1 i (decf i)))
      (generate-prev-primes it))
    (mem-aref primes :uint64 i)))

;; Returns the largest valid stop number for primesieve.
;; @return 2^64-1 (UINT64_MAX).
(defcfun ("primesieve_get_max_stop" get-max-stop) :uint64)

;; Get the current set sieve size in KiB
(defcfun ("primesieve_get_sieve_size" get-sieve-size) :int)

;; Get the current set number of threads
(defcfun ("primesieve_get_num_threads" get-num-threads) :int)

;; Set the sieve size in KiB (kibibyte).
;; The best sieving performance is achieved with a sieve size
;; of your CPU's L1 or L2 cache size (per core).
;; @pre sieve_size >= 8 && <= 4096.
(defcfun ("primesieve_set_sieve_size" set-sieve-size) :void
  (sieve-size :int))

;; Set the number of threads for use in
;; primesieve_count_*() and primesieve_nth_prime().
;; By default all CPU cores are used.
(defcfun ("primesieve_set_num_threads" set-num-threads) :void
  (num-threads :int))

(defcfun ("primesieve_version" version) :string)
