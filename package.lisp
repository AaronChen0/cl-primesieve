(defpackage :cl-primesieve
  (:nicknames #:cl-ps)
  (:use #:cl #:cffi)
  (:export
   #:generate-primes
   #:generate-n-primes
   #:nth-prime
   #:count-primes
   #:count-twins
   #:count-triplets
   #:count-quadruplets
   #:count-quintuplets
   #:count-sextuplets
   #:print-primes
   #:print-twins
   #:print-triplets
   #:print-quadruplets
   #:print-quintuplets
   #:print-sextuplets
   #:make-iterator
   #:init
   #:free-iterator
   #:skipto
   #:next-prime
   #:prev-prime
   #:get-num-threads
   #:get-sieve-size
   #:get-max-stop
   #:set-num-threads
   #:set-sieve-size
   #:version))
