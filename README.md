# cl-primesieve

Common Lisp bindings for the [primesieve](https://github.com/kimwalisch/primesieve) C++ library.

Generates primes orders of magnitude faster than any pure Lisp code!

## Features

* Get an array of primes
* Iterate over primes using little memory
* Find the nth prime
* Count/print primes and [prime k-tuplets](https://en.wikipedia.org/wiki/Prime_k-tuple)
* Multi-threaded for counting primes and finding the nth prime

## Usage examples

``` common-lisp
(defpackage :cl-ps-test (:use :cl :cl-primesieve))
(in-package :cl-ps-test)

;; count/print primes
(count-primes 1 100)
(print-primes 1 100)
(count-twins 1 100)
(print-twins 1 100)

;; Get an array with the primes inside the interval [start, stop].
(generate-primes 1 (expt 10 9)) ; run under 1 sec

;; Get an array with the first n primes >= start(optional).
(generate-n-primes 1000)
(generate-n-primes 1000 100) ; get 1000 primes starting from 100

;; Get the 1000th prime
(nth-prime 1000)

;; Instead of generating a large array of primes and then do something
;; with the primes it is also possible to simply iterate over the primes
;; which uses less memory.
(defvar iter (make-iterator))
(init iter)        ; init it before using it
(next-prime iter)
(skipto iter 100)  ; reset the iterator to start from 100
(next-prime iter)
(prev-prime iter)

;; Set/Get number of threads used.
;; By default all CPU cores are used.
(get-num-threads)
(set-num-threads 2)
```
Package cl-primesieve has nickname cl-ps.

## Installation

For Debian/Ubuntu linux:
``` shell
sudo apt install primesieve
cd ~/quicklisp/local-projects/
git clone https://github.com/AaronChen0/cl-primesieve
```

In a lisp repl:
``` common-lisp
(ql:quickload "cl-primesieve")
```
