# cl-primesieve

Common Lisp bindings for the [primesieve](https://github.com/kimwalisch/primesieve) C++ library.

Generates primes orders of magnitude faster than any pure Lisp code!

## Features

* Get an array of primes
* Iterate over primes using little memory
* Find the nth prime
* Count/print primes and [prime k-tuplets](https://en.wikipedia.org/wiki/Prime_k-tuple)
* Multi-threaded for counting primes and finding the nth prime

## Installation

For Debian/Ubuntu linux:
``` shell
sudo apt install primesieve
cd ~/quicklisp/local-projects/
git clone https://github.com/AaronChen0/cl-primesieve
```

``` common-lisp
(ql:quickload "cl-primesieve")
```
