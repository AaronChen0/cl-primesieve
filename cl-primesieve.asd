(asdf:defsystem #:cl-primesieve
  :description "Common Lisp bindings for libprimesieve"
  :author "Aaron Chen"
  :license "MIT"
  :depends-on ("cffi")
  :serial t
  :components ((:file "package")
               (:file "primesieve")))
