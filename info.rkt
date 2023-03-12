; vim: filetype=racket
#lang info

(define collection
  "gg")
(define pkg-desc
  "A tool to discover, catalogue and compare git repos across N machines.")
(define version
  "0.0")
(define pkg-authors
  '("Siraaj Khandkar <siraaj@khandkar.net>"))
(define license
  'MIT)
(define deps
  '("base"))
(define build-deps
  '("scribble-lib" "racket-doc" "rackunit-lib"))
(define scribblings
  '(("doc/gg.scrbl" ())))
(define racket-launcher-names
  '("gg"))
(define racket-launcher-libraries
  '("main.rkt"))
