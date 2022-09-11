#!/usr/bin/env gxi

(import :std/make)

;; the build specification
(def build-spec
  '((exe: "sncf")))

;; the source directory anchor
(def srcdir
  (path-normalize (path-directory (this-source-file))))

;; the main function of the script
(def (main . args)
  (match args
    ;; this is the default (and, here, only) action, which builds the project
    ([]
     (make srcdir: srcdir          ; source anchor
           ;;bindir: srcdir          ; where to place executables; default is GERBIL_PATH/bin
           optimize: #t            ; enable optimizations
           debug: #f               ; enable debugger introspection
           static: #t              ; don't generate static compilation artifacts
           build-spec))))          ; the actual build specification
