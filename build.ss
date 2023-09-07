#!/usr/bin/env gxi

(import :std/make)

(def lib-build-spec
  '("sncf/api"
    "sncf/display"
    "sncf/mattermost"
    ))

(def bin-build-spec
  '((exe: "sncf")))

(def server-build-spec
  '((exe: "server")))

(def srcdir
  (path-normalize (path-directory (this-source-file))))

(def (main . args)
  (match args
    (["lib"]
     (make srcdir: srcdir
	   optimize: #t
           debug: 'src
           lib-build-spec))
    (["bin"]
     (make srcdir: srcdir
           optimize: #t
           debug: #f
	   build-deps: "build-deps-bin"
           bin-build-spec))
    (["server"]
     (make srcdir: srcdir
	   bindir: srcdir
           optimize: #t
           debug: #f
	   build-deps: "build-deps-server"
           server-build-spec))
    ([]
     (main "lib")
     (main "bin"))))
