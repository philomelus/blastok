;;;; -*- lisp -*-

(asdf:defsystem "blastok"
  :version "0.0.1"
  :author "Russ Gibson"
  :license "Public Domain"
  :depends-on ("cl-liballegro")
  :components ((:module "src"
                :components
                ((:file "package")
                 (:file "main"))))
  :description "Game similar to classic Asteroids.")

