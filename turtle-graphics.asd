(in-package :asdf-user)
(defsystem "turtle-graphics"
  :description "Turtle graphics for lisp supporting rgba32."
  :version "0.1"
  :author "Johannes Martinez Calzada"
  :licence "LGPL"
  :depends-on ("tag" "surface-x11")
  :components ((:file "turtle")))
