(asdf:defsystem lishp
  :name "lishp"
  :version "1"
  :maintainer "codr7"
  :author "codr7"
  :description " a Common Lispy, text based shell"
  :licence "MIT"
  :build-operation "asdf:program-op"
  :build-pathname "lishp"
  :entry-point "lishp:main"
  :depends-on ("local-time")
  :serial t
  :components ((:file "lishp")))
