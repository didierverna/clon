(asdf:defsystem :simple
  :depends-on (:net.didierverna.clon.setup :net.didierverna.clon)
  :components ((:file "simple"))
  :entry-point "simple:main")
