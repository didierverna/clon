(asdf:defsystem :advanced
  :depends-on (:net.didierverna.clon.setup :net.didierverna.clon)
  :components ((:file "advanced"))
  :entry-point "advanced:main")
