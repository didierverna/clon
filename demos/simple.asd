(asdf:defsystem :simple
  :depends-on (#+ecl :net.didierverna.clon.setup ;; Cf. User manual Chap. 7.1
	       :net.didierverna.clon)
  :components ((:file "simple"))
  :entry-point "simple:main")
