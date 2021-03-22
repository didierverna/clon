(asdf:defsystem :advanced
  :depends-on (#+ecl :net.didierverna.clon.setup ;; Cf. User manual Chap. 7.1
	       :net.didierverna.clon)
  :components ((:file "advanced"))
  :entry-point "advanced:main")
