(defpackage blastok
  (:use :cl)
  (:import-from #:cffi
                #:foreign-slot-value
                #:null-pointer-p
                #:null-pointer
                #:foreign-pointer
				#:foreign-alloc
                #:foreign-free
                #:foreign-enum-value)
  (:export #:main))
