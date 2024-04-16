(load "src/status.scm")
(load "src/utils.scm")
(load "src/sha3.scm")

(define (make-array base-address memory)
  (define address-bytes (->byte-vector base-address))

  (define val-address
    (compose (partial sha3 224 address-bytes)
	     integer->bytevector))

  (define (write bindings)
    (map (lambda (binding)
	   (let ((pos (car binding))
		 (val (cdr binding)))
	     (list 'write! (val-address pos) val)))
	 bindings))

  (define (read n)
    (map (compose (partial list 'read)
		  val-address)
	 (iter n)))

  (lambda (op . args)
    (let ((queries
	   (case op
	     (write! (apply write  args))
	     (read   (apply read args))
	     (else (error 'atomic-vector
			  "unknown operation"
			  op)))))
      (apply memory queries))))
