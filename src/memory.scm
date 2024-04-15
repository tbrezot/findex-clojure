(load "src/utils.scm")
(load "src/status.scm")

;;; Implements a memory abstraction as an atomic key-value store with the
;;; following operations:
;;; - (read   addr)
;;; - (write! addr new-word)
;;; - (free!  addr old-word)
;;; - (c&s!   addr old-word new-word)
(define (make-store)
  (define lock  (make-mutex))
  (define store (make-hashtable equal-hash equal?))

  (define (apply-op-when get-cur op ok?)
    (let ((cur-word (get-cur)))
      (if (ok? cur-word)
	  (begin
	    (op)
	    (make-success cur-word))
	  (make-failure cur-word))))

  (define (dump) store)

  (define (read addr)
    (make-success (hashtable-ref store addr '())))

  (define (write! addr word)
    (apply-op-when (delay (hashtable-ref store addr '()))
		   (delay (hashtable-set! store addr word))
		   null?))

  (define (c&s! addr old-word new-word)
    (apply-op-when (delay (hashtable-ref store addr '()))
		   (delay (hashtable-set! store addr new-word))
		   (partial equal? old-word)))

  (define (free! addr old-word)
    (apply-op-when (delay (hashtable-ref store addr '()))
		   (delay (hashtable-delete! store addr))
		   (partial equal?)))

  (lambda (op . args)
    (let ((op (case op
		(dump   dump)
		(read   read)
		(c&s!   c&s!)
		(free!  free!)
		(write! write!)
		(else (error 'store
			     "unknown operation"
			     op)))))
      (atomically lock (apply op args)))))
