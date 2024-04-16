(define-syntax atomically
  (syntax-rules ()
    ((_ lock expr)(begin
		    (mutex-acquire lock)
		    (let ((result expr))
		      (mutex-release lock)
		      result)))))

(define (identity . args) (apply values args))

(define iter
  (case-lambda
    ((stop) (iter 0 stop 1))
    ((start stop) (iter start stop 1))
    ((start stop step)
     (if (< start stop)
	 (cons start (iter (+ start step) stop step))
	 '()))))

(define compose
  (case-lambda
    ((f) f)
    ((f g) (lambda args
	     (let-values ((out (apply g args)))
	       (apply f out))))
    ((f g . f-list)
     (apply compose (cons (compose f g) f-list)))))

(define (partial f . early-bindings)
  (lambda others-bindings
    (apply f (append early-bindings others-bindings))))

(define (integer->bytevector n)
  (if (zero? n)
      (make-bytevector 1 0)
      (uint-list->bytevector (list n)
			     'big
			     (exact (ceiling (/ (integer-length n) 8))))))

(define (->byte-vector obj)
  (cond
   ((bytevector? obj) obj)
   ((integer? obj)    (integer->bytevector obj))
   (else (error '->byte-vector
		"object cannot be converted into bytes"
		obj))))

(define (random-address)
  (random (expt 2 128)))
