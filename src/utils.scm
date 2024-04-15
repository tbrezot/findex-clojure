(define-syntax atomically
  (syntax-rules ()
    ((_ lock expr)(begin
		    (mutex-acquire lock)
		    (let ((result expr))
		      (mutex-release lock)
		      result)))))

(define iter
  (case-lambda
    ((stop) (iter 0 stop 1))
    ((start stop) (iter start stop 1))
    ((start stop step)
     (if (< start stop)
	 (cons start (iter (+ start step) stop step))
	 '()))))

(define (compose f g)
  (lambda args (f (apply g args))))

(define (partial f . early-bindings)
  (lambda others-bindings
    (apply f (append early-bindings others-bindings))))
