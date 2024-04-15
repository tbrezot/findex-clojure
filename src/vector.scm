(load "src/memory.scm")
(load "src/status.scm")
(load "src/utils.scm")

(define (make-atomic-mutable-vector base-address memory)
  (define cnt 0)

  (define cnt-address
    (cons base-address 'header))

  (define val-address
    (partial list base-address 'array))

  ;; Increment the counter and cache the previous value.
  (define (incr-cnt!)
    (until-success (partial memory 'c&s! cnt-address)
		   (lambda (init cur-val)
		     (set! cnt cur-val)
		     (list cur-val (1+ cur-val)))
		   (list (if (zero? cnt) '() cnt)
			 (1+ cnt))))

  ;; Store the given value at the given position.
  (define (store-value! pos val)
    (unwrap-success (memory 'write! (val-address pos) val)))

  ;; Push a new value to the end of the vector.
  (define (push! value)
    (incr-cnt!)
    (store-value! cnt value)
    (set! cnt (1+ cnt)))

  ;; Read the value stored at the given address.
  (define fetch
    (compose unwrap-success (partial memory 'read)))

  ;; Read all values stored in the vector in two round-trips:
  ;; - first fetch the current counter value and update the cache;
  ;; - read all addresses derived from this counter.
  (define (read)
    (let ((new-cnt (fetch cnt-address)))
      (set! cnt new-cnt)
      (map (compose fetch val-address)
	   (iter new-cnt))))

  ;; Dispatch operations.
  (lambda (op . args)
    (case op
      (push! (apply push! args))
      (read  (apply read args))
      (else (error 'atomic-vector
		   "unknown operation"
		   op)))))

(define (test-sequential)
  (define n-repeat 10)
  (define store  (make-store))
  (define data (map (lambda (i) (random i))
		    (iter 1 n-repeat)))
  (define vector (make-atomic-mutable-vector 'vec store))
  (for-each (lambda (v) (vector 'push! v))
	    data)
  (assert (equal? (vector 'read) data)))

(define (test-parallel)
  (define n-threads 10)
  (define n-repeat  10)
  (define store (make-store))

  (define (worker id vector)
    (for-each (lambda (repetition)
		(let ((val (+ id (* repetition n-threads))))
		  (vector 'push! val)))
	      (iter n-repeat)))

  (let* ((handles (map (lambda (id)
			 (let ((vector (make-atomic-mutable-vector 'vec store)))
			   (fork-thread (lambda () (worker id vector)))))
		       (iter n-threads)))
	 (res (map (lambda (handle) (thread-join handle))
		   handles)))
    (let* ((vector (make-atomic-mutable-vector 'vec store))
	   (data   (vector 'read)))
      (assert (equal? (sort < data)
		      (iter (* n-threads n-repeat)))))))
