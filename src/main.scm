(load "src/utils.scm")
(load "src/vector.scm")
(load "src/memory.scm")

;;; Convert the given value into a ms duration.
(define (ms v) (make-time 'time-duration (* v  (expt 10 6)) 0))

(define (random-in-range start stop)
  (+ start (random (- stop start))))

(define (network-delay rtt-min rtt-max)
  (lambda args
    (sleep (ms (random-in-range rtt-min rtt-max)))
    (apply values args)))

(define (in-out-log obj)
  (lambda args
    (printf "Arguments:\n~a\n" args)
    (let ((res (apply obj args)))
      (printf "Result:\n~a\n\n" res)
      res)))

(define (observe trace f)
  (let ((lock     (make-mutex))
	(traced-f (trace f))
	(n-calls  0)
	(n-fails  0))
    (lambda args
      (case (car args)
	(n-calls n-calls)
	(n-fails n-fails)
	(else
	 (atomically lock (set! n-calls (1+ n-calls)))
	 (let ((res      (apply traced-f args)))
	   (if (status? res)
	       (unless (success? res)
		 (atomically lock
			     (set! n-fails (+ n-fails 1))))
	       (atomically lock
			   (set! n-fails (+ n-fails
					    (length (filter (compose not success?)
							    res))))))
	   res))))))

(define (print-report n-workers n-items observed-memories)
  (let ((summarize
	 (lambda (item)
	   (apply + (map (lambda (id-memory)
			   ((cdr id-memory) item))
			 observed-memories))))
	(particularize
	 (lambda (action)
	   (for-each (lambda (id-memory)
		       (let* ((id      (car id-memory))
			      (memory  (cdr id-memory))
			      (n-calls (memory 'n-calls))
			      (n-fails (memory 'n-fails)))
			 (action id n-fails n-calls)))
		     (sort (lambda (v1 v2)
			     (< (car v1) (car v2)))
			   observed-memories)))))
    (printf "\nReport:\n")
    (printf "number of workers:         ~a\n" n-workers)
    (printf "number of item per worker: ~a\n" n-items)
    (printf "number of memory calls:    ~a\n" (summarize 'n-calls))
    (printf "number of failed calls:    ~a\n" (summarize 'n-fails))
    (printf "number of failed/total memory calls per worker:\n")
    (particularize (partial printf "  - worker ~a: ~a/~a\n"))
    (printf "\n")))

(define (run-test test logger builder n-workers n-items n-repeat)
  (let* ((rtt-max           150)
	 (rtt-min           10)
	 (observed-memories (test (partial observe logger)
				  (network-delay rtt-min rtt-max)
				  builder
				  n-workers
				  n-items
				  n-repeat)))
    (print-report n-workers
		  n-items
		  observed-memories)))
