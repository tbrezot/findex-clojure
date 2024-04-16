(load "src/memory.scm")
(load "src/status.scm")
(load "src/utils.scm")
(load "src/array.scm")

(define-record-type header
  (fields (mutable counter)
	  (immutable array-ptr)))

;;; Creates a new vector with the following internal structure:
;;;
;;; +-----------+  +-------------
;;; | CNT | PTR |->| V1 | V2 ...
;;; +-----------+  +-------------
;;;
;;; Upon incrementation of the counter, a new header is created and stored using
;;; the given write-header! function. It should return a status type containing
;;; the current value of the header in case of failure.
;;;
;;; The given memory is used to store the backing array and the value of the
;;; header pointer should never change.
;;;
;;; The following operations are exposed:
;;; - (vector 'push! value) that stores value at the end of the array;
;;; - (vector 'read)        that reads the current value of the array.
(define (make-base-vector address memory read-header write-header!)
  (define header (make-header 0 address))
  (define array (make-array (header-array-ptr header) memory))

  (define (increment-counter! n)
    (let ((update-header
	   (lambda (_ current-header)
	     (set! header current-header)
	     (make-header (+ n (header-counter header))
			  (header-array-ptr header)))))
      (until-success write-header!
		     update-header
		     (update-header '()  header))))

  (define (write-value! values)
    (let ((n (length values)))
      (until-success
       (lambda (pos) (array 'write! (map cons
					 (iter pos (+ pos n))
					 values)))
       (lambda (pos _) (+ pos (length values)))
       (header-counter header))))

  (define (base-vector-push! values)
    (let* ((n          (length values))
	   (new-header (increment-counter! n))
	   (pos-start  (header-counter header))
	   (pos-stop   (+ pos-start n)))
      (map unwrap-success
	   (array 'write!
	      (map cons
		   (iter pos-start pos-stop)
		   values)))
      (set! header new-header)))

  (define (base-vector-read)
    (set! header (read-header))
    (map unwrap-success
	 (array 'read (header-counter header))))

  (lambda (op . args)
    (case op
      (read  (apply base-vector-read  args))
      (push! (apply base-vector-push! args))
      (else (error 'immutable-vector
		   "unknown operation"
		   op)))))

;;; Creates a new vector that uses the given memory. The header is written at
;;; the given address and mutated upon incrementation while the backing array is
;;; written to a random address.
(define (make-mutable-vector address memory)
  (define address-bytes (->byte-vector address))
  (define write-header!
    (let ((old-header '()))
      (lambda (new-header)
	(let ((stt (memory 'c&s! address-bytes old-header new-header)))
	  (if (success? stt)
	      (set! old-header new-header)
	      (set! old-header (status-info stt)))
	  stt))))
  (define (read-header)
    (unwrap-success (memory 'read address-bytes)))
  (make-base-vector address-bytes memory read-header write-header!))

;;; Assert values can correctly be written/read to/from the vector.
(define (test-vector-parallel observe delay builder n-threads n-items n-repeat)
  (define address         (random-address))
  (define memory          (make-batched-store))
  (define build           (lambda (mem) (builder address mem)))

  (define (worker id memory)
    (let ((vector (build memory)))
      (for-each (lambda (item)
		  (let ((val (+ id (* item n-threads))))
		    (vector 'push! (repeat n-repeat (lambda () val)))))
		(iter n-items))))

  (let* ((observed-memories '()))
    (time
     (map thread-join
	  (map (lambda (id)
		 (let ((memory (observe memory)))
		   (set! observed-memories
			 (cons (cons id memory) observed-memories))
		   (fork-thread (lambda ()
				  (worker id (compose memory delay))))))
	       (iter n-threads))))

    (let ((read-data (sort < ((build memory) 'read)))
	  (expected-data
	   (fold-left (partial merge <)
		      '()
		      (repeat n-repeat (lambda ()
					 (iter (* n-threads n-items)))))))
      (assert (equal? read-data expected-data))
      observed-memories)))
