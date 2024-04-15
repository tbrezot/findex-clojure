;; Chez Scheme implementation of the Findex algorithm using the queue metaphor.

;;; Partially evaluates the given function with to given arguments.
(define (partial f . args)
  (lambda other-args
    (apply f (append args other-args))))

;;; Returns the composition of the given functions.
(define (compose . f-list)
  (if (null? f-list)
      (lambda args (apply values args))
      (lambda args
	(call-with-values
	    (lambda ()
	      (apply (apply compose
			    (cdr f-list))
		     args))
	  (car f-list)))))


;;; Returns the a procedure that apply its arguments to all given functions, then
;;; returns each output.
(define (parallel . f-list)
  (lambda args
    (apply values (map (lambda (f) (apply f args))
		       f-list))))

;;; Returns a functions that spread its arguments across the given functions.
(define (spread-1 . f-list)
  (lambda args
    (apply values (map (lambda (f arg) (f arg))
		       f-list args))))

;;; Returns the memoization of the given function.
(define (memoize f)
  (let ((cache '()))
    (lambda args
      (let ((known-value (assoc args cache)))
	(if known-value
	    (cdr known-value)
	    (let ((val (apply f args)))
		      (set! cache (cons (cons args val)
					cache))
		      val))))))

;;; Returns the list of values obtained by iterating from start to stop by step.
(define iter
  (case-lambda
    [(stop)       (iter 0 stop 1)]
    [(start stop) (iter start stop 1)]
    [(start stop step)
     (if (<= stop start)
	 '()
	 (letrec ((loop (lambda (n)
			  (if (<= stop n)
			      '()
			      (cons n (loop (+ n step)))))))
	   (loop start)))]))

(define (println . args)
  (apply format (cons #t args))
  (display "\n"))

(define (identity . args)
  (values args))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                  Dummy cryptographic primitives                           ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (sha3-128 . args)
  (cons 'SHA3 args))

(define (kmac key . args)
  (list 'KMAC key args))

(define (aead-encrypt key val aad)
  (cons 'CTX (list (cons 'KEY key)
		   (cons 'AAD aad)
		   (cons 'VAL val))))

(define (aead-decrypt key ctx aad)
  (assert (eq? (car ctx) 'CTX))
  (let* ((alist (cdr ctx)))
    (assert (equal? (cdr (assoc 'KEY alist)) key))
    (assert (equal? (cdr (assoc 'AAD alist)) aad))
    (cdr (assoc 'VAL alist))))

(define (make-db)
  (define lock (make-mutex))
  (define store '())

  (define (read uid)
    (let ((cur (assoc uid store)))
      (if cur (cdr cur) '())))

  (define (write uid val)
    (let ((cur (assoc uid store)))
      (if cur
	  (set-cdr! cur val)
	  (set! store (cons (cons uid val) store)))))

  (define (comp&swap uid old new)
    (let* ((binding (assoc uid store)))
      (if binding
	  (let ((cur (cdr binding)))
	    (if (equal? cur old)
		(set-cdr! binding new))
	    cur)
	  (begin
	    (if (null? old)
		(set! store (cons (cons uid new) store)))
	    '()))))

  (lambda (op . args)
    (mutex-acquire lock)
    (let ((res (cond ((eq? op 'dump) store)
		     ((eq? op 'read) (read (car args)))
		     ((eq? op 'write) (write (car args) (cadr args)))
		     ((eq? op 'c&s) (comp&swap (car args) (cadr args) (caddr args))))))
      (mutex-release lock)
      res)))


(define (decompose op values)
  (list 'CHAIN (cons 'OP op) (cons 'VALUES values)))

(define (recompose chain)
  chain)

;;; Implements a queue using a counter holding the current length and a sequence
;;; of registers to hold its values. Uses the following procedures to access to
;;; the stored variables:
;;; - (incr tag n): increments the value bound to tag by n.
;;; - (bind tag val): bind the given value to tag.
;;; - (read tag): read the value bound to tag.
(define (make-naive-atomic-queue incr bind read)
  ;; Reads the values from the queue bound to the given tag.
  (define (read-queue tag)
    (let ((size (read (cons tag 'counter))))
      (if (null? size)
	  '()
	  (map (compose read (partial cons tag))
	       (iter size)))))

  ;; Pushes the given values to the queue bound to the given tag.
  (define (push-queue tag values)
    (let* ((delta (length values))
	   (size  (incr (cons tag 'counter) delta)))
      (for-each bind
		(map (partial cons tag)
		     (iter size (+ size delta)))
		values)))

  (values read-queue push-queue))

(define (findex key db)
  (define counter-cache '())

  (define kw-hash sha3-128)

  ;; Transforms the given tag into a cryptographically secure token using the
  ;; Findex key.
  (define tokenize (memoize (partial kmac key 'TOKENIZE)))

  ;; Encrypts the given value using the Findex key.
  (define encrypt  (partial aead-encrypt (kmac key 'AEAD)))

  ;; Decrypts the given value using the Findex key.
  (define decrypt  (partial aead-decrypt (kmac key 'AEAD)))

  ;; Retrieves the value bound to the given tag.
  (define (secure-read tag)
    (let* ((tok (tokenize tag))
	   (ctx (db 'read tok)))
      (if (null? ctx)
	  '()
	  (decrypt ctx tok))))

  ;; Binds the given value to the given tag.
  (define (secure-bind tag val)
    (let ((tok (tokenize tag)))
      (db 'write tok (encrypt val tok))))

  ;; Increments the value bound to the given tag by n.
  (define (secure-incr tag n)
    (let ((tok       (tokenize tag))
	  (binding   (assoc tag counter-cache)))
      (letrec ((incr (lambda (old old-ctx)
		       (let* ((new-ctx (encrypt (+ old n) tok))
			      (cur-ctx (db 'c&s tok old-ctx new-ctx))
			      (cur (if (null? cur-ctx) 0 (decrypt cur-ctx tok))))
			 (apply (if (equal? old cur) identity incr)
				(list cur cur-ctx))))))
	(let ((cur-state (apply incr (if binding (cdr binding) (list 0 '())))))
	  (set! counter-cache (cons (cons tag cur-state) counter-cache))
	  (car cur-state)))))

  (let-values (((read-queue push-queue)
		(make-naive-atomic-queue secure-incr secure-bind secure-read)))
    (let ((search (compose recompose read-queue kw-hash))
	  (insert (compose push-queue (spread-1 kw-hash (partial decompose 'add))))
	  (delete (compose push-queue (spread-1 kw-hash (partial decompose 'del)))))
      (lambda (op . args)
	(cond ((eq? op 'search) (search (car args)))
	      ((eq? op 'insert) (insert (car args) (cadr args)))
	      ((eq? op 'delete) (delete (car args) (cadr args)))
	      ((eq? op 'show-cache) (println "~a" counter-cache)))))))

(define multi-findex
  (lambda (op . args)
    (cond ((eq? op 'search) (par-map (partial findex 'search)
				 args)))))
