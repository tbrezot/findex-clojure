;; Define constants
(begin
  ;; Token length
  (define token-len 32)

  ;; Symmetric encryption overhead
  (define mac-len 16)
  (define nonce-len 12)
  (define enc-overhead (+ mac-len nonce-len))

  ;; Chain Table specific values
  (define block-len 16)
  (define line-len 5)
  (define flag-len (ceiling-quotient line-len 8))


  ;; Entry Table specific values.
  (define last-uid 32)
  (define hash-len 32)
  (define seed-len 16)

  ;; Chain Table line length
  (define ct-line-len
    (+ token-len enc-overhead flag-len
       (* (+ block-len 1) line-len)))

  (define et-line-len
    (+ token-len enc-overhead last-uid hash-len seed-len)))


(define (format-stats et-len ct-len)
  "Format the given ET/CT lengths."
  (list (list "Entry Table:" et-len)
	(list "Chain Table:" ct-len)))

(define (display-stats stats)
  "Display the given list of lists as space separated values"
  (map (lambda (line)
	 (map (lambda (v)
		(map display (list " " v)))
	      line)
	 (newline))
       stats))

(define (fact n)
  (cond ((< n 2) 1)
	(else (* n (fact (- n  1))))))

(define (zipf n)
  (cond ((< n 1) '())
	(else (cons (/ 1 (fact n)) (zipf (- n 1))))))

;(define (draw distr n)
  ;())

(define (normalize distr)
  (let ((sum (apply + distr)))
    (map (lambda (v) (/ v sum)) distr)))

(define (compute-table-lengths v-len n-kw n-doc distr)
  (let ((n-ct-line
	  (apply + (map (lambda (f)
			  (ceiling-quotient
			    (* f n-doc (ceiling-quotient v-len block-len))
			    line-len))
			(distr n-kw)))))
    (list (* et-line-len n-kw)
	  (* ct-line-len n-ct-line))))

;(display-stats (format-stats et-line-len ct-line-len))

;(newline)

;(display-stats (apply format-stats (compute-table-lengths 32 1000 1000000
							  ;(lambda (n)
							    ;(normalize (zipf n))))))

(use-modules (statprof))
(statprof-reset 0 50000 #t)
(statprof-start)
(normalize (zipf 2000))
(statprof-stop)
(statprof-display)
