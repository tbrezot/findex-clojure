(define-record-type status
  (fields header info))

(define (success? status)
  (eq? 'OK (status-header status)))

(define (make-success info)
  (make-status 'OK info))

(define (make-failure info)
  (make-status 'KO info))

(define (until-success op update init)
  (let loop ((stt (apply op init)))
    (if (success? stt)
	stt
	(loop (apply op (update init (status-info stt)))))))

(define (unwrap-success op)
  (let ((stt op))
    (if (success? stt)
	(status-info stt)
	(error 'unwrap-success "memory error" (cons op stt)))))
