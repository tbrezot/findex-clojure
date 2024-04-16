(define-record-type status
  (fields header info))

(define (success? status)
  (eq? 'OK (status-header status)))

(define (make-success info)
  (make-status 'OK info))

(define (make-failure info)
  (make-status 'KO info))

(define (until-success op update init)
  (let loop ((curr init))
    (let* ((stt (op curr)))
      (if (success? stt)
	  curr
	  (loop (update curr (status-info stt)))))))

(define (unwrap-success stt)
  (if (success? stt)
      (status-info stt)
      (error 'unwrap-success "memory error" stt)))
