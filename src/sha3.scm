(load "src/utils.scm")

(define libcrypto (load-shared-object
		   "/home/theophile/.guix-profile/lib/libcrypto.so"))

(define evp-q-digest
  (foreign-procedure "EVP_Q_digest"
		     (void* string void* u8* size_t u8* void*)
		     boolean))
(define evp-shake-128
  (foreign-procedure "EVP_shake128" () void*))

(define evp-shake-256
  (foreign-procedure "EVP_shake256" () void*))

(define evp-sha3-224
  (foreign-procedure "EVP_sha3_224" () void*))

(define evp-sha3-256
  (foreign-procedure "EVP_sha3_256" () void*))

(define evp-sha3-384
  (foreign-procedure "EVP_sha3_384" () void*))

(define evp-sha3-512
  (foreign-procedure "EVP_sha3_512" () void*))

(define evp-md-ctx-new
  (foreign-procedure "EVP_MD_CTX_new" () void*))

(define evp-digest-init
  (foreign-procedure "EVP_DigestInit" (void* void*) boolean))

(define evp-digest-update
  (foreign-procedure "EVP_DigestUpdate" (void* u8* size_t) boolean))

(define evp-digest-final
  (foreign-procedure "EVP_DigestFinal" (void* u8* void*) boolean))

(define evp-digest-final-xof
  (foreign-procedure "EVP_DigestFinalXOF" (void* u8* size_t) boolean))

(define md-free!
  (foreign-procedure "EVP_MD_free" (void*) void))

(define ctx-free!
  (foreign-procedure "EVP_MD_CTX_free" (void*) void))

(define (finalize! ctx output-length)
  (let ((output (make-bytevector output-length)))
    (if (evp-digest-final ctx output 0)
	output
	(error 'finalize
	       "Error during EVP_DigestFinal"
	       (cons ctx output-length)))))

(define (finalize-xof! ctx output-length)
  (let ((output (make-bytevector output-length)))
    (if (evp-digest-final-xof ctx output (* 8 output-length))
	output
	(error 'finalize-xof
	       "Error during EVP_DigestFinalXOF"
	       output-length))))

(define (update! ctx bytes)
  (unless (evp-digest-update ctx bytes (bytevector-length bytes))
    (error 'sha3-multi "Error during EVP_DigestUpdate" bytes)))

(define (init! ctx md)
  (unless (evp-digest-init ctx md)
    (error 'sha3-multi "Error during EVP_DigestInit")))

;;; Digest the given byte-vector using the given algorithm in one FFI call.
(define (digest algo bytes)
  (let-values (((name output-length)
		(case algo
		  (SHA3-224 (values "SHA3-224" 28))
		  (SHA3-256 (values "SHA3-256" 32))
		  (SHA3-384 (values "SHA3-384" 48))
		  (SHA3-512 (values "SHA3-512" 64))
		  (else (error 'sha3-multi
			       "unsupported digest algorithm"
			       algo)))))
    (let ((output (make-bytevector output-length)))
      (if (evp-q-digest 0 name 0 bytes (bytevector-length bytes) output 0)
	  output
	  (error 'digest
		 "Error during EVP_Q_Digest"
		 (cons name bytes))))))

;;; Incrementally computes a Keccak hashing of the given bytes using the given
;;; MD specification, to produce output-length bytes.
(define (keccak md finalizer output-length bytes)
  (let ((ctx (evp-md-ctx-new)))
    (init! ctx md)
    (let loop ((bytes bytes))
      (if (null? bytes)
	  (let ((output (finalizer ctx output-length)))
	    (md-free! md)
	    (ctx-free! ctx)
	    output)
	  (begin
	    (update! ctx (car bytes))
	    (loop (cdr bytes)))))))

(define lock (make-mutex))

;;; Incrementally compute a SHA3 hashing of the given bytes with the given
;;; security.
(define  (sha3 security . bytes)
  (atomically
   lock
   (let-values (((md output-length)
		 (case security
		   (224 (values (evp-sha3-224) 28))
		   (256 (values (evp-sha3-256) 32))
		   (384 (values (evp-sha3-384) 48))
		   (512 (values (evp-sha3-512) 64))
		   (else (error 'sha3-multi "unsupported SHA3 security" security)))))
     (keccak md finalize! output-length bytes))))

;;; Incrementally compute a SHAKE hashing of the given bytes with the given
;;; securty to produce output-length bytes.
(define  (shake security output-length . bytes)
  (atomically
   lock
   (let ((md (case security
	       (128 (evp-shake-128))
	       (256 (evp-shake-256))
	       (else (error 'shake "unsupported SHAKE security" security)))))
     (keccak md finalize-xof! output-length bytes))
   ;; bytes
   ))


(define (test-multiple-run n-iter f)
  (for-each (lambda (_) (f)) (iter n-iter)))
