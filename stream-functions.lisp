;;;; streams: 

;;; mzero: the null stream
(defmacro mzero ()
  `nil)

(defun null-stream? (a-inf)
  (not a-inf))

;;; unit: defines a single-element stream
(defmacro unit (a)
  `(stream-cons ,a (mzero)))

;;; case-inf: evalueate one of three expressions based on whether the
;;;   given stream has zero one or many (possibly infinite)
;;;   elements. For the empty stream, on-zero is evaluated. For a
;;;   stream with one element, that element is bound to a^ in the
;;;   evaluateion of on-one. For a multi-element stream, the fist
;;;   element of the stream is bound to a and the thunk generating the
;;;   rest of the stream is bound to f in the evaluation of on-choice.
(defmacro case-stream (e on-zero ((a^) on-one) ((a f) on-choice))
  (let ((a-inf (gensym)))
    `(let ((,a-inf ,e))
       (cond
	 ;; evaluate on-zero for the empty stream
	 ((not ,a-inf) ,on-zero)
	 ;; if a-inf has no rest then it is a one-element stream
	 ((not (stream-rest ,a-inf))
	  (let ((,a^ (stream-first ,a-inf)))
	    ,on-one))
	 ;; otherwise a-inf is a multi-element stream represented as 
	 ;; a first element and a thunk that returns the rest of the
	 ;; stream
	 (t (let ((,a (stream-first ,a-inf))
		  (,f (stream-rest ,a-inf)))
	      ,on-choice))))))

;;; map-inf: map the function p over the first n elements of a
;;;   stream. If the stream has length m and m < n then p is mapped over
;;;   the first m elements of the stream.
(defun map-inf (n p a-inf)
  (case-stream a-inf
	       ;; return null list for the empty stream
	       nil
	       ;; for a stream containing only the element a, return a
	       ;; list containing (p a)
	       ((a)
		(cons (funcall p a) nil))
	       ;; for a multi-element stream list the values of p
	       ;; applied to the first n elements of the stream
	       ((a f)
		(cons (funcall p a)
		      (cond
			((not n) (map-inf n p (funcall f)))
			((> n 1) (map-inf (- n 1) p (funcall f))))))))

;;; map-stream: map the function p over a stream of inputs returning a
;;;   stream of outputs
(defun map-stream (p a-inf)
  (case-stream a-inf
	       (mzero)
	       ((a)
		(unit (funcall p a)))
	       ((a f)
		(stream-cons (funcall p a)
			     (lambda ()
			       (map-stream p (funcall f)))))))

;;; mplus: concatenate two streams, a-inf is a stream and f is a thunk
;;;   returning a stream
(defun mplus (a-inf f)
  (case-stream a-inf
	       ;; first stream is empty, return second stream
	       (funcall f) ; to get the second stream, evaluate f
	       ;; first stream has one elemrnt, concatenated stream
	       ;; will haave the first streams element as its first
	       ;; element and have the second stream as the rest
	       ((a) (stream-cons a f))
	       ;; first stream is a multi-element stream
	       ((a f0) (stream-cons a ; first element is first element of
				      ; first stream
				      (lambda ()
					(mplus (funcall f0) f)))))) ; rest is rest of
			 		; first stream
					; concatenated to
					; second stream

;;; concatenate-stream: concatenate the two streams a-inf and b-inf
(defun concatenate-stream (a-inf b-inf)
  (if (null-stream? b-inf)
      a-inf
      (mplus a-inf (lambda () b-inf))))

;;; bind: mapcan for streams, maps the function g that takes a stream
;;;   element and returns a stream over each element of the stream
;;;   concatenating the results
(defun bind (a-inf g)
  (case-stream a-inf
	       ;; empty stream: return the null stream
	       (mzero)
	       ;; singleton stream: apply g to stream
	       ((a) (funcall g a))
	       ;; multi-element stream: concatenate the result of
	       ;; calling g on the fiirst element of stream to binding
	       ;; g on the rest of the stream
	       ((a f) (mplus (funcall g a)
			     (lambda ()
			       (bind (funcall f) g))))))

;;; mplus-i: interleave two streams, this is mostly the same as mplus
(defun mplus-i (a-inf f)
  (case-stream a-inf
	       (funcall f)
	       ((a) (stream-cons a f))
	       ;; unlike in mplus, the recursive case switches the role
	       ;; of f and f0 so that elements of the first and secnond
	       ;; streams are alternately choice'd onto the new stream
	       ((a f0) (stream-cons a
				    (lambda ()
				      (mplus-i (funcall f) f0))))))

(defun interleave-stream (a-inf b-inf)
  (if (null-stream? b-inf)
      a-inf
      (mplus-i a-inf (lambda () b-inf))))

;;; bind-i: maps the function g that takes a stream element and
;;;   returns a stream over each element of the stream intreleaving
;;;   the results.  This is just like bind, except the intermediate
;;;   results are interleaved instead of concatenated.
(defun bind-i (a-inf g)
  (case-stream a-inf
	       (mzero)
	       ((a) (funcall g a))
	       ;; bind and bind-i differ only in that bind uses mplus
	       ;; here where bind-i uses mplus-i
	       ((a f) (mplus-i (funcall g a)
			       (lambda ()
				 (bind-i (funcall f) g))))))

;;; stream->list: convert a finite stream to a list by mapping the
;;;   identity function over the stream.
(defun stream->list (a-inf)
  (map-inf nil (lambda (x) x) a-inf))

;;; list->stream: convert a list to a finite stream
(defun list->stream (lst)
  (if (null lst)
      (mzero)
      (stream-cons (car lst)
		   (lambda ()
		     (list->stream (cdr lst))))))

;;; string->stream: convert a string to a finite stream of characters
(defun string->stream (string)
  (let ((final-char-idx (1- (length string))))
    (if (<= 0 final-char-idx)
	(labels ((make-stream (char-idx)
		   (if (= char-idx final-char-idx)
		       (unit (char string char-idx))
		       (stream-cons (schar string char-idx)
				    (lambda ()
				      (make-stream (1+ char-idx)))))))
	  (funcall #'make-stream 0))
	(mzero))))


;;; input->stream
(defun input->stream (input-stream)
  (let ((stream-first
	 (read-char input-stream))
	(stream-rest (lambda ()
		       (let ((c (read-char input-stream '())))
			 (if c c (mzero))))))
    (stream-cons stream-first
		 stream-rest)))

;;; with-stream-from-file
(defmacro with-stream-from-file (stream filespec &body body)
  (let ((input-stream (gensym "input-stream")))
    `(with-open-file (,input-stream ,filespec)
       (let ((,stream (input->stream ,input-stream)))
	 ,@body))))
