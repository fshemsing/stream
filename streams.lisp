;;; lstream: a 'lazy' stream
;;;   stream-first is the first element of the stream
;;;   stream-rest is a thunk that, when called, returns the rest of the stream
(defstruct (lstream (:constructor stream-cons (stream-first stream-rest))
		    (:conc-name nil)
		    (:predicate stream?)
		    (:print-function (lambda (x st d)
				       (declare (ignore d))
				       (if
					(null (stream-rest x))
					(format st "S<~A>"
						(stream-first x))
					(format st "S<~A...>"
						(stream-first x))))))
  stream-first
  stream-rest)

