(in-package :cl-chan)
(file-enable-sql-reader-syntax)

(defmacro page-template ((&key title) &body body)
  `(with-html-output-to-string (*standard-output* nil :prologue t :indent t)
     (:html :xmlns "http://www.w3.org/1999/xhtml" :xml\:lang "en" :lang "en"
	    (:head (:meta :http-equiv "Content-Type" :content "text/html;charset=utf-8")
		   (:title (str ,title))
		   (:link :rel "stylesheet" :type "text/css" :href "/cl-chan.css"))
	    (:body ,@body))))

(define-formlet (post-comment-form)
    ((thread-id hidden) 
     (author text) (email text) (subject text) (body textarea)
     ;; (captcha recaptcha)
     )
  (let ((new-comment (make-instance 'comment
				    :thread-id (parse-integer thread-id)
				    :author author :email email 
				    :subject subject :body body
				    :date-time (now))))
    (update-records-from-instance new-comment)
    (redirect (format nil "/thread?thread-id=~a" thread-id))))

(define-formlet (post-thread-form)
    ((author text) (email text) (subject text)
     (body textarea :validation ((longer-than? 5) "You need to type at least six characters here."))
     ;; (captcha recaptcha)
     )
  (let* ((thread-id (update-records-from-instance
		     (make-instance 'thread :board-id 1)))
	 (new-comment (make-instance 'comment 
				     :thread-id thread-id
				     :author author :email email 
				     :subject subject :body body
				     :date-time (now))))
    (update-records-from-instance new-comment)
    (redirect"/board")))

(define-easy-handler (board-page :uri "/board") ()
  (page-template (:title "cl-chan")
    (let ((board (caar (select 'board :where [= [slot-value 'board 'id] 1]))))
      (echo board))))

(define-easy-handler (thread-page :uri "/thread") (thread-id)
  (let ((thread (caar (select 'thread :where [= [slot-value 'thread 'id] thread-id]))))
    (page-template (:title (or (subject (car (comments thread))) (id thread)))
      (echo thread))))