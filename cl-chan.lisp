(defpackage :cl-chan (:use :cl :cl-who :hunchentoot :formlets :clsql)
  (:shadowing-import-from :clsql :select))
(in-package :cl-chan)

(connect '("localhost" "cl_chan" "me" "my password") :database-type :mysql)

(setf *default-caching* nil)

(setf formlets:*public-key* "my-public-key" 
      formlets:*private-key* "my-private-key")

(defvar *web-server* (start (make-instance 'acceptor :port 4242)))
(file-enable-sql-reader-syntax)

(defun now () (clsql-sys:utime->time (get-universal-time)))

(def-view-class board ()
  ((id :accessor id :initarg :id :type integer :db-constraints (:not-null :auto-increment) :db-kind :key)
   (name :reader name :initarg :name :type (string 5))
   (threads :accessor threads :db-kind :join
	    :db-info (:join-class thread :home-key id :foreign-key board-id :set t))))

(def-view-class thread ()
  ((id :accessor id :initarg :id :type integer :db-constraints (:not-null :auto-increment) :db-kind :key)
   (board-id :reader board-id :initarg :board-id :type integer)
   (comments :accessor comments :db-kind :join
	     :db-info (:join-class comment :home-key id :foreign-key thread-id :set t))))

(def-view-class comment ()
  ((id :accessor id :initarg :id :type integer :db-constraints (:not-null :auto-increment) :db-kind :key)
   (thread-id :reader thread-id :initarg :thread-id :type integer)
   (author :reader author :initarg :author :initform nil :type string)
   (email :reader email :initarg :email :initform nil :type string)
   (subject :reader subject :initarg :subject :initform nil :type string)
   (body :reader body :initarg :body :initform nil :type string)
   (date-time :reader date-time :initarg :date-time :type wall-time)))

(defmethod echo ((board board))
  (with-html-output (*standard-output* nil :indent t)
    (:h1 (str (name board))) (:hr)
    (show-formlet post-thread-form) (:hr)
    (dolist (thread (threads board))
      (summarize thread))))

(defmethod summarize ((thread thread) &optional (preview-comment-count 5))
  (let* ((preview-comments (last (cdr (comments thread)) preview-comment-count))
	 (omitted-count (- (length (cdr (comments thread))) (length preview-comments)))
	 (first-comment (car (comments thread))))
    (with-html-output (*standard-output* nil :indent t)
      (:div :class "thread"
	    (echo-header first-comment)
	    (:a :href (format nil "/thread?thread-id=~a" (id thread)) "Reply")
	    (:span :class "body" (:p (str (body first-comment))))
	    (when (> omitted-count 0)
	      (htm (:p :class "omitted" 
		       (str (format nil "~a comments omitted (and we don't do pictures yet)" 
				    omitted-count)))))
	    (dolist (r preview-comments) (str (echo r)))))))

(defmethod echo ((thread thread))
  (let ((first-comment (car (comments thread))))
    (with-html-output (*standard-output* nil :indent t)
      (:a :href "/board" "[Back]") (:hr)
      (show-formlet post-comment-form :default-values (list (id thread))) (:hr)
      (:div :class "thread"
	    (echo-header first-comment)
	    (:span :class "body" 
		   (:p (str (body first-comment))))
	    (dolist (r (cdr (comments thread))) (str (echo r)))))))

(defmethod echo ((comment comment))
  (with-html-output-to-string (*standard-output* nil :indent t)
    (:div :class "comment"
	  (echo-header comment)
	  (:span :class "body" 
		 (:p (str (body comment)))))))

(defmethod echo-header ((comment comment))
  (with-html-output (*standard-output*)
    (:span :class "header" 
	   (dolist (elem '(author email date-time subject))
	     (htm (:span :class (format nil "~(~a~)" elem) (str (slot-value comment elem))))))))

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

(push (create-static-file-dispatcher-and-handler "/cl-chan.css" (merge-pathnames "cl-chan.css")) *dispatch-table*)

;;;;;;;;;;;;;;; test data and functions
(defun create-test-database ()
  (create-tables)
  (insert-test-data))

(defun drop-tables ()
  (dolist (i '(board thread comment))
    (drop-view-from-class i)))

(defun create-tables ()
  (dolist (i '(board thread comment))
    (create-view-from-class i)))

(defun insert-test-data ()
  (loop for i in (list (make-instance 'board :name "a")
		       (make-instance 'thread :board-id 1)
		       (make-instance 'thread :board-id 1)

		       (make-instance 'comment 
			    :thread-id 1
			    :author "me" :email "my@email.com" :subject "FRIST"
			    :body "I am most certainly the first poster in this fine establishment"
			    :date-time (now))
		       (make-instance 'comment 
			    :thread-id 1
			    :author "someone else" :email "you@fmail.com" :subject "Stop being a douchebag"
			    :date-time (now))
		       (make-instance 'comment 
			    :thread-id 1
			    :subject "You must be new here"
			    :body "trolled-softly.jpg"
			    :date-time (now))
		       (make-instance 'comment 
			    :thread-id 2
			    :body "[Something vaguely anti-semetic.]"
			    :date-time (now)))
	do (update-records-from-instance i)))