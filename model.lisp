(in-package :cl-chan)
(file-enable-sql-reader-syntax)

(defun now () (clsql-sys:utime->time (get-universal-time)))

;;;;;;;;;; board
(def-view-class board ()
  ((id :accessor id :initarg :id :type integer :db-constraints (:not-null :auto-increment) :db-kind :key)
   (name :reader name :initarg :name :type (string 5))
   (threads :accessor threads :db-kind :join
	    :db-info (:join-class thread :home-key id :foreign-key board-id :set t))))

(defmethod echo ((board board))
  (with-html-output (*standard-output* nil :indent t)
    (:h1 (str (name board))) (:hr)
    (show-formlet post-thread-form) (:hr)
    (dolist (thread (threads board))
      (summarize thread))))

;;;;;;;;;; thread
(def-view-class thread ()
  ((id :accessor id :initarg :id :type integer :db-constraints (:not-null :auto-increment) :db-kind :key)
   (board-id :reader board-id :initarg :board-id :type integer)
   (comments :accessor comments :db-kind :join
	     :db-info (:join-class comment :home-key id :foreign-key thread-id :set t))))

(defmethod summarize ((thread thread) &optional (preview-comment-count 5))
  (let* ((preview-comments (last (cdr (comments thread)) preview-comment-count))
	 (omitted-count (- (length (cdr (comments thread))) (length preview-comments)))
	 (first-comment (car (comments thread))))
    (with-html-output (*standard-output* nil :indent t)
      (:div :class "thread"
	    (echo-header first-comment)
	    (:a :href (format nil "/thread?thread-id=~a" (id thread)) "Reply")
	    (echo-image first-comment)
	    (:span :class "body" (:p (str (body first-comment))))
	    (:br :class "clear")
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
		   (echo-image first-comment)
		   (:p (str (body first-comment))))
	    (:br :class "clear")
	    (dolist (r (cdr (comments thread))) (str (echo r)))))))

;;;;;;;;;; comment
(def-view-class comment ()
  ((id :accessor id :initarg :id :type integer :db-constraints (:not-null :auto-increment) :db-kind :key)
   (thread-id :reader thread-id :initarg :thread-id :type integer)
   (author :reader author :initarg :author :initform nil :type string)
   (email :reader email :initarg :email :initform nil :type string)
   (subject :reader subject :initarg :subject :initform nil :type string)
   (body :reader body :initarg :body :initform nil :type string)
   (date-time :reader date-time :initarg :date-time :type wall-time)
   (image :reader image :initarg :image :type string)))

(defmethod echo ((comment comment))
  (with-html-output-to-string (*standard-output* nil :indent t)
    (:div :class "comment"
	  (echo-header comment)
	  (:span :class "body" 
		 (echo-image comment)
		 (:p (str (body comment)))
		 (:br :class "clear")))))

(defmethod echo-image ((comment comment))
  (when (image comment) 
    (with-html-output (*standard-output* nil :indent t)
      (:a :href (merge-pathnames (image comment) "/img/big/") 
	  (:img :class "pic" :src (merge-pathnames (image comment) "/img/preview/"))))))

(defmethod echo-header ((comment comment))
  (with-html-output (*standard-output*)
    (:span :class "header" 
	   (dolist (elem '(author email date-time subject))
	     (htm (:span :class (format nil "~(~a~)" elem) (str (slot-value comment elem))))))))