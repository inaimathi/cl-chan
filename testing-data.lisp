(in-package :cl-chan)

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