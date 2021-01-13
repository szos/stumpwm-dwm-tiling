(in-package :stumpwm)

(defclass dwm-group (tile-group)
  ((master-window :initarg :master-window :initform nil
		  :accessor dwm-group-master-window)
   (window-stack :initarg :window-stack :initform nil
		 :accessor dwm-group-window-stack)))

(defclass dwm-window (tile-window) ())

(defparameter *maximum-dwm-group-windows* 7
  "the maximum number of windows that can be placed in a dwm-group before we 
attempt to place them in the next dwm-group (or generate a new one)")

(defcommand gnew-dwm (name) ((:rest "Group Name: "))
  (unless name 
    (throw 'error :abort))
  (add-group (current-screen) name :type 'dwm-group))

(defcommand gnewbg-dwm (name) ((:rest "Group Name: "))
  (unless name
    (throw 'error :abort))
  (add-group (current-screen) name :type 'dwm-group :background t))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Redefinied Internals ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun kill-group (group to-group)
  (if (and (typep to-group 'dwm-group)
	   (>= (+ (length (group-windows to-group))
		  (length (group-windows group)))
	       *maximum-dwm-group-windows*))
      (message
       "unable to kill nonempty group - the group to send windows to is full. ")
      (unless (eq group to-group)
	   (let ((screen (group-screen group)))
	     (merge-groups group to-group)
	     (setf (screen-groups screen) (remove group (screen-groups screen)))
	     (netwm-update-groups screen)
	     (netwm-set-group-properties screen)))))

;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Utility Functions ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;

(defun generate-new-dwm-group ()
  (let ((name
	  (read-one-line (current-screen)
			 "To many windows in group. Enter a new group name: ")))
    (gnewbg-dwm name)))

(defun find-suitable-dwm-group (current)
  (let ((possible-groups
	  (non-hidden-groups (screen-groups (group-screen current)))))
    (loop for group in possible-groups
	  when (and (not (equal current group))
		    (typep group 'dwm-group)
		    (< (length (group-windows group))
		        *maximum-dwm-group-windows*))
	    do (return-from find-suitable-dwm-group group))
    (generate-new-dwm-group)))

(define-condition dwm-group-too-many-windows (error) ())

(defun dwm-split-frame-in-dir (group dir &optional (ratio 1/2))
  (let ((f (tile-group-current-frame group)))
    (if (> (length (group-frames group)) *maximum-dwm-group-windows*)
	(progn (message "Too many windows in group, Adding window to next group")
	       (error 'dwm-group-too-many-windows))
	(if (or (split-frame group dir ratio))
	    (progn
	      (when (frame-window f)
		(update-decoration (frame-window f)))
	      (show-frame-indicator group))
	    (error 'dwm-group-too-many-windows)))))

(defun dwm-hsplit (&optional (ratio "1/2"))
  (dwm-split-frame-in-dir (current-group) :column (read-from-string ratio)))
(defun dwm-vsplit (&optional (ratio "1/2"))
  (dwm-split-frame-in-dir (current-group) :row (read-from-string ratio)))

(defun dwm-balance-stack-tree (&optional (group (current-group)))
  (let ((tree (tile-group-frame-head group (current-head))))
    (when (listp tree)
      (balance-frames-internal group (cadr tree)))))

(defcommand dwm-balance () ()
  (dwm-balance-stack-tree))

(defun dwm-remove-all-splits (&optional (group (current-group)))
  (let* ((frame (tile-group-current-frame group))
	 (head (frame-head group frame))
	 (tree (tile-group-frame-head group head)))
    (if (atom tree)
	(message "Done removing")
	(progn (remove-split)
	       (dwm-remove-all-splits)))))

;;;;;;;;;;;;;;;;;;
;;; Add Window ;;;
;;;;;;;;;;;;;;;;;;

;; (defun dwm-group-add-window (window)
;;   "this function should be hung on *new-window-hook*, and implements dwm-style 
;; tiling with a master window and a window stack. "
;;   (let* ((group (window-group window))
;; 	 (frames (group-frames group)))
;;     (case (length frames)
;;       (1
;;        (when (> (length (group-windows group)) 1)
;; 	 (let ((prev-win (dwm-group-master-window group)))
;; 	   (push prev-win (dwm-group-window-stack group))
;; 	   ;; the 0th frame is always master
;; 	   (dwm-hsplit "2/3")
;; 	   (pull-window prev-win (car (remove (frame-by-number group 0)
;; 					      (group-frames group))))
;; 	   (pull-window window (frame-by-number group 0))))
;;        (setf (dwm-group-master-window group) window))
;;       (otherwise
;;        (let* ((master-frame (or (window-frame (dwm-group-master-window group))
;; 				(frame-by-number group 0)))
;; 	      (frames-no-master (remove master-frame frames)))
;; 	 (push (dwm-group-master-window group) (dwm-group-window-stack group))
;; 	 (pull-window (dwm-group-master-window group) (car frames-no-master))
;; 	 (handler-case (progn (dwm-vsplit)
;; 			      (dwm-balance-stack-tree group)
;; 			      (pull-window window (frame-by-number group 0))
;; 			      (focus-frame group (frame-by-number group 0))
;; 			      (setf (dwm-group-master-window group) window))
;; 	   (dwm-group-too-many-windows ()
;; 	     ;; (message "handler-case for dwm-group-too-many-windows")
;; 	     ;; undo the reorganization we did above. 
;; 	     (pull-window (dwm-group-master-window group)
;; 			  (frame-by-number group 0))
;; 	     (pop (dwm-group-window-stack group))
;; 	     ;; move the window to a new group
;; 	     (let ((new-group (find-suitable-dwm-group group)))
;; 	       (pull-to-group window new-group)
;; 	       ;; focus the window
;; 	       (focus-all window)
;; 	       ;; recall this function on the window to properly handle the handoff
;; 	       (dwm-group-add-window window)))))))))

;;;;;;;;;;;;;;;;;;;;;;
;;; Destroy Window ;;;
;;;;;;;;;;;;;;;;;;;;;;

(defun dwm-destroy-stack-window (group frame window)
  (focus-frame group frame)
  (remove-split)
  (focus-frame group (frame-by-number group 0))
  (dwm-balance-stack-tree group)
  (setf (dwm-group-window-stack group)
	(remove window (dwm-group-window-stack group))))

(defun dwm-pop-from-stack (group)
  (let ((new-master (when (dwm-group-window-stack group)
		      (pop (dwm-group-window-stack group)))))
    (if (not new-master)
	(progn (dwm-remove-all-splits group)
	       (message "No more windows"))
	(let ((new-master-windows-old-frame (window-frame new-master)))
	  (setf (dwm-group-master-window group) new-master)
	  (focus-frame group new-master-windows-old-frame)
	  (remove-split)
	  (pull-window new-master (frame-by-number group 0))
	  (dwm-balance-stack-tree group)))))

(defun dwm-destroy-window (window)
  (let ((frame (window-frame window))
	(group (window-group window)))
    (if (= (frame-number frame) 0) ; then its master frame
	(dwm-pop-from-stack group)
	(dwm-destroy-stack-window group frame window))))

(defun swap-window-with-main-by-number (group window-or-number)
  (let* ((stack (dwm-group-window-stack group))
	 (win (if (numberp window-or-number)
		  (member window-or-number stack :key 'window-number)
		  window-or-number))
	 (old-master (dwm-group-master-window group)))
    (when win
      (exchange-windows (dwm-group-master-window group) (car win))
      (setf (dwm-group-master-window group) (car win))
      (setf (dwm-group-window-stack group)
	    (remove (car win) (dwm-group-window-stack group)))
      (push old-master (dwm-group-window-stack group))
      (focus-all (car win)))))

(defcommand dwm-switch-to-window (number) ((:number "Window Number: "))
  (swap-window-with-main-by-number (current-group) number))

(defun dwm-destroy-window-hook (window)
  (when (typep (window-group window) 'dwm-group)
    (dwm-destroy-window window)
    (repack-window-numbers)))

;; (add-hook *destroy-window-hook* 'dwm-destroy-window-hook)

;; (defun dwm-new-window-hook (window)
;;   (when (typep (window-group window) 'dwm-group)
;;     (dwm-group-add-window window)))

;; (add-hook *new-window-hook* 'dwm-new-window-hook)

;; (add-hook *destroy-window-hook* 'dwm-destroy-window-hook)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Retile in Case of Emergency ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun dwm-emergency-retile ()
  (let ((group (current-group)))
    (when (typep group 'dwm-group)
      (let* ((master-window (dwm-group-master-window group))
	     (other-windows (remove master-window (group-windows group))))
	;; (dwm-remove-all-splits group)
	(only)
	(dwm-hsplit "2/3")
        (focus-frame group (frame-by-number group 0))
	(pull-window master-window (frame-by-number group 0))
	(fnext)
	(loop for n from 2 to (length other-windows)
	      do (dwm-vsplit))
	(focus-frame group (frame-by-number group 0))))))

(defcommand dwm-retile () ()
  (dwm-emergency-retile))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; group-add-window method ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod group-add-window ((group dwm-group) window &key frame raise &allow-other-keys)
  (cond ((typep window 'float-window)
	 (call-next-method)
	 ;; (message "Floating windows in dwm-groups is currently not supported")
	 )
	((eq frame :float)
	 (change-class window 'float-window)
	 (float-window-align window)
	 (when raise (group-focus-window group window))
	 ;; (message "Floating windows in dwm-groups is currently not supported")
	 )
	(t
	 ;; this is undefined behavior i think, but its done in the other
	 ;; group-add-window method so i think its ok, at least on sbcl. 
	 (change-class window 'dwm-window)
	 (setf (window-frame window) (frame-by-number group 0))
	 ;; (push (dwm-group-master-window group) (dwm-group-window-stack group))
	 ;; (setf (dwm-group-master-window group) window)
	 ;; (setf (frame-window (frame-by-number group 0)) window)
	 ;; we cant rely on hanging a window on a hook... hmmm. 
	 (case (length (group-frames group))
	   (1
	    (when (> (length (group-windows group)) 1)
	      (message "two windows!")
	      (focus-frame group (car (group-frames group)))
	      (dwm-hsplit "2/3")
	      (let* ((prev-win (dwm-group-master-window group))
		     (prev-win-new-frame (car (remove (frame-by-number group 0)
						      (group-frames group)))))
		(push prev-win (dwm-group-window-stack group))
		;; because we just hsplit, we know that there must be 2+ groups
		(setf (window-frame prev-win) prev-win-new-frame
		      ;; (frame-window prev-win-new-frame) prev-win
		      (window-frame window) (frame-by-number group 0)
		      ;; (frame-window (frame-by-number group 0)) window
		      )))
	    (setf (dwm-group-master-window group) window))
	   (otherwise
	    ;; (let* ((master-window (dwm-group-master-window group))
	    ;; 	   (other-windows (dwm-group-window-stack group)))
	    ;;   (only)
	    ;;   (dwm-hsplit "2/3")
	    ;;   (focus-frame group (frame-by-number group 0))
	    ;;   (pull-window window (frame-by-number group 0))
	    ;;   (fnext)
	    ;;   (loop for n from 1 to (length other-windows)
	    ;; 	    do (dwm-vsplit))
	    ;;   (focus-frame group (frame-by-number group 0))
	    ;;   (push master-window (dwm-group-window-stack group))
	    ;;   (setf (dwm-group-master-window group) window))
	    (let* ((master-frame
		     (or (window-frame (dwm-group-master-window group))
			 (frame-by-number group 0)))
		   (frames-no-master (remove master-frame (group-frames group))))
	      (push (dwm-group-master-window group)
		    (dwm-group-window-stack group))
	      (setf (window-frame (dwm-group-master-window group))
		    (car frames-no-master))
	      (focus-frame group (car frames-no-master))
	      (handler-case
		  (progn
		    (dwm-vsplit)
		    (dwm-balance-stack-tree group)
		    (setf (window-frame window) (frame-by-number group 0))
		    (focus-frame group (frame-by-number group 0))
		    (setf (dwm-group-master-window group) window))
		(dwm-group-too-many-windows ()
		  (setf (window-frame (dwm-group-master-window group))
			(frame-by-number group 0))
		  (pop (dwm-group-window-stack group))
		  (message "To many windows! Group state is out of whack, please move the most recently added window to another group!")
		  (let ((new-group (find-suitable-dwm-group group)))
		    (pull-to-group window new-group)
                    (focus-all window)
		    (message "Window ~a sent to group ~a" window new-group)))))
	    ))
	 (loop for frame in (group-frames group)
	       do (sync-frame-windows group frame))
	 (when (null (frame-window (window-frame window)))
	   (frame-raise-window (window-group window) (window-frame window)
			       window nil))
	 ;; (run-with-timer 1 nil 'dwm-emergency-retile)
	 )))

(defmethod group-delete-window ((group dwm-group) (window dwm-window))
  (cond ((member window (dwm-group-window-stack group))
	 (let* ((stackwin (member window (dwm-group-window-stack group)))
		(new-stack (remove window (dwm-group-window-stack group)))
		(new-master (car stackwin))
		(new-master-frame (window-frame new-master))
		(head (frame-head group new-master-frame))
		(tree (tile-group-frame-head group head)))
	   (setf (tile-group-frame-head group head)
		 (remove-frame tree new-master-frame)
		 (dwm-group-window-stack group) new-stack)
           (tree-iterate tree
			 (lambda (leaf)
			   (sync-frame-windows group leaf)))
	   (focus-frame group (frame-by-number group 0))
	   (dwm-balance-stack-tree group)))
	((= (length (group-windows group)) 0)
	 (let ((f (window-frame window)))
	   ;; maybe pick a new window for the old frame
	   (when (eq (frame-window f) window)
	     (frame-raise-window group f (first (frame-windows group f))
				 nil))))
	((= (length (group-windows group)) 1)
	 (let ((only (pop (dwm-group-window-stack group))))
	   (pull-window only (frame-by-number group 0))
	   (focus-frame group (frame-by-number group 0))
	   (only)))
	(t
	 (let* ((new-master (pop (dwm-group-window-stack group)))
		(nm-frame (window-frame new-master)))
	   (pull-window new-master (frame-by-number group 0))
	   (focus-frame group nm-frame)
	   (remove-split)
	   (focus-frame group (frame-by-number group 0))
	   (setf (dwm-group-master-window group) new-master)
	   (dwm-balance-stack-tree group)))))

