(in-package :stumpwm)

(defparameter *dwm-dbg* nil)
(defparameter *dwm-nf-dbg* nil)

(defclass dwm-group (tile-group)
  ((master-window :initarg :master-window :initform nil
                  :accessor dwm-group-master-window)
   (window-stack :initarg :window-stack :initform nil
                 :accessor dwm-group-window-stack)
   (superfluous-window :initform nil
                       :accessor moving-superfluous-window)))

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

(define-condition dwm-group-too-many-windows (error) ())

(defun dwm-split-frame-in-dir (group dir &optional (ratio 1/2))
  (let ((f (tile-group-current-frame group)))
    (if (> (length (group-frames group)) *maximum-dwm-group-windows*)
        (progn (message "Too many windows in group")
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

;;; rework frame splitting to take a frame instead of generating one

(defun dwm-split-frame (group frame how &optional (ratio 1/2))
  "Split the current frame into 2 frames. Return new frame number, if
it succeeded. NIL otherwise. RATIO is a fraction of the screen to
allocate to the new split window. If ratio is an integer then the
number of pixels will be used. This can be handy to setup the
desktop when starting."
  (check-type how (member :row :column))
  (let ((head (frame-head group frame)))
    ;; don't create frames smaller than the minimum size
    (when (or (and (eq how :row)
                   (>= (frame-height frame) (* *min-frame-height* 2)))
              (and (eq how :column)
                   (>= (frame-width frame) (* *min-frame-width* 2))))
      (multiple-value-bind (f1 f2) (funcall (if (eq how :column)
                                                'split-frame-h
                                                'split-frame-v)
                                            group frame ratio)
        (setf (tile-group-frame-head group head)
              (if (atom (tile-group-frame-head group head))
                  (list f1 f2)
                  (funcall-on-node (tile-group-frame-head group head)
                                   (lambda (tree)
                                     (substitute (list f1 f2) frame tree))
                                   (lambda (tree)
                                     (unless (atom tree)
                                       (find frame tree))))))
        (migrate-frame-windows group frame f1)
        (choose-new-frame-window f2 group)
        (if (eq (tile-group-current-frame group)
                frame)
            (setf (tile-group-current-frame group) f1))
        (setf (tile-group-last-frame group) f2)
        (sync-frame-windows group f1)
        (sync-frame-windows group f2)
        ;; we also need to show the new window in the other frame
        (when (frame-window f2)
          (unhide-window (frame-window f2)))
        (frame-number f2)))))

(defun dwm-split-frame-in-dir-with-frame (group frame dir &optional (ratio 1/2))
  (if (> (length (group-frames group)) *maximum-dwm-group-windows*)
      (progn (message "Too many windows in group")
             (error 'dwm-group-too-many-windows))
      (if (or (dwm-split-frame group frame dir ratio))
          (progn
            (when (frame-window frame)
              (update-decoration (frame-window frame)))
            (show-frame-indicator group))
          (error 'dwm-group-too-many-windows))))

(defun dwm-hsplit-frame (frame &optional (ratio "1/2"))
  (dwm-split-frame-in-dir-with-frame (current-group) frame :column (read-from-string ratio)))
(defun dwm-vsplit-frame (frame &optional (ratio "1/2"))
  (dwm-split-frame-in-dir-with-frame (current-group) frame :row (read-from-string ratio)))

(defun find-empty-frames (&optional (group (current-group)))
  (loop for frame in (group-frames group)
        when (null (frame-windows group frame))
          collect frame))

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

;;; DWM stack/master management ;;;

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

(defun swap-window-with-main (group window-or-number)
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
  (swap-window-with-main (current-group) number))

(defun prompt-for-swap-window (group)
  (let ((frame (choose-frame-by-number group)))
    (swap-window-with-main group (window-number (frame-window frame)))))

(defcommand dwm-switch () ()
  (prompt-for-swap-window (current-group)))

;; (define-key *root-map* (kbd "z") "dwm-switch")

(defun dwm-destroy-window-hook (window)
  (when (typep (window-group window) 'dwm-group)
    (dwm-destroy-window window)
    (repack-window-numbers)))

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
        (focus-frame group (frame-by-number group 0))
        (dwm-balance-stack-tree group)))))

(defcommand dwm-retile () ()
  (dwm-emergency-retile))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; group-add-window method ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun open-dwm-group-p (group)
  (< (length (group-frames group)) *maximum-dwm-group-windows*))

(defun find-open-dwm-group (&optional (group (current-group))
                              (screen (current-screen)))
  (let ((newgroup (loop for g in (screen-groups screen)
                        when (and (open-dwm-group-p g) (not (equal group g)))
                          return g)))
    (if newgroup
        newgroup
        (progn (gnewbg-dwm (read-one-line screen "New DWM Group Name: "))
               (find-open-dwm-group)))))

(let ((counter 0))
  (defun gen-group-name ()
    (format nil "Overflow group ~S" (incf counter))))

(defmethod group-add-window ((group dwm-group) window &key frame raise &allow-other-keys)
  (cond ((typep window 'float-window)
         (call-next-method)
         (message "Floating windows in dwm-groups is currently not supported"))
        ((eq frame :float)
         (change-class window 'float-window)
         (float-window-align window)
         (when raise (group-focus-window group window))
         (message "Floating windows in dwm-groups is currently not supported"))
        (t
         ;; this is undefined behavior i think, but its done in the other
         ;; group-add-window method so i think its ok, at least on sbcl. 
         (change-class window 'dwm-window)
         (setf (window-frame window) (frame-by-number group 0))
         (case (length (group-windows group))
           (1
            ;; (message "moving from 0 to 1 window")
            (setf (dwm-group-master-window group) window))
           (2
            ;; (message "moving from 1 to 2 windows")
            (let ((frame (frame-by-number group 0)))
              (dwm-hsplit-frame frame "2/3"))
            (let* ((prev-win (dwm-group-master-window group))
                   (prev-win-new-frame (car (remove (frame-by-number group 0)
                                                    (group-frames group)))))
              (push prev-win (dwm-group-window-stack group))
              (setf (window-frame prev-win) prev-win-new-frame
                    (frame-window prev-win-new-frame) prev-win
                    (window-frame window) (frame-by-number group 0)
                    (frame-window (frame-by-number group 0)) window
                    (dwm-group-master-window group) window)))
           (otherwise
            ;; (message "We already have the stack set up, so put windows there!")
            (let* ((master-frame (frame-by-number group 0))
                   (old-master (dwm-group-master-window group))
                   (frames-no-master (remove master-frame (group-frames group)))
                   (frame-to-split (car frames-no-master)))
              (handler-case
                  (progn
                    (dwm-vsplit-frame frame-to-split)
                    (push (dwm-group-master-window group)
                          (dwm-group-window-stack group))
                    (setf (dwm-group-master-window group) window)
                    (setf (window-frame window) (frame-by-number group 0)
                          (window-frame old-master)
                          (or (car (find-empty-frames group))
                              (error "No Empty Frames in group ~S! Something has gone terribly wrong!" group))
                          (frame-window (window-frame old-master)) old-master)
                    (focus-frame group (frame-by-number group 0))
                    (dwm-balance-stack-tree group))
                (dwm-group-too-many-windows ()
                  (setf (moving-superfluous-window group) window)
                  (let ((new-group (gnew (gen-group-name))))
                    (move-window-to-group window new-group)))))))
         (loop for frame in (group-frames group)
               do (sync-frame-windows group frame))
         (when (null (frame-window (window-frame window)))
           (frame-raise-window (window-group window) (window-frame window)
                               window nil)))))

(defun dwm-find-superfluous-windows (group)
  (let ((frames (group-frames group)))
    (flatten
     (loop for frame in frames
           when (> (length (frame-windows group frame)) 1)
             collect (remove (frame-window frame) (frame-windows group frame))))))

(defmethod group-delete-window ((group dwm-group) (window dwm-window))
  (cond ((equal window (moving-superfluous-window group))
         (setf (moving-superfluous-window group) nil))
        ((equal window (dwm-group-master-window group))
         ;; (message "group-windows: ~S" (group-windows group))
         (let* ((new-master (pop (dwm-group-window-stack group))))
           (if new-master
               (let* ((new-masters-old-frame (window-frame new-master))
                      (master-frame (frame-by-number group 0))
                      (head (current-head group))
                      (tree (tile-group-frame-head group head)))
                 (cond 
                   ((not (dwm-group-window-stack group)) ; we got 1 window left
                    (setf (window-frame new-master) master-frame
                          (frame-window master-frame) new-master
                          (dwm-group-master-window group) new-master)
                    (loop for remframe in (remove (frame-by-number group 0)
                                                  (group-frames group))
                          do (setf (tile-group-frame-head group head)
                                   (remove-frame tree remframe)))
                    (setf (tile-group-current-frame group) master-frame)
                    (focus-frame group master-frame)
                    (update-decoration (frame-window master-frame))
                    (loop for frame in (group-frames group)
                          do (sync-frame-windows group frame)))
                   (t ; we have a stack left and a new master window
                    (setf (tile-group-frame-head group head)
                          (remove-frame tree new-masters-old-frame)
                          (window-frame new-master) master-frame
                          (frame-window master-frame) new-master
                          (dwm-group-master-window group) new-master)
                    (dwm-balance-stack-tree group)
                    (loop for frame in (group-frames group)
                          do (sync-frame-windows group frame))
                    (focus-frame group master-frame))))
               (let ((f (window-frame window))) ; taken from tile-group/window
                 ;; maybe pick a new window for the old frame
                 (when (eq (frame-window f) window)
                   (frame-raise-window group f
                                       (first (frame-windows group f)) nil))))))
        ((member window (dwm-group-window-stack group))
         (let* ((new-stack (remove window (dwm-group-window-stack group)))
                (frame-to-remove (window-frame window))
                (head (frame-head group frame-to-remove))
                (tree (tile-group-frame-head group head)))
           (setf (dwm-group-window-stack group) new-stack)
           (cond (new-stack
                  (setf (tile-group-frame-head group head)
                        (remove-frame tree frame-to-remove))
                  (tree-iterate tree (lambda (leaf)
                                       (sync-frame-windows group leaf)))
                  (focus-frame group (frame-by-number group 0))
                  (dwm-balance-stack-tree group)
                  (loop for frame in (group-frames group)
                        do (sync-frame-windows group frame)))
                 (t
                  (setf (tile-group-frame-head group head)
                        (remove-frame tree frame-to-remove))
                  (focus-frame group (frame-by-number group 0))
                  (loop for frame in (group-frames group)
                        do (sync-frame-windows group frame))))))))
