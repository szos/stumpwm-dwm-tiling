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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Redefinied Internals ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; (defun kill-group (group to-group)
;;   (if (and (typep to-group 'dwm-group)
;;            (>= (+ (length (group-windows to-group))
;;                   (length (group-windows group)))
;;                *maximum-dwm-group-windows*))
;;       (message
;;        "unable to kill nonempty group - the group to send windows to is full. ")
;;       (unless (eq group to-group)
;;            (let ((screen (group-screen group)))
;;              (merge-groups group to-group)
;;              (setf (screen-groups screen) (remove group (screen-groups screen)))
;;              (netwm-update-groups screen)
;;              (netwm-set-group-properties screen)))))

;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Utility Functions ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;

;; (defun generate-new-dwm-group ()
;;   (let ((name
;;           (read-one-line (current-screen)
;;                          "To many windows in group. Enter a new group name: ")))
;;     (gnewbg-dwm name)))

;; (defun find-suitable-dwm-group (current)
;;   (let ((possible-groups
;;           (non-hidden-groups (screen-groups (group-screen current)))))
;;     (loop for group in possible-groups
;;           when (and (not (equal current group))
;;                     (typep group 'dwm-group)
;;                     (< (length (group-windows group))
;;                         *maximum-dwm-group-windows*))
;;             do (return-from find-suitable-dwm-group group))
;;     (generate-new-dwm-group)))

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

;;;;;;;;;;;;;;;;;;
;;; Add Window ;;;
;;;;;;;;;;;;;;;;;;

;; (defun dwm-group-add-window (window)
;;   "this function should be hung on *new-window-hook*, and implements dwm-style 
;; tiling with a master window and a window stack. "
;;   (let* ((group (window-group window))
;;       (frames (group-frames group)))
;;     (case (length frames)
;;       (1
;;        (when (> (length (group-windows group)) 1)
;;       (let ((prev-win (dwm-group-master-window group)))
;;         (push prev-win (dwm-group-window-stack group))
;;         ;; the 0th frame is always master
;;         (dwm-hsplit "2/3")
;;         (pull-window prev-win (car (remove (frame-by-number group 0)
;;                                            (group-frames group))))
;;         (pull-window window (frame-by-number group 0))))
;;        (setf (dwm-group-master-window group) window))
;;       (otherwise
;;        (let* ((master-frame (or (window-frame (dwm-group-master-window group))
;;                              (frame-by-number group 0)))
;;            (frames-no-master (remove master-frame frames)))
;;       (push (dwm-group-master-window group) (dwm-group-window-stack group))
;;       (pull-window (dwm-group-master-window group) (car frames-no-master))
;;       (handler-case (progn (dwm-vsplit)
;;                            (dwm-balance-stack-tree group)
;;                            (pull-window window (frame-by-number group 0))
;;                            (focus-frame group (frame-by-number group 0))
;;                            (setf (dwm-group-master-window group) window))
;;         (dwm-group-too-many-windows ()
;;           ;; (message "handler-case for dwm-group-too-many-windows")
;;           ;; undo the reorganization we did above. 
;;           (pull-window (dwm-group-master-window group)
;;                        (frame-by-number group 0))
;;           (pop (dwm-group-window-stack group))
;;           ;; move the window to a new group
;;           (let ((new-group (find-suitable-dwm-group group)))
;;             (pull-to-group window new-group)
;;             ;; focus the window
;;             (focus-all window)
;;             ;; recall this function on the window to properly handle the handoff
;;             (dwm-group-add-window window)))))))))

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

(define-key *root-map* (kbd "z") "dwm-switch")

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
        (focus-frame group (frame-by-number group 0))
        (dwm-balance-stack-tree group)))))

(defcommand dwm-retile () ()
  (dwm-emergency-retile))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; group-add-window method ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; (defvar *move-window-hook* nil
;;   "called at the very end of move-window-to-group. functions take a window")

;; (defun move-window-to-group (window to-group)
;;   (if (equalp to-group (window-group window))
;;       (message "That window is already in the group ~a." (group-name to-group))
;;       (labels ((really-move-window (window to-group)
;;                  (unless (eq (window-group window) to-group)
;;                    (hide-window window)
;;                    ;; house keeping
;;                    (setf (group-windows (window-group window))
;;                          (remove window (group-windows (window-group window))))
;;                    (group-delete-window (window-group window) window)
;;                    (setf (window-group window) to-group
;;                          (window-number window) (find-free-window-number to-group))
;;                    (push window (group-windows to-group))
;;                    (xlib:change-property (window-xwin window) :_NET_WM_DESKTOP
;;                                          (list (netwm-group-id to-group))
;;                                          :cardinal 32)
;;                    (group-add-window to-group window))))
;;         ;; When a modal window is moved, all the windows it shadows must be moved
;;         ;; as well. When a shadowed window is moved, the modal shadowing it must
;;         ;; be moved.
;;         (cond
;;           ((window-modal-p window)
;;            (mapc (lambda (w)
;;                    (really-move-window w to-group))
;;                  (append (list window) (shadows-of window))))
;;           ((modals-of window)
;;            (mapc (lambda (w)
;;                    (move-window-to-group w to-group))
;;                  (modals-of window)))
;;           (t
;;            (really-move-window window to-group)))
;;         (run-hook-with-args *move-window-hook* window))))

;; (defun winmove-hook-fn (window))

;; (add-hook *move-window-hook* )

;; (defun dwm-emergency-new-group (group window)
;;   (let ((new-group (find-suitable-dwm-group group)))
;;     (pull-to-group window new-group)
;;     (focus-all window)
;;     (message "Window ~a sent to group ~a" window new-group)))

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

(defparameter *moving-superfluous-window* nil)

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
         ;; we always want the new window to be the new master - that means frame 0
         (setf (window-frame window) (frame-by-number group 0))
         (case (length (group-windows group))
           (1
            (message "moving from 0 to 1 window")
            (setf (dwm-group-master-window group) window))
           (2
            (message "moving from 1 to 2 windows")
            (let ((frame (frame-by-number group 0)))
              ;; we want to split the master frame.
              (dwm-hsplit-frame frame "2/3"))
            (let* ((prev-win (dwm-group-master-window group))
                   (prev-win-new-frame (car (remove (frame-by-number group 0)
                                                    (group-frames group)))))
              ;; we can write better stack frame selection later, for now we just
              ;; want to get a new frame on the stack. 
              (push prev-win (dwm-group-window-stack group))
              ;; because we just hsplit, we know that there must be 2+ groups
              (setf (window-frame prev-win) prev-win-new-frame
                    (frame-window prev-win-new-frame) prev-win
                    (window-frame window) (frame-by-number group 0)
                    (frame-window (frame-by-number group 0)) window
                    (dwm-group-master-window group) window)))
           (otherwise
            (message "We already have the stack set up, so put windows there!")
            (let* ((master-frame (frame-by-number group 0))
                   (frames-no-master (remove master-frame (group-frames group)))
                   (frame-to-split (car frames-no-master)))
              (handler-case
                  (progn
                    (dwm-vsplit-frame frame-to-split)
                    ;; push old master onto the stack
                    (push (dwm-group-master-window group)
                          (dwm-group-window-stack group))
                    ;; set new master to the master
                    (setf (dwm-group-master-window group) window)
                    ;; fix up the frames
                    (setf (window-frame window) (frame-by-number group 0)
                          (window-frame (car (dwm-group-window-stack group)))
                          (or (car (find-empty-frames group))
                              (error "No Empty Frames in group ~S! Something has gone terribly wrong!" group)))
                    ;; focus the master window
                    (focus-frame group (frame-by-number group 0))
                    
                    (dwm-balance-stack-tree group))
                (dwm-group-too-many-windows ()
                  (setf (moving-superfluous-window group) window)
                  (let ((new-group (gnew (gen-group-name))))
                    (move-window-to-group window new-group))
                  ;; (move-window-to-group window (find-open-dwm-group group))
                  )))))
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
         ;; if were moving a superfluous window out of the group, we dont want
         ;; to do nothin as we aint done nothin to set it up. 
         (setf (moving-superfluous-window group) nil))
        ((equal window (dwm-group-master-window group))
         ;; if were removing the master window
         (message "group-windows: ~S" (group-windows group))
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
                    ;; remove all other frames
                    (loop for remframe in (remove (frame-by-number group 0)
                                                  (group-frames group))
                          do (setf (tile-group-frame-head group head)
                                   (remove-frame tree remframe)))

                    (setf (tile-group-current-frame group) master-frame)
                    
                    (focus-frame group master-frame)
                    (update-decoration (frame-window master-frame))
                    ;; make sure the frame is balanced - ie take whole screen
                    ;; (setf (frame-x master-frame) 0
                    ;;       (frame-y master-frame) 0
                    ;;       )
                    
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
               (let ((f (window-frame window)))
                 ;; maybe pick a new window for the old frame
                 (when (eq (frame-window f) window)
                   (frame-raise-window group f
                                       (first (frame-windows group f)) nil))))))
        ((member window (dwm-group-window-stack group))
         ;; If were removing a stack window, we want to pop it off and reorganize
         ;; the stack
         (let* ((stackwin (car (member window (dwm-group-window-stack group))))
                (new-stack (remove stackwin (dwm-group-window-stack group)))
                (stackwins-frame (window-frame stackwin))
                (head (frame-head group stackwins-frame))
                (tree (tile-group-frame-head group head)))
           (unless new-stack
             (setf (dwm-group-master-window group) stackwin))
           ;; we want to make the new frame tree not contain stackwins frame, as it
           ;; isnt holding a window any more. 
           (setf (tile-group-frame-head group head)
                 (remove-frame tree stackwins-frame)
                 (dwm-group-window-stack group) new-stack)
           (tree-iterate tree (lambda (leaf) (sync-frame-windows group leaf)))
           (focus-frame group (frame-by-number group 0))
           (dwm-balance-stack-tree group)
           (loop for frame in (group-frames group)
                 do (sync-frame-windows group frame))))))

;; (defmethod group-delete-window ((group dwm-group) (window dwm-window))
;;   (if (equal window *moving-superfluous-window*)
;;       (setf *moving-superfluous-window* nil)
;;       (cond ((> (length (group-windows group)) (- *maximum-dwm-group-windows* 1))
;;              ;; this case keeps us from crashing stump, but doesnt fix our issues.
;;              (cond ((member window (dwm-group-window-stack group))
;;                     (let ((superfluous-windows
;;                             (dwm-find-superfluous-windows group)))
;;                       (when (> (length superfluous-windows) 0)
;;                         (pull-window (car superfluous-windows)
;;                                      (window-frame window)))
;;                       (tree-iterate
;;                        (tile-group-frame-head group (current-head group))
;;                        (lambda (l) (sync-frame-windows group l)))
;;                       (setf (dwm-group-master-window group)
;;                             (frame-window (frame-by-number group 0)))))
;;                    ((equal window (dwm-group-master-window group))
;;                     (setf (dwm-group-master-window group)
;;                           (frame-window (frame-by-number group 0))))))
;;             ((member window (dwm-group-window-stack group))
;;              (let* ((stackwin (member window (dwm-group-window-stack group)))
;;                     (new-stack (remove window (dwm-group-window-stack group)))
;;                     (new-master (car stackwin))
;;                     (new-master-frame (window-frame new-master))
;;                     (head (frame-head group new-master-frame))
;;                     (tree (tile-group-frame-head group head)))
;;                (setf (tile-group-frame-head group head)
;;                      (remove-frame tree new-master-frame)
;;                      (dwm-group-window-stack group) new-stack)
;;                (tree-iterate tree
;;                              (lambda (leaf)
;;                                (sync-frame-windows group leaf)))
;;                (focus-frame group (frame-by-number group 0))
;;                (dwm-balance-stack-tree group)))
;;             ((= (length (group-windows group)) 0)
;;              (push (list "no windows in the group. " ;; (group-windows group)
;;                          )
;;                    *dwm-dbg*)
;;              (let ((f (window-frame window)))
;;                ;; maybe pick a new window for the old frame
;;                (when (eq (frame-window f) window)
;;                  (frame-raise-window group f (first (frame-windows group f))
;;                                      nil))))
;;             ((= (length (group-windows group)) 1)
;;              ;; (push (list "only one window in the group. " (group-windows group)) *dwm-dbg*)
;;              (let* ((only (pop (dwm-group-window-stack group)))
;;                     (head (current-head group))
;;                     (tree (tile-group-frame-head group head)))
;;                (pull-window only (frame-by-number group 0))
;;                (focus-frame group (frame-by-number group 0))
;;                (loop for remframe in (remove (frame-by-number group 0)
;;                                              (group-frames group))
;;                      do (setf (tile-group-frame-head group head)
;;                               (remove-frame tree remframe)))
;;                (tree-iterate (tile-group-frame-head group head)
;;                              (lambda (l) (sync-frame-windows group l)))
;;                (setf (dwm-group-master-window group) only)))
;;             (t
;;              (let* ((new-master (pop (dwm-group-window-stack group)))
;;                     (nm-frame (window-frame new-master))
;;                     (head (current-head group))
;;                     (tree (tile-group-frame-head group head)))
;;                (pull-window new-master (frame-by-number group 0))
;;                (setf (tile-group-frame-head group head) (remove-frame tree nm-frame))
;;                (tree-iterate tree (lambda (l) (sync-frame-windows group l)))
;;                (setf (dwm-group-master-window group) new-master)
;;                (dwm-balance-stack-tree group))))))
