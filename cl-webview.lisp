(in-package #:cl-webview)

(eval-when (:compile-toplevel :load-toplevel :execute)
  ;; We need to remove the :DIVIDE-BY-ZERO trap to call into C code
  #+sbcl
  (sb-int:set-floating-point-modes :traps nil))

(defctype webview-t :pointer)

(defconstant +hint-none+ 0
  "Width and height are default size.")
(defconstant +hint-min+ 1
  "Width and height are minimum bounds.")
(defconstant +hint-max+ 2
  "Width and height are maximum bounds.")
(defconstant +hint-fixed+ 3
  "Window size can't be changed by user.")

(defclass webview ()
  ((pointer :initarg :pointer :reader pointer)
   (title :initarg :title :accessor title)
   (height :initarg :height :accessor height)
   (width :initarg :width :accessor width)
   (page :initarg :page :accessor page)
   (hint :initarg :hint :accessor hint :initform +hint-none+)))

(defmethod (setf title) (new-title (obj webview))
  "Set the `title' of a `webview' object"
  (prog1 (setf (slot-value obj 'title) new-title)
    (webview-set-title (pointer obj) new-title)))

(defmethod (setf page) (new-page (obj webview))
  "Set the `page' of a `webview' object'"
  (prog1 (setf (slot-value obj 'page) new-page)
    (webview-navigate (pointer obj) new-page)))

(defmethod (setf height) (new-height (obj webview))
  "Set the `height' of a `webview' object. Both the `height' and the `width' must be set to take
effect."
  (prog1 (setf (slot-value obj 'height) new-height)
    (when (and (height obj) (width obj))
      (webview-set-size (pointer obj) (width obj) (height obj) (hint obj)))))

(defmethod (setf width) (new-width (obj webview))
  "Set the `width' of a `webview' object. Both the `height' and the `width' must be set to take
effect."
  (prog1 (setf (slot-value obj 'width) new-width)
    (when (and (height obj) (width obj))
      (webview-set-size (pointer obj) (width obj) (height obj) (hint obj)))))

(defmethod initialize-instance :after ((instance webview) &rest initargs &key)
  (declare (ignore initargs))
  (let ((pointer (pointer instance))
        (title (title instance))
        (page (page instance))
        (height (height instance))
        (width (width instance))
        (hint (hint instance)))
    (when title
      (webview-set-title pointer title))
    (when page
      (webview-navigate pointer page))
    (when (and height width)
      (webview-set-size pointer width height hint))
    (trivial-garbage:finalize instance (lambda ()
                                         ;; We do `webview-destroy' instead of `destroy' so we
                                         ;; don't have a reference to our instance in the finalizer
                                         (webview-destroy pointer)))))

(defun create (&key debug parent title height width page (hint +hint-none+))
  "Create a new instance of `webview'. If `debug' is true enable developer tools when the platform
supports them. When `parent' is a native window handle the new webview will be embedded into the
given `parent'."
  (make-instance 'webview
                 :pointer (webview-create (bool->int debug) (window-pointer parent))
                 :title title
                 :height height
                 :width width
                 :page page))

(defcfun "webview_create" webview-t
  (debug :int)
  (window :pointer))

(defun bool->int (bool)
  (if bool 1 0))

(defun window-pointer (webview)
  (if webview (get-window webview) (null-pointer)))

(defgeneric get-window (webview)
  (:documentation "Returns a native window handle pointer. When using GTK the pointer is a
GtkWindow pointer.")
  (:method ((webview webview))
    (webview-get-window (pointer webview))))

(defcfun "webview_get_window" :pointer
  (w webview-t))

(defgeneric destroy (webview)
  (:documentation "Destroys a webview and closes the native window.")
  (:method ((webview webview))
    (webview-destroy (pointer webview))))

(defcfun "webview_destroy" :void
  (w webview-t))

(defgeneric run (webview)
  (:documentation "Runs the main loop until it's terminated.")
  (:method ((webview webview))
    (webview-run (pointer webview))))

(defcfun "webview_run" :void
  (w webview-t))

(defgeneric terminate (webview)
  (:documentation "Stops the main loop. This function can be called from a backgroud thread.")
  (:method ((webview webview))
    (webview-terminate (pointer webview))))

(defcfun "webview_terminate" :void
  (w webview-t))

(defcfun "webview_set_title" :void
  (w webview-t)
  (title :string))

(defcfun "webview_navigate" :void
  (w webview-t)
  (url :string))

(defgeneric init (webview js)
  (:documentation "Injects javascript code at the initialization of new pages. Every time
webview opens a new page this initialization code will be executed. Execution happens before
`window.onload'.")
  (:method ((webview webview) js)
   (webview-init (pointer webview) js)))

(defcfun "webview_init" :void
  (w webview-t)
  (js :string))

(defgeneric evaluate (webview js)
  (:documentation "Evaluate arbitrary javascript code. Evaluation is asynchronous and the results
are ignored. Use `defbinding' to define functions that can return results.")
  (:method ((webview webview) js)
   (webview-eval (pointer webview) js)))

(defcfun "webview_eval" :void
  (w webview-t)
  (js :string))

(defcfun "webview_set_size" :void
  (w webview-t)
  (width :int)
  (height :int)
  (hints :int))

(defmacro defbinding (webview name (&rest args) &body body)
  "Define a function that will be available in the global javascript context."
  (a:with-gensyms (seq req arg fn result status err)
    `(progn
       (defcallback ,name :void ((,seq :string) (,req :string) (,arg :pointer))
         (declare (ignore ,arg))
         (flet ((,fn (,@args) ,@body))
           (let (,result
                 (,status 0))
             (handler-case
                 (setf ,result (apply #',fn (json-array->list ,req)))
               (error (,err)
                 (setf ,result (format nil "~a" ,err))
                 (setf ,status -1)))
             (returns ,webview ,seq ,status ,result))))
       (bind ,webview ',name))))

(defun json-array->list (array-string)
  (map 'list #'identity (jzon:parse array-string)))

(defgeneric returns (webview seq status result)
  (:documentation "Return `result' from a native binding. When `status' is 0 `result' is 
expected to be valid JSON, other wise `result' should be an error.")
  (:method ((webview webview) seq status result)
    (webview-return (pointer webview) seq status (jzon:stringify result))))

(defcfun "webview_return" :void
  (w webview-t)
  (seq :string)
  (status :int)
  (result :string))

(defgeneric bind (webview name)
  (:documentation "Bind a C function with `name' that will be available as a global javascript 
function.")
  (:method ((webview webview) name)
    (let ((fn (eval `(callback ,name)))
          (fn-id (string-downcase (string name)))
          (pointer (pointer webview)))
      (webview-bind pointer fn-id fn (null-pointer)))))

(defcfun "webview_bind" :void
  (w webview-t)
  (name :string)
  (fn :pointer)
  (arg :pointer))
