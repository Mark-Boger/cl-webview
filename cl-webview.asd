(defpackage #:cl-webview-system
  (:use #:asdf #:cl)
  (:export #:ccfile))

(in-package #:cl-webview-system)

(defclass ccfile (source-file)
  ((type :initform "cc")))

(defmethod output-files ((o compile-op) (c ccfile))
  (list (make-pathname :name (component-name c) :type "so")))

(defmethod perform ((o compile-op) (c ccfile))
  (let ((in (first (input-files o c)))
        (out (first (output-files o c))))
    (uiop:run-program
     (format nil
             "g++ $(pkg-config --cflags --libs gtk+-3.0 webkit2gtk-4.0) -fPIC -shared ~a -o ~a"
             in out))))

(defmethod perform ((o load-op) (c ccfile))
  (let ((in (first (input-files o c))))
    (uiop:call-function "cffi:load-foreign-library" in)))

(defmethod operation-done-p ((o compile-op) (c ccfile))
  (let ((in (first (input-files o c)))
        (out (first (output-files o c))))
    (and (probe-file in)
         (probe-file out)
         (> (file-write-date out) (file-write-date in)))))

(asdf:defsystem #:cl-webview
  :description "Describe cl-webview here"
  :author "Mark Boger <93mar.bog@gmail.com>"
  :license  "MIT"
  :version "0.0.1"
  :depends-on (:cffi
               :alexandria
               :com.inuoe.jzon
               :trivial-garbage)
  :serial t
  :components ((:file "package")
               (:ccfile "webview")
               (:file "cl-webview")))

