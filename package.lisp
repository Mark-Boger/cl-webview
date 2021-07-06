(defpackage #:cl-webview
  (:use #:cffi #:cl)
  (:local-nicknames (:a :alexandria)
                    (:jzon :com.inuoe.jzon))
  (:export
   #:+hint-none+
   #:+hint-min+
   #:+hint-max+
   #:+hint-fixed+
   #:webview
   #:create
   #:destroy
   #:run
   #:get-window
   #:init
   #:evaluate
   #:returns
   #:bind
   #:defbinding
   #:pointer
   #:title
   #:height
   #:width
   #:page
   #:hint))
