(require :cl-webview)

(in-package :cl-webview)

(defparameter *page* "data:text/html,
<!doctype html>
<html>
    <body>
        <div>Hello, World</div>
        <br/>
        <div id=\"click\">Counter:<span id=\"count\">0</span></div>
    </body>
    <script>
        window.onload = function() {
            let count = document.getElementById('count');
            let click = document.getElementById('click');

            click.onclick = async () => {
                try {
                    let new_count = await counter();
                    count.innerHTML = new_count;
                } catch (err) {
                    alert(err);
                }
            }

        };
    </script>
</html>")

(defparameter *w* (create :debug t
                          :title "Lisp Example"
                          :width 600
                          :height 800
                          :page *page*))

(defparameter *counter* 0)

(defbinding *w* counter ()
  (incf *counter*))

(run *w*)

(uiop:quit)
