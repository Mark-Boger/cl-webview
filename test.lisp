(require :cl-webview)

(in-package :cl-webview)

(defparameter *page* "data:text/html,
<!doctype html>
<html>
    <body>
        <div>Hello, World</div>
        <br/>
        <div id=\"click\">Counter:<span id=\"count\">0</span></div>
        <button id=\"inc\">Increment</button>
        <button id=\"dec\">Decrement</button>
    </body>
    <script>
        window.onload = function() {
            let count = document.getElementById('count');
            let inc = document.getElementById('inc');
            let dec = document.getElementById('dec');

            inc.onclick = async () => {
                try {
                    let new_count = await inc_counter();
                    count.innerHTML = new_count;
                } catch (err) {
                    alert(err);
                }
            }

            dec.onclick = async () => {
                try {
                    let new_count = await dec_counter();
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

(defbinding *w* inc_counter ()
  (incf *counter*))

(defbinding *w* dec_counter ()
  (decf *counter*))

(run *w*)

(uiop:quit)
