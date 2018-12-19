(require 'calc)

(defmath is-odd (num)
  (= (logand num 1) 1))

(defmath 3x+1 (num)
  (if (is-odd num)
      (1+ (* 3 num))
    (/ num 2)))

(defmath transform-3x+1 (num)
  (while (> num 1)
    (setq num (3x+1 num)))
  num)

(defun test-3x+1-conjecture (max-num)
  (loop for n from 2 to max-num
        unless (= 1 (calcFunc-transform-3x+1 n))
        collect n))

(defmacro measure-time (&rest body)
  "Measure and return the running time of the code in body."
  (declare (indent defun))
  (let ((start (make-symbol "start")))
    `(let ((,start (float-time)))
       ,@body
       (- (float-time) ,start))))
