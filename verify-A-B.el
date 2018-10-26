(require 'cl)
  (require 'calc)

  (defmacro measure-time (&rest body)
  "Measure and return the running time of the code in body."
  (declare (indent defun))
  (let ((start (make-symbol "start")))
    `(let ((,start (float-time)))
        ,@body
        (- (float-time) ,start))))

  

  (defun get-next-A (a)
    (math-floor (math-quotient (math-mul a sq2many) ten2many)))

  (defun get-next-B (b)
    (math-floor (math-quotient (math-mul b sq22many) ten2many)))

;;  (defun get-next-A (n)
;;    (/ (+ (* n 141) (/ (+ (* n 421356) (/ (+ (* n 237309) (/ (+ (* n 504880) (/ (* n 168872) 1000000)) 1000000)) 1000000)) 1000000)) 100))
  
;;  (defun get-next-B (n)
;;    (/ (+ (* n 341) (/ (+ (* n 421356) (/ (+ (* n 237309) (/ (+ (* n 504880) (/ (* n 168872) 1000000)) 1000000)) 1000000)) 1000000)) 100))

  (defun verify-A-B (&optional numver start start2)
    (if (numberp numver)   nil  (setq numver 1000))
    (if (numberp   start)  nil  (setq start  1))
    (if (numberp  start2)  nil  (setq start2 1))

    (setq sq2many  (math-read-number "141421356237309504880168872")
          sq22many (math-read-number "341421356237309504880168872")
          ten2many (math-read-number "100000000000000000000000000"))

    (setq ver (get-next-A start)
          numver  (+ ver numver)
          failed            nil)

    (while (and (< ver numver) (not failed))
      (cond 
        ((= (get-next-A  start) ver) (setq start  (1+  start)))
        ((= (get-next-B start2) ver) (setq start2 (1+ start2)))
        (t (setq failed t))
      )
        (if (= (mod ver 10000) 0) (print ver) nil)
        (if (not failed)
          (setq ver (1+ ver)))
    )

    (if (= ver numver) 
      (print "Verified")
      (print "Failed"))

    (print start)
    (print start2)
    (print (get-next-A start))
    (print (get-next-B start2))
    (print ver)
  )

  (defun try-a-bunch ()
    (setq time-passed (measure-time (verify-A-B 5000000000 806101730553 333898269448)))
    (print time-passed))
