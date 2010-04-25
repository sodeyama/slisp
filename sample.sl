;; fibonacci on Y combinator
(defun fib0 (f)
  (lambda (n)
    (cond ((= n 0) 1)
          ((= n 1) 1)
          (t (+ (f (- n 1)) (f (- n 2)))))))
(defun Y (f)
  ((lambda (x)
     (f (x x)))
   (lambda (x)
     (f (x x)))))
(print "The result of fibonacci 5")
(print ((Y fib0) 5))


;; some sample
(defun fib (n)
  (if (= n 0) 0
    (if (= n 1) 1
      (+ (fib (- n 1)) (fib (- n 2))))))
(print (fib 5))
(setq result ((lambda (x y) (fib (+ x y))) 5 3))
(defun hoge (x)
  (let ((a 10))
    (progn
      (print "aa")
      (+ (* a result) x))))
(hoge 3)


;; takeuchi function
(defun tak (x y z)
  (if (not (< y x)) z
    (tak
     (tak (- x 1) y z)
     (tak (- y 1) z x)
     (tak (- z 1) x y))))
;(tak 12 6 0)


;; Perfect Y Combinator sample
(print 
 (((lambda (f)
     ((lambda (x)
        (f (x x)))
      (lambda (x)
        (f (x x)))))
   (lambda (f)
     (lambda (n)
       (if (= n 0)
           1
         (* n (f (- n 1))))))) 8))


;; Current continuation sample
(print "starting..")
(setq hoge nil)
(let ((a 1))
  (let ((b 2))
    (print (+ (+ a b)
              (call/cc (lambda (cc)
                         (progn
                           (setq hoge cc)
                           (cc 4))))))))
(print "ending..")
(print (hoge 5))
(print (hoge 6))


;; Current containuation sample2
(print "starting call/cc sample2")
(let ((a 3))
  (print (* a
            (call/cc (lambda(cc)
                       (progn
                         (setq hoge2 cc)
                         (cc 3)))))))
(print "ending..")
(print (hoge2 4))