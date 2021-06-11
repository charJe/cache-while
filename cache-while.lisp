(in-package #:cache-while)

(defun cache-bound-p (place symbol)
  (not (null (gethash symbol place nil))))

(defmacro cache-while (place eqs-values &body body)
  (let ((values (gensym))
        (eqs (gensym))
        (variables (loop repeat (length eqs-values)
                         collect (gensym "CACHE")))
        (result (gensym "CACHE"))
        (mvalues (map 'list 'second eqs-values))
        (meqs (map 'list 'first eqs-values))
        (mplace (gensym)))
    `(let ((,values (list ,@mvalues))
           (,eqs (quote ,meqs))
           (,mplace ,place))
       (if (or (some (lambda (var)
                       (not (cache-bound-p ,mplace var)))
                     (quote ,(cons result variables)))
               (notevery (lambda (val1 test val2)
                           (funcall test val1 val2))
                         (map 'list (lambda (var)
                                      (gethash var ,mplace))
                              (quote ,variables))
                         ,eqs
                         ,values))
           (progn
             (mapc
              (lambda (var value)
                (setf (gethash var ,mplace)
                      value))
              (quote ,variables)
              ,values)
             (setf (gethash ',result ,mplace)
                   (progn
                     ,@body)))
           (nth-value 0 (gethash ',result ,mplace))))))