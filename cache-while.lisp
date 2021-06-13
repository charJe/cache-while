(in-package #:cache-while)

(defmacro cache-while (place eqs-values &body body)
  (let ((values (gensym))
        (eqs (gensym))
        (spots (loop for i from 1 to (length eqs-values)
                         collect i))
        (result 0)
        (mvalues (map 'list 'second eqs-values))
        (meqs (map 'list 'first eqs-values))
        (cache (gensym)))
    `(let ((,values (list ,@mvalues))
           (,eqs (quote ,meqs))
           (,cache ,(if (symbolp place)
                        `(when (boundp ',place)
                           ,place)
                        place)))
       (if (or (null ,cache)
                (notevery (lambda (val1 test val2)
                            (funcall test val1 val2))
                          (map 'list (lambda (spot)
                                       (elt ,cache spot))
                               ',spots)
                          ,eqs
                          ,values))
           (progn
             (unless ,cache
               (setf ,place
                     (make-array ,(+ 1 (length spots))
                                 :initial-element nil)))
             (mapc
              (lambda (spot value)
                (setf (elt ,place spot)
                      value))
              ',spots
              ,values)
             (setf (elt ,place ,result)
                   (progn
                     ,@body)))
           (elt ,cache ,result)))))
