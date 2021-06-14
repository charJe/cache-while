(in-package #:cache-while)

(defmacro cache-while (place eqs-values &body body)
  (let ((values (gensym))
        (eqs (gensym))
        (mspots (loop for i from 1 to (length eqs-values)
                      collect i))
        (result 0)
        (mvalues (map 'list 'second eqs-values))
        (meqs (map 'list 'first eqs-values))
        (spots (gensym))
        (cache (gensym "CACHE"))
        (calculated-place (gensym)))
    `(let* ((,values (list ,@mvalues))
            (,eqs ',meqs)
            (,spots ',mspots)
            (,calculated-place
              (or ,(if (symbolp place)
                       `(and (boundp ',place)
                             ,place)
                       place)
                  ;; PLACE is table of arrays
                  (setf ,place (make-hash-table))))
            ;; CACHE is array
            (,cache
              (or (gethash ',cache ,calculated-place)
                  (setf (gethash ',cache ,calculated-place)
                        (make-array ,(+ 1 (length mspots))
                                    :initial-element nil)))))
       (if (notevery
            (lambda (val1 test val2)
              (funcall test val1 val2))
            (map 'list (lambda (spot)
                         (elt ,cache spot))
                 ,spots)
            ,eqs
            ,values)
           (progn
             (mapc
              (lambda (spot value)
                (setf (elt ,cache spot)
                      value))
              ,spots
              ,values)
             (setf (elt ,cache ,result)
                   (progn ,@body)))
           (elt ,cache ,result)))))
