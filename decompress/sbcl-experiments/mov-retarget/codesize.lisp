(let ((n 0) (bytes 0))
  (sb-vm:map-allocated-objects
   (lambda (obj type size) (declare (ignore type))
     (when (typep obj 'sb-kernel:code-component)
       (incf n) (incf bytes (sb-kernel:%code-text-size obj))))
   :all)
  (format t "code objects ~d, text bytes ~d~%" n bytes))
