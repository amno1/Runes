;;;; Counts instructions, register-to-register MOVs, stack references and tag
;;;; shifts in an SBCL disassembly:
;;;;   sbcl --script count-movs.lisp results/inflate-block-before.asm.txt

(load "~/quicklisp/setup.lisp")
(ql:quickload :cl-ppcre :silent t)

(let ((insns '()))
  (with-open-file (in (second sb-ext:*posix-argv*))
    (loop for line = (read-line in nil)
          while line
          do (unless (or (eql 0 (search "; Size" line))
                         (eql 0 (search ";   " line)))
               ;; [;] [address:] [label:] bytes instruction [; comment]
               (ppcre:register-groups-bind (insn)
                   ("^;?\\s*(?:[0-9A-F]+:\\s+)?(?:L\\d+:\\s+)?[0-9A-F]+\\s+(.*)$"
                    (string-right-trim '(#\Space #\Tab #\Return) line))
                 (push (string-trim " " (subseq insn 0 (position #\; insn)))
                       insns)))))
  (flet ((matching (regex)
           (count-if (lambda (insn) (ppcre:scan regex insn)) insns)))
    (format t "insns ~d reg-reg MOV ~d stack ~d SAR/SHL1 ~d~%"
            (length insns)
            (matching "^MOV [RE]\\w+, [RE]\\w+$")
            (matching "RBP-")
            (matching "^(SAR|SHL) \\w+, 1$"))))
