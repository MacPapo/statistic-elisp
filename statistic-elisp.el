;;; statistic-elisp.el -- Summary

;;; Commentary:
;;; Statistic formulas

;;; Code:

(require 'cl-lib)

(defun fact (n)
  "Calculate the factorial of a number N."
  (if (<= n 1)
      1
    (* n (fact (- n 1)))))

(defun perm-no-rip (n)
  "Calculate the number N of simple permutations."
  (interactive "nInsert a Num: ")
  (let ((res (fact n)))
    (message "Result: %d" res)))

(defun perm-rip (n nr)
  "Calculate the number N of permutations with repetitions NR."
  (interactive "nInsert a Num: \nnInsert the number of repetitions: ")
  (let* ((prod-rep (apply '* (mapcar (lambda (n)
                                       (fact n))
                                     (statistic-compose-list nr))))
         (res (/ (fact n) prod-rep)))
    (message "Result: %d" res)
    res))

(defun statistic-compose-list (n)
  "Create a list of numbers with length N."
  (let ((new-list ()))
    (while (> n 0)
      (setq new-list
            (append new-list (list
                              (read-number "Give me a number: "))))
      (cl-decf n))
    new-list))

(defun disp-no-rip (n r)
  "Calculate the number N of simple arrangements R."
  (interactive
   "nGive me the number of elements: \nnGive me the number of combo: ")
  (let ((res (/ (fact n)
                (fact (- n r)))))
    (message "Result: %d" res)
    res))

(defun disp-rip (n r)
  "Calculate the number N of arrangements with R repetitions."
  (interactive
   "nGive me the number of elements: \nnGive me the number of combo: ")
  (let ((res (expt n r)))
    (message "Result: %d" res)
    res))

(defun combo-no-rip (n r)
  "Calculate the number N of simple combinations R."
  (interactive
   "nGive me the number of elements: \nnGive me the number of combo: ")
  (let ((res (/ (fact n)
                (* (fact r) (fact (- n r))))))
    (message "Result: %d" res)
    res))

(defun combo-rip (n r)
  "Calculate the number N of combinations with R repetitions."
  (interactive
   "nGive me the number of elements: \nnGive me the number of combo: ")
  (let ((res (/ (fact (- (+ n r) 1))
                (* (fact r) (fact (- n 1))))))
    (message "Result: %d" res)
    res))

(provide 'statistic-elisp)
;;; statistic-elisp.el ends here
