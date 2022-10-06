;;; statistic-elisp.el -- Summary

;;; Commentary:
;;; Statistic formulas

;;; Code:

(defun fact (n)
  "Calculate the factorial of a number N."
  (if (<= n 1)
      1
    (* n
       (fact (- n 1)))))

(defun perm-no-rip (n)
  "Calculate the number N of simple permutations."
  (interactive "nInsert a Num: ")
  (message "Result: %d" (fact n)))

(defun perm-rip (n nr)
  "Calculate the number N of permutations with repetitions NR."
  (interactive "nInsert a Num: \nnInsert the number of repetitions: ")
  (let ((prod-rep (apply '* (mapcar (lambda (n)
                                      (fact n))
                                    (statistic-compose-list nr)))))
    (message "Result: %d" (/ (fact n) prod-rep))))

(defun statistic-compose-list (n)
  "Create a list of numbers with length N."
  (let ((new-list ()))
    (while (> n 0)
      (setq new-list
            (append new-list (list
                              (read-number "Give me a number: "))))
      (cl-decf n))
    new-list))

(defun disp-no-rip (n k)
  "Calculate the number N of simple arrangements K."
  (interactive
   "Give me the number of elements: \nnGive me the number of combo: ")
  (message "Result: %d" (/ (fact n)
                           (fact (- n k)))))

(defun disp-rip (n k)
  "Calculate the number N of arrangements with K repetitions."
  (interactive
   "nGive me the number of elements: \nnGive me the number of combo: ")
  (message "Result: %d" (expt n k)))

(defun combo-no-rip (n r)
  "Calculate the number N of simple combinations R."
  (interactive
   "nGive me the number of elements: \nnGive me the number of combo: ")
  (message "Result: %d" (/ (fact n)
                           (* (fact r) (fact (- n r))))))

(defun combo-rip (n r)
  "Calculate the number N of combinations with R repetitions."
  (interactive
   "nGive me the number of elements: \nnGive me the number of combo: ")
  (message "Result: %d" (/ (fact (- (+ n r) 1))
                           (* (fact r) (fact (- n 1))))))

(provide 'statistic-elisp)
;;; statistic-elisp.el ends here
