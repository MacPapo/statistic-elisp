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
    (message "Result: %.7f" res)))

(defun perm-rip (n nr)
  "Calculate the number N of permutations with repetitions NR."
  (interactive "nInsert a Num: \nnInsert the number of repetitions: ")
  (let* ((prod-rep (apply '* (mapcar (lambda (n)
                                       (fact n))
                                     (statistic-compose-list nr))))
         (res (/ (float (fact n)) prod-rep)))
    (message "Result: %.7f" prod-rep)
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
  (let ((res (/ (float (fact n))
                (fact (- n r)))))
    (message "Result: %.7f" res)
    res))

(defun disp-rip (n r)
  "Calculate the number N of arrangements with R repetitions."
  (interactive
   "nGive me the number of elements: \nnGive me the number of combo: ")
  (let ((res (expt (float n) r)))
    (message "Result: %.7f" res)
    res))

(defun combo-no-rip (n r)
  "Calculate the number N of simple combinations R."
  (interactive
   "nGive me the number of elements: \nnGive me the number of combo: ")
  (let ((res (/ (float (fact n))
                (* (float (fact  r)) (fact (float (- n r)))))))
    (message "Result: %.7f" res)
    res))

(defun combo-rip (n r)
  "Calculate the number N of combinations with R repetitions."
  (interactive
   "nGive me the number of elements: \nnGive me the number of combo: ")
  (let ((res (/ (fact (- (+ n r) 1))
                (* (fact r) (fact (- n 1))))))
    (message "Result: %.7f" res)
    res))

;;; Distribuzione

(defun dipergeom (n k ne)
  "Sia X la variabile aleatoria che conta il numero di successi su
n estrazioni senza reinserimento da una popolazione con N
elementi dei quali K sono considerati successo. Si dice allora
che X ha una distribuzione ipergeometrica di parametri N, K e NE"

  (interactive
   "nGive me N value: \nnGive me K value: \nnGive me n value: ")
  
  (let* ((ke (max 0 (- ne (- n k))))
	 (res (/ (* (combo-no-rip k ke)
		    (combo-no-rip (- n k) (- ne ke)))
		 (combo-no-rip n ne))))
    (message "Result: %.7f" res)
    res))

(defun dbinom (x size prob)
  "Calculate the binom distribution of X with SIZE and PROB.

Supponiamo di eseguire n prove bernoulliane indipendenti, ognuna
con probabilità di successo p. Sia X la variabile aleatoria che
conta il numero totale di successi ottenuti nelle n prove. Si
dice allora che X ha una distribuzione binomiale di parametri n e
p ∈ (0, 1)"
  (interactive
   "nGive me x value: \nnGive me the size: \nnGive me the probability: ")
  (let* ((bin (combo-no-rip size x))
	 (res (* bin
		 (expt (float prob) x) 
		 (expt (flaot (- 1 prob)) (- size x)))))
    (message "Result: %.7f" res)
    res))

(defun pbinom (q size prob)
  "Calc Q SIZE PROB."
  (interactive
   "nGive me x value: \nnGive me the size: \nnGive me the probability: ") 
  (if (< q 0)
      0
    (let ((res (+ (dbinom q size prob) 
		  (pbinom (- q 1) size prob))))
      (message "Result: %.8f" res)
      res)))

(defun dpois (x lamb)
  "Calc X LAMB."
  (interactive
   "nGive x: \nnGive lamb: ")
  (let ((res (* (/ (expt (float lamb) x)
		   (fact (float x)))
		(exp (- lamb)))))
    (message "Result: %.7f" res)
    res))

(defun ppois (q lamb)
  "Calc Q LAMB."
  (interactive
   "nGive x: \nnGive lamb: ")
  (if (< q 0)
      0
    (let ((res (+ (dpois q lamb) 
		  (ppois (- q 1) lamb))))
      (message "Result: %.7f" res)
      res)))

(defun neg-ppois (q lamb)
  "Calc Q LAMB."
  (interactive
   "nGive q: \nnGive lamb: ")
  (let ((res (- 1 (ppois q lamb))))
    (message "Result: %.7f" res)
    res))

(defun appros-ppois (x size prob)
  "Calc X SIZE PROB."
  (interactive
   "nGive me x value: \nnGive me the size: \nnGive me the probability: ")
  (if (< x 0)
      0
    (let* ((lamb (* size prob))
	   (res (+ (* (float (exp (- lamb)))
		      (/ (expt (float lamb) x)
			 (fact (float x))))
		   (appros-ppois (- x 1) size prob))))
      (message "Result: %.8f" res)
      res)))

(defun dgeom (x prob)
  "Distribuzione Geometrica X PROB."
  (interactive
   "nGive x: \nnGive prob: ")
  (let ((res (* (expt (float (- 1 prob)) (- x 1))
		prob)))
    (message "Result: %.8f" res)
    res))

(provide 'statistic-elisp)
;;; statistic-elisp.el ends here
