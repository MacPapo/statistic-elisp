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
  (let* ((f-n (fact n))
	 (f-r (fact  r))
	 (f-n-r (fact (- n r)))
	 (res (/ (float f-n)
		 (float (* f-r
			   f-n-r)))))
    ;; (message "FN: %.7f" f-n)
    ;; (message "FR: %.7f" f-r)
    ;; (message "FNR: %.7f" f-n-r)
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

;;;;;;;;;;;;;;; IPERGEOMETRICA ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun distribuzione-ipergeometrica (n k ke ne)
  "estrazione senza reinserimento di palline che possono assumere
solo 2 valori (0 o 1)

Sia X la variabile aleatoria che conta il numero di successi su
n estrazioni senza reinserimento da una popolazione con N
elementi dei quali K sono considerati successo. Si dice allora
che X ha una distribuzione ipergeometrica di parametri N, K e NE"

  (interactive
   "nDammi il numero di elementi: \nnDammi il numero di casi positivi: \nnQuanti casi positivi vuoi estrarre?: \nnDammi il numero di estrazioni: ")
  
  (let* ((res (/ (* (combo-no-rip k ke)
		    (combo-no-rip (- n k) (- ne ke)))
		 (combo-no-rip n ne))))
    (message "Result: %.7f" res)
    res))

(defun valore-atteso-ipergeometrica (ne k n)
  "VALORI: ne -> numero di estrazioni
k-> numero di casi positivi
n-> numero di elementi"

  (interactive
   "nDammi totale estrazioni: \nnNumero casi positivi: \nnDammi il numero di elementi: ")
  (let ((res (/ (float (* ne k))
		n)))
    (message "Result: %.7f" res)
    res))

(defun varianza-ipergeometrica (ne k n)
  "VALORI: ne -> numero di estrazioni
k-> numero di casi positivi
n-> numero di elementi"

  (interactive
   "nDammi totale estrazioni: \nnNumero casi positivi: \nnDammi il numero di elementi: ")
  (let ((res (/ (float (* (* ne (- n ne))
			  (* k  (- n  k))))
		(* (float (expt n 2))
		   (- n 1)))))
    (message "Result: %.7f" res)
    res))

(defun uno-meno-distribuzione-ipergeometrica(n k ke ne)
  "ALMENO"
  
  (interactive
   "nDammi il numero di elementi: \nnDammi il numero di casi positivi: \nnQuanti casi positivi devono essere estratti almeno: \nnDammi il numero di estrazioni: ")
  (let ((res (- 1 (distribuzione-ipergeometrica n k (- 1 ke) ne))))
    (message "Result: %.7f" res)
    res))

(defun ripartizione-ipergeometrica (n k ke ne)
  "non piu'"
  
  (interactive
   "nDammi il numero di elementi: \nnDammi il numero di casi positivi: \nnQuanti casi positivi devono essere estratti al massimo?: \nnDammi il numero di estrazioni: ")
  (if (< ke 0)
      0
    (let ((res (+ (distribuzione-ipergeometrica n k ke ne)
		  (ripartizione-ipergeometrica n k (- ke 1) ne))))
      (message "Result: %.7f" res)
      res)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;; BINOMIALE ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun distribuzione-binomiale (n k prob)
  "estrazione con reinserimento di palline che possono assumere
solo 2 valori (0 o 1)

Calculate the binom distribution of X with SIZE and PROB.

Supponiamo di eseguire n prove bernoulliane indipendenti, ognuna
con probabilità di successo p. Sia X la variabile aleatoria che
conta il numero totale di successi ottenuti nelle n prove. Si
dice allora che X ha una distribuzione binomiale di parametri n e
p ∈ (0, 1)"
  (interactive
   "nDammi il numero di eventi: \nnDammi il numero di successi: \nnDammi la probabilita: ")
  (let* ((bin (combo-no-rip n k))
	 (res (* bin
		 (expt (float prob) k) 
		 (expt (float (- 1 prob)) (- n k)))))
    (message "Result: %.7f" res)
    res))

(defun ripartizione-binomiale (n k prob)
  "estrazione con reinserimento di palline che possono assumere 
solo due valore ( 0 o 1)

Supponiamo di avere n prove bernuolliane indipendenti tra loro e
 ognuna con una probabilità di successo di p.
Sia X la v,a, che descrive il numero totale di successi su n prove, 
la funzione di ripartizione ( n k prob ) calcola la probabilità che al
 più k elementi su n campionati abbiano avuto successo"
  (interactive
   "nDammi il numero di eventi: \nnDammi il numero di successi: \nnDammi la probabilità: ")
  (if (< k 0)
      0
    (let ((res (+ (distribuzione-binomiale n k prob) 
		  (ripartizione-binomiale n (- k 1) prob))))
      (message "Result: %.8f" res)
      res)))

(defun uno-meno-ripartizione-binomiale (n k prob)
  "Estrazione con reinserimento di palline che possono assumere 
solo due valore ( 0 o 1)

Supponiamo di avere n prove bernuolliane indipendenti tra loro e
 ognuna con una probabilità di successo di p.
Sia X la v,a, che descrive il numero totale di successi su n prove, 
la funzione di uno-meno-ripartizione-ripartizione ( n k prob ) 
calcola la probabilità che almeno k elementi su n campionati 
abbiano avuto successo"
  (interactive
   "nDammi il numero di eventi: \nnDammi il numero di successi: \nnDammi la probabilità: ")
  (let ((res (- 1 (ripartizione-binomiale n (- k 1) prob))))
    (message "Result: %.7f" res)
    res))

(defun approssimazione-distribuzione-binomiale-poisson (n k prob)
  "Quando il numero di eventi tende and infinito e la probabilità di 
di ognuno di essi tende a zero, si può approssimare la suddetta 
distribuzione Binomiale ad una distribuzione di Poisson.

RICEVE: n -> eventi favorevoli
k -> eventi totali
prob -> probabilità"
  (interactive
   "nDammi il numero di eventi favorevoli: \nnDammi il totale di eventi: \nnDammi la probabilità: ")
  (let* ((lamb (* (float k) prob))
	 (res (distribuzione-poisson n lamb)))
    (message "Result: %.7f" res)
    res))

(defun approssimazione-ripartizione-binomiale-poisson (n k prob)
  "Quando il numero di eventi tende and infinito e la probabilità di 
di ognuno di essi tende a zero, si può approssimare la suddetta 
distribuzione Binomiale ad una distribuzione di Poisson.

RICEVE: n -> eventi favorevoli
k -> eventi totali
prob -> probabilità"
  (interactive
   "nDammi il numero di eventi favorevoli: \nnDammi il totale di eventi: \nnDammi la probabilità: ")
  (let* ((lamb (* (float k) prob))
	 (res (ripartizione-poisson n lamb)))
    (message "Result: %.7f" res)
    res))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;; POISSON ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun distribuzione-poisson (k lamb)
  "Questa distribuzione viene usata per esprimere la probabilità che 
uno specifico numero di eventi accada in un dato intervallo di tempo, 
sapendo che, mediamente, se ne verifica un numero LAMB.

RICEVE: K -> numero di eventi per intervallo di tempo
LAMB -> numero medio di eventi per intervallo di tempo"
  (interactive
   "nDammi il numero di eventi per intervallo di tempo: \nnDammi il numero medio di eventi per intervallo di tempo: ")
  (let ((res (* (/ (expt (float lamb) k)
		   (fact (float k)))
		(exp (- lamb)))))
    (message "Result: %.7f" res)
    res))

(defun ripartizione-poisson (k lamb)
  "Calc K -> numero di eventi per intervallo di tempo
LAMB -> numero medio di eventi per intervallo di tempo"
  (interactive
   "nDammi il numero di eventi per intervallo di tempo: \nnDammi il numero medio di eventi per intervallo di tempo: ")
  (if (< k 0)
      0
    (let ((res (+ (distribuzione-poisson k lamb) 
		  (ripartizione-poisson (- k 1) lamb))))
      (message "Result: %.7f" res)
      res)))

(defun uno-meno-ripartizione-poisson (k lamb)
  "Data una v.a. X che segue l'andamento della distribuzione di 
Poission, la funzione calcola la probabilità che il numero di eventi 
favorevoli sia almeno di k.

RICEVE: k -> numero di eventi favorevoli
LAMB -> numero medio di eventi per intervallo di tempo"
  
  (interactive
   "nDammi il numero di eventi per intervallo di tempo: \nnDammi il numero medio di eventi per intervallo di tempo: ")
  (let ((res (- 1 (ripartizione-poisson (- k 1) lamb))))
    (message "Result: %.7f" res)
    res))

(defun ripartizione-poisson-estremi-inclusi (n k lamb)
  "Data una v.a. X che segue l'andamento della distribuzione di 
Poission, la funzione calcola la probabilità che il numero di eventi 
favorevoli sia compreso tra gli estremi anch'essi considerati.

RICEVE: n -> estremo superiore
k -> estremo inferiore
LAMB -> numero medio di eventi per intervallo di tempo"
  (interactive
   "nDammi l'estremo superiore: \nnDammi l'estremo inferiore: \nnDammi la lambda: ")
  (let ((res (- (ripartizione-poisson n lamb)
		(ripartizione-poisson (- k 1) lamb))))
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
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;; GEOMETRICA ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun distribuzione-geometrica (x prob)
  "La distribuzione geometrica viene usata per esprimere il numero di 
eventi x, ognuno con probabilità p, che devo analizzare prima di 
avere un successo"
  
  (interactive
   "nDammi il numero di eventi: \nnDammi la probabilità: ")
  (let ((res (* (expt (float (- 1 prob)) (- x 1))
		prob)))
    (message "Result: %.8f" res)
    res))

(defun ip--distribuzione-geometrica (x prob)
  "RISCRIVIMI"
  (let ((res (* (expt (float (- 1 prob)) x)
		prob)))
    res))

(defun ip--ripartizione-geometrica (x prob)
  "RISCRIVIMI"
  (message "VALORE X: %f" x)
  (if (< x 0)
      0
    (let ((res (+ (ip--distribuzione-geometrica x prob)
		  (ip--ripartizione-geometrica (- x 1) prob))))
      res)))

(defun ripartizione-geometrica (x prob)
  "questa funzione mi fornisce la probabilità che io debba aspettare 
al più x eventi ,con probabilità p, prima di avere un successo"
  (interactive
   "nDammi il numero di eventi: \nnDammi la probabilità: ")
  (let ((res (ip--ripartizione-geometrica (- x 1) prob)))
    (message "Result: %.7f" res)
    res))

(defun uno-meno-ripartizione-geometrica (x prob)
  "questa funzione mi fornisce la probabilità che io debba aspettare 
almeno x eventi ,con probabilità p, prima di avere un successo"
  (interactive
   "nDammi il numero di eventi: \nnDammi la probabilità: ")
  (let ((res (- 1 (ripartizione-geometrica (- x 1) prob))))
    (message "Result: %.7f" res)
    res))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide 'statistic-elisp)
;;; statistic-elisp.el ends here
