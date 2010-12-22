(defun sentence ()    (append (noun-phrase) (verb-phrase)))
(defun noun-phrase () (append (Article) (Noun)))
(defun verb-phrase () (append (Verb) (noun-phrase)))
(defun Article ()     (one-of '(the a)))
(defun Noun ()        (one-of '(man ball woman table)))
(defun Verb ()        (one-of '(hit took saw liked)))

(defun Adj* ()
  (if (= (random 2) 0)
      nil
    (append (Adj) (Adj*))))

(defun PP* ()
  (if (random-elt '(t nil))
      (append (PP) (PP*))
    nil))

(defun noun-phrase () (append (Article) (Adj*) (Noun) (PP*)))
(defun PP ()          (append (Prep) (noun-phrase)))
(defun Adj ()         (one-of '(big little blue green adiabatic)))
(defun Prep ()        (one-of '(to in by with on))) 

(defun one-of (set)
  "Pick one element of set, and make a list of it"
  (list (random-elt set)))

(defun random-elt (choices)
  "Choose an element from a list at random."
  (elt choices (random (length choices))))