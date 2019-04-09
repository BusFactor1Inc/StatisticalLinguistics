(load (current-pathname "10000-english-words")) 
(load (current-pathname "latin-word-list")) 
(defconstant *word-list* +10000-english-words+)

(defconstant +words-as-strings+
  (mapcar #'symbol-name *word-list*))

(defconstant +words-as-lists-of-letters+
  (mapcar (lambda (x) (coerce x 'list))  +words-as-strings+))

(defconstant +used-characters+
  (sort (remove-duplicates
          (mapcan #'copy-list +words-as-lists-of-letters+))
        #'char<))

(defconstant +used-characters-table+
  (let ((i 1)
        (table (make-hash-table :test #'char=)))
    (mapcar (lambda (c)
              (setf (gethash c table) i)
              (incf i))
            +used-characters+)
    table))

(defstruct digraph
  order
  data)

(defstruct trigraph
  order
  data)

(defun split-into-digraphs (l)
  (let ((x (list (car l)))
        (i 0)
        retval)
    (dolist (y (cdr l))
      (push y x)
      (push (make-digraph
             :order i
             :data (reverse x))
            retval)
      (setf x (list (car x)))
      (incf i))
    retval))

(defun split-into-trigraphs (l)
  (ignore-errors
    (let ((x (nreverse (list (pop l) (pop l))))
          (i 0)
          retval)
      (dolist (y (cdr l))
        (push y x)
        (push (make-trigraph
               :order i
               :data (reverse x))
              retval)
        (setf x (list (first x) (second x)))
        (incf i))
    retval)))

(defconstant +digraphs+
  (mapcan (lambda (list-of-letters)
            (split-into-digraphs list-of-letters)) ;
          +words-as-lists-of-letters+))
  
(defconstant +trigraphs+
  (mapcan (lambda (list-of-letters)
            (split-into-trigraphs list-of-letters)) ;
          +words-as-lists-of-letters+))

(defun make-counts-table (source)
  (let ((table (make-hash-table :test #'equalp))
        (table-2 (make-hash-table :test #'equalp)))
    (dolist (x source)
      (if (gethash x table)
          (incf (gethash x table))
        (setf (gethash x table) 1)))
    (maphash (lambda (k v)
               (push k (gethash v table-2)))
             table)
    table-2))
  
(defconstant +digraph-counts-table+
  (make-counts-table +digraphs+))

(defconstant +trigraph-counts-table+
  (make-counts-table +trigraphs+))

(defun make-orders-table (source orders-selector)
  (let ((table (make-hash-table :test #'=)))
    (dolist (x source)
      (let ((order (funcall orders-selector x)))
        (push x (gethash order table))))
    table))

(defconstant +digraph-orders-table+
  (make-orders-table +digraphs+ #'digraph-order))
  
(defconstant +trigraph-orders-table+
  (make-orders-table +trigraphs+ #'trigraph-order))

(defun table-to-alist (table)
  (let (retval)
    (maphash (lambda (k v)
               (push (cons k v) retval))
             table)
    retval))

(defconstant +digraph-counts-alist+
  (let ((retval (table-to-alist +digraph-counts-table+)))
    (sort retval #'< :key #'car)))

(defconstant +digraph-orders-alist+
  (let ((retval (table-to-alist +digraph-orders-table+)))
    (sort retval #'< :key #'car)))

(defconstant +starts-with-digraphs-table+
  (let ((table (make-hash-table :test #'char=)))
    (dolist (x +digraphs+)
      (push (cdr (digraph-data x)) (gethash (car (digraph-data x)) table)))
    table))

(defconstant +starts-with-trigraphs-table+
  (let ((table (make-hash-table :test #'char=)))
    (dolist (x +digraphs+)
      (push (cdr (digraph-data x)) (gethash (car (digraph-data x)) table)))
    table))

(defconstant +starts-with-trigraphs-alist+
  (let ((retval (table-to-alist +starts-with-trigraphs-table+)))
    (sort retval #'char< :key #'car)))

(defun pick (l)
  (nth (random (length l)) l))

(defun random-digraph ()
  (digraph-data (pick (cdr (assoc 0 +digraph-orders-alist+)))))

(defun next-digraph (starts-with-character)
  (let ((digraphs (gethash starts-with-character
                           +starts-with-digraphs-table+)))
    (cons starts-with-character (pick digraphs))))


(defun next-trigraph (starts-with-character)
  (let ((trigraphs (gethash starts-with-character
                           +starts-with-trigraphs-table+)))
    (cons starts-with-character (pick trigraphs))))


(defun gemantria (character-list character-table)
  (let ((sum 0))
    (dolist (c character-list)
      (incf sum (gethash c character-table)))
    (list (coerce character-list 'string) sum)))
  
(defconstant +english-gemantria-table+
  (let ((table (make-hash-table :test #'=)))
    (map nil
         (lambda (gemantria)
           (push (car gemantria) (gethash (second gemantria) table)))
         (remove-if #'null
                    (mapcar
                     (lambda (x)
                       (when x
                         (gemantria (coerce (symbol-name x) 'list)
                                    +used-characters-table+)))
                     +10000-english-words+)))
         table))

(defun english-gemantria (word)
  (let ((gemantria (gemantria word +used-characters-table+)))
    (list (car gemantria)
          (let ((alters (gethash (second gemantria)
                                 +english-gemantria-table+)))
            (list (pick alters) (pick alters) (pick alters))))))


(defun random-english-word (&optional (length (+ 3 (random 10))))
  (let ((word (random-digraph)))
    (dotimes (i length)
      (setf word (append word
                         (cdr
                          (next-trigraph (car (last word)))))))
    (english-gemantria word)))

(defun random-english-word-2 (&optional (length (+ 3 (random 7))))
  (let ((word (random-digraph)))
    (dotimes (i length)
      (if (< 0.5 (random 1.0))
          (setf word (append word
                             (cdr
                              (next-digraph (car (last word))))))
        (setf word (append word
                           (cdr
                            (next-trigraph (car (last word))))))))
    (english-gemantria word)))
      

