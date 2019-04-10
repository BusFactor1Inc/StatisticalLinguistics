(eval-when (compile load)
  (load "~/quicklisp/setup.lisp")
  (ql:quickload :hunchentoot)
  (ql:quickload :cl-who))

(use-package :cl-who)

(load (current-pathname "1000-english-words")) 
(load (current-pathname "10000-english-words")) 
(load (current-pathname "latin-word-list")) 
(defparameter *word-list* +10000-english-words+)

(setf *word-list* (remove-if (lambda (x)
                               (< (length (symbol-name x)) 3))
                             *word-list*))

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
            alters))))


(defun random-english-word (&optional (length (+ 1 (random 7))))
  (let ((word (random-digraph)))
    (dotimes (i length)
      (setf word (append word
                         (cdr
                          (next-trigraph (car (last word)))))))
    (english-gemantria word)))

(defun random-english-word-2 (&optional (length (+ 1 (random 7))))
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
      


(defun word-for-definition (definition)
  (destructuring-bind (word value)
      (gemantria (coerce definition 'list)
                 +used-characters-table+)
    (loop 
     (let ((data (gemantria (coerce
                             (first (random-english-word
                                     (1+ (random (length definition)))))
                                    'list)
                            +used-characters-table+)))
       (when (= value (second data))
         (return-from word-for-definition (first data)))))))
         

(defparameter *server* nil)

(hunchentoot:define-easy-handler
 (root :uri "/"
       :default-request-type :get) ()
  (let ((word (random-english-word))
        (word-2 (random-english-word-2)))
    (let ((html
           (with-html-output-to-string
               (*standard-output* nil :prologue t)
             (:html
              (:head (:title (str "Magic Word Generator"))
              (:body
               (:h1 (str "Magic Word Generator."))
               (:h1 (str (concatenate 'string
                                      (car word)
                                      "&nbsp;(" (car word-2) ")")))
               (:h2 "Means: ")
               (:ol
                (:li (str (concatenate 'string
                                       (pick (cadr word)) ",&nbsp;"
                                       (pick (cadr word-2)))))
                (:li (str (concatenate 'string
                                       (pick (cadr word)) ",&nbsp;"
                                       (pick (cadr word-2)))))
                (:li (str (concatenate 'string
                                       (pick (cadr word)) ",&nbsp;"
                                       (pick (cadr word-2))))))))))))
      (with-open-file (s (current-pathname "data/log.html")
                         :direction :output
                         :if-exists :append
                         :if-does-not-exist :create)
        (print html s))
      html)))


(setf hunchentoot:*show-lisp-backtraces-p* t)

(defun main ()
  (setf *server* (hunchentoot:start
                  (make-instance 'hunchentoot:easy-acceptor :port 4242))))