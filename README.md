Statistical Linguistics
==

A set of functions and data sets for working with words starting from
a word list.

Currently generates random words, with Gemantric definitions:

```cl
CL-USER 1> (load "main.lisp")
CL-USER 2>  (dotimes (i 10) (print (random-english-word)))

("SPENGSHAT" ("SENSORS" "REPRODUCED" "MODERATOR")) 
("GUTEXISTARA" ("MONTGOMERY" "ACCOMMODATIONS" "ACCOMMODATIONS")) 
("REMERI" ("HOUSE" "BOATING" "BECOMING")) 
("ROGEST" ("MOTELS" "DELIVERED" "DESCRIBES")) 
("UPASTINGENEDI" ("EXTENSIONS" "EXPRESSION" "UNNECESSARY")) 
("CONGHOUNG" ("PARENTING" "WRAPPING" "PREPARING")) 
("SOLECTEXPLDYLC" ("CONSCIOUSNESS" "CONSCIOUSNESS" "CONSISTENTLY")) 
("ADIEDE" ("READ" "LP" "CET")) 
("NUTUDA" ("STORED" "HEALTHCARE" "SCANNING")) 
("STADIOUGHE" ("THRESHOLD" "ENQUIRY" "DEVELOPING")) 
NIL
````

--

Burton Samograd
2019
