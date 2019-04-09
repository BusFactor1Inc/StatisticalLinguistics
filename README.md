Statistical Linguistics
==

A set of functions and data sets for working with words starting from
a word list using a combination of digraphs and trigraph statistics
and features.

Usage:
--

```cl
CL-USER 1> (load "main.lisp")
```

Currently generates random 'English-based' words, with Gemantric
definitions:


```cl
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
```

It can also generate a new word with the same Gemantric definition:

```cl
CL-USER 3> (word-for-definition "DANGER")
"AREDACHI"

CL-USER 9> (word-for-definition "LOVE")
"DUTI"

CL-USER 10> (word-for-definition "LOVE")
"WHIN"
````

"Just for fun."	

--

Burton Samograd

2019
