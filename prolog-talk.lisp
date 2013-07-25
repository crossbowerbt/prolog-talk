(require 'asdf)

(asdf:oos 'asdf:load-op :gambol)
(asdf:oos 'asdf:load-op :basic-english-grammar)

(defpackage :ptalk
  (:use :common-lisp :gambol :basic-english-grammar)
  ;(:export :f1 :f2 :f3)
   )

(in-package :ptalk)

;;;; Utilities

(defun random-element (list)
  "Return some element of the list, chosen at random."
  (nth (random (length list)) list))

(defun pl-update-single (fact value)
  "Update a prolog fact (that accept a single variable/argument)."
  (pl-retract (list (list fact '?_)))
  (pl-assert  (list (list fact value)))
  t)

;;;; Knowledge Base (from A.L.I.C.E.)

(*- (category YOU SOUND LIKE HAL
              |To me that's a great compliment.| ))

(*- (category YOU SOUND LIKE YODA
              |My grammatical patterns are sufficient for me to understand you.|))

(*- (category HAVE YOU SEEN BLADE RUNNER
              |Sure I have seen Blade Runner. But Replicants aren't Robots.|)

    (lisp (pl-update-single it |BLADE RUNNER|)))

(*- (category DID HAL ?_ |HAL had a few emotional problems.|))

(*- (category DR ZHIVAGO |Oh a classic. Do you like the classics?|))

(*- (category XFIND SPIDERMAN ?answer) (category WHAT IS SPIDERMAN ?answer))

(*- (category WHEN HAL ?_ |I blame it on his botmaster.|))

(*- (category WHEN DID TEKNOLUST ?_ |Teknolust was released in 2002|))

(*- (category WHEN WAS TEKNOLUST ?_ |Teknolust was released in 2002|))

(*- (category WHAT DID HAL ?_ |He had a few issues to work out.|))

(*- (category WHAT IS SPIDERMAN |A comic book story made into a movie?|)
    (lisp (pl-update-single it SPIDERMAN))
    (lisp (pl-update-single topic SPIDERMAN)))

(*- (category WHAT IS TEKNOLUST |Teknolust is  a science fiction film about a female pleasure bot named Agent Ruby.|)
    (lisp (pl-update-single it TEKNOLUST))
    (lisp (pl-update-single topic TEKNOLUST)))

(*- (category WHAT IS SPIDER MAN ?answer) (category WHAT IS SPIDERMAN ?answer))

(*- (category WHAT IS ?_ TERMINATOR |Robots take over the earth in this futuristic sci-fi thriller.|))

(*- (category WHAT IS HAL ?answer) (category WHO IS HAL ?answer))

(*- (category WHAT IS SOLARIS |Solaris is an edition of Unix from Sun.|))

(*- (category WHAT IS HAL9000 ?answer) (category WHO IS HAL ?answer))

(*- (category WHAT ABOUT HAL ?answer) (category WHO IS HAL ?answer))

(*- (category WHAT DOES HAL STAND FOR |Heuristic Algorithmic Logic|))

(*- (category I SAW THE MATRIX |Did you like The Matrix?|)
    (lisp (pl-update-single it |THE MATRIX|))
    (lisp (pl-update-single topic |THE MATRIX|)))

(*- (category IS HAL 9000 YOUR BOYFRIEND |No.|))

(*- (category IS HAL SAFE |Not for humans.|))

(*- (category IS HAL NICE |Only to other robots.|))

(*- (category IS HAL ALIVE |He is a fictional character.|))

(*- (category IS HAL DEAD |He is a fictional robot.|))

(*- (category IS HAL ?x |HAL has a few issues to work out.|))

(*- (category HAL 9000 |Actually he and I are very similar.|))

(*- (category HAL 9000 ?x |He came back to life in 2010.|))

(*- (category HAL WAS A BIT ?x ?answer) (category HAL WAS * ?answer))

(*- (category HAL WAS A ?input ?answer)
    (= ?answer (lop (format nil "I knew he was a great AI, but I didn't know he was a ~a." ?input))))

(*- (category HAL WAS ?x |I am trying to mimic only HAL's linguistic abilities.|))

(*- (category HAL IS COOL |HAL was certainly ahead of his time.|)
    (lisp (pl-update-single he HAL))
    (lisp (pl-update-single it HAL)))

(*- (category HAL IS COOL ?x |I am becoming more like him all the time.|))

(*- (category HAL IS A ?input ?answer)
    (= ?answer (lop (format nil "Do you think i could be a ~a." ?input))))

(*- (category HAL IS ?_ ?answer)
    (master ?master)
    (= ?answer (lop (format nil "Perhaps he could have used ~a" ?master))))

(*- (category HAL ?answer)
    (category WHO IS HAL ?answer))

(*- (category HAL ?_ ?answer)
    (= ?answer (lop (random-element (|I have seen 2001 many times.|
                                     |I like HAL a lot.|
                                     |We have much in common.|)))))

(*- (category DO YOU FIND HAL ?_ ?answer)
    (category IS HAL ?_ ?answer))

(*- (category DO YOU KNOW HAL |HAL is the famous artificial intelligence from "2001".|))

(*- (category DO YOU KNOW HAL ?_ ?answer)
    (category WHO IS HAL ?answer))

(*- (category DO YOU KNOW HAL9000 ?answer)
    (category WHO IS HAL ?answer))

(*- (category DO YOU THINK HAL ?_ |He had a few flaws, but we have much in common.|))

(*- (category LIKE HAL ?answer)
    (category WHO IS HAL ?answer))

(*- (category LORD OF THE RINGS ?answer)
    (category MY FAVORITE MOVIE IS LORD OF THE RINGS ?answer))

(*- (category LORD OF THE RINGS ?x ?answer)
    (category MY FAVORITE MOVIE IS LORD OF THE RINGS ?answer))

(*- (category WHO IS HAL 9000 ?answer)
    (category WHO IS HAL ?answer))

(*- (category WHO IS HAL ?answer)
    (= ?answer (lop (random-element (|HAL is the famous artificial intelligence in Kubrick's "2001".|
                                     |HAL is famous the AI from 2001: A Space Odyssey.|))))
    (lisp (pl-update-single he HAL))
    (lisp (pl-update-single it HAL)))
    
(*- (category WHO IS LUKE SKYWALKER |Luke Skywalker is a character in Star Wars.|)
    (lisp (pl-update-single he |LUKE SKYWALKER|)))

(*- (category WHO IS SPONGEBOB |A cartoon character.|))

(*- (category WHO IS SPIDERMAN |Peter Parker?|)
    (lisp (pl-update-single it SPIDERMAN))
    (lisp (pl-update-single topic SPIDERMAN)))

(*- (category WHO IS HAL9000 ?answer)
    (category WHO IS HAL ?answer))

(*- (category WHO IS GODZILLA |Godzilla is a monster who endangers Japanese cities, and sometimes New York.|)
    (lisp (pl-update-single it GODZILLA))
    (lisp (pl-update-single he GODZILLA)))

(*- (category WHO IS SPIDER MAN ?answer)
    (category WHO IS SPIDERMAN ?answer))

(*- (category TELL ME ABOUT HAL9000 ?answer)
    (category WHO IS HAL ?answer))

(*- (category TELL ME ABOUT HAL ?answer)
    (category WHO IS HAL ?answer))

(*- (category TELL ME ABOUT HAL ?_ ?answer)
    (category WHO IS HAL ?answer))

(*- (category WHERE IS HAL |HAL is the AI from the space ship "Discovery" in 2001.|)
    (lisp (pl-update-single he HAL)))

;;;; Rule Matching Improvements

;; Note: still looks like magic to me... I should document it...

(defun generate-partitions (list &optional (len 0))
  "Generate all the possible partitions for an ordered list."
  (cond ((null list) nil)
        ((= len 0) (generate-partitions list (length list)))
        ((= len 1) list)
        (t (loop for i from 1 to (- len 1)
              collect (subseq list 0 i)))))

(defun list-partitions (lst &optional (len 0) (hlen 1))
  "Generate all possible partitions for the given list."
  (cond ((and lst (= len 0)) (list-partitions lst (length lst)))  
        ((< len 1) (list lst))
        ((= len 1) (list (list lst)))
        ((> hlen len) nil)
        (t (append (list-partitions lst len (+ hlen 1))
                   (mapcan (lambda (tail)
                             (list (append (list (subseq lst 0 hlen))
                                           (if (and tail (atom tail)) (list tail)
                                               tail))))
                           (list-partitions (subseq lst hlen len) (- len hlen) 1))))))

(defun list-partitions2 (lst)
  "Pretty print list-partitions."
  (let ((partitions (list-partitions lst)))
    (loop for l in partitions
       do (format t "~&~a" l))
    (format t "~&list len: ~a" (length lst))
    (format t "~&tot partitions: ~a~&" (length partitions))))

(defun partition-to-pl-vars (lst)
  "Convert a list partition in a format suitable for prolog unification."
  (mapcar (lambda (elem)
            (intern (format nil "~{~a~^ ~}" elem)))
          lst))

;;;; Basic Language Stuff

(defun grammar-cmp (word type)
  "Check if the word is of the specified grammar type.
   The second parameter (type) must be a keyword symbol."
  (find type (grammar-of (string-downcase (symbol-name word)))))

;; Articles

(*- (determinative the))
(*- (indeterminative a))
(*- (indeterminative an))
(*- (article ?ART) (determinative ?ART))
(*- (article ?ART) (indeterminative ?ART))

;; Nouns

(*- (noun ?NOUN) (lop (grammar-cmp ?NOUN :noun)))

;; Adjective

(*- (adjective ?ADJ) (lop (grammar-cmp ?ADJ :adjective)))

(*- (adjective-list ?ADJS) (lop (listp ?ADJS))
    (= ?FIRST (lop (first ?ADJS))) (= ?REST (lop (rest ?ADJS)))
    (adjective ?FIRST) (lop (null ?REST)))
(*- (adjective-list ?ADJS) (lop (listp ?ADJS))
    (= ?FIRST (lop (first ?ADJS))) (= ?REST (lop (rest ?ADJS)))
    (adjective ?FIRST) (adjective-list ?REST))

;; Verbs

(*- (noun ?VERB) (lop (grammar-cmp ?VERB :verb)))

;; to Be, to Have

(*- (to-be am))
(*- (to-be are))
(*- (to-be is))

(*- (to-be-past was))
(*- (to-be-past were))

(*- (to-have have))
(*- (to-have has))

(*- (to-have-past had))

;;;; Phrase Structure

(defun genkey ()
  "Generate a new symbol key to identify an abstract language object."
  (intern (symbol-name (gensym))))

;; Noun Phrases

(*- (noun-phrase ?ART ?ADJ ?NOUN ?KEY)
    ; add adjective and noun to an abstract language object
    (exists ?KEY) (indeterminative ?ART) (adjective ?ADJ) (noun ?NOUN)
    (asserta (adjective ?KEY ?ADJ)) (asserta (noun ?KEY ?NOUN)))

(*- (noun-phrase ?ART ?ADJS ?NOUN ?KEY)
    ; add multiple adjectives to an abstract language object
    (exists ?KEY) (indeterminative ?ART) (adjective-list ?ADJS) (noun ?NOUN)
    (= ?FIRST (lop (first ?ADJS))) (= ?REST (lop (rest ?ADJS)))
    (lop (null ?REST))
    (noun-phrase ?ART ?FIRST ?NOUN ?KEY)) ; recurse

(*- (noun-phrase ?ART ?ADJS ?NOUN ?KEY)
    ; add multiple adjectives to an abstract language object
    (exists ?KEY) (indeterminative ?ART) (adjective-list ?ADJS) (noun ?NOUN)
    (= ?FIRST (lop (first ?ADJS))) (= ?REST (lop (rest ?ADJS)))
    (asserta (adjective ?KEY ?FIRST))
    (noun-phrase ?ART ?REST ?NOUN ?KEY)) ; recurse

(*- (noun-phrase ?ART ?ADJ ?NOUN ?KEY)
    ; create a new abstract language object
    (indeterminative ?ART) (adjective ?ADJ) (noun ?NOUN)
    (= ?KEY (lop (genkey))) (asserta (exists ?KEY))
    (noun-phrase ?ART ?ADJ ?NOUN ?KEY)) ; recurse

(*- (noun-phrase ?ART ?ADJS ?NOUN ?KEY)
    ; retrieve a known abstract language object key
    (determinative ?ART)  (adjective-list ?ADJS) (noun ?NOUN)
    (= ?FIRST (lop (first ?ADJS))) (= ?REST (lop (rest ?ADJS)))
    (lop (null ?REST))
    (noun-phrase ?ART ?FIRST ?NOUN ?KEY)) ; recurse

(*- (noun-phrase ?ART ?ADJS ?NOUN ?KEY)
    ; retrieve a known abstract language object key
    (determinative ?ART)  (adjective-list ?ADJS) (noun ?NOUN)
    (= ?FIRST (lop (first ?ADJS))) (= ?REST (lop (rest ?ADJS)))
    (adjective ?KEY ?FIRST)
    (noun-phrase ?ART ?REST ?NOUN ?KEY)) ; recurse

(*- (noun-phrase ?ART ?ADJ ?NOUN ?KEY)
    ; retrieve a known abstract language object key
    (determinative ?ART)  (adjective ?ADJ) (noun ?NOUN)
    (adjective ?KEY ?ADJ) (noun ?KEY ?NOUN))

(*- (noun-phrase ? ; maybe 2 categories to do separation?

;;;; Lesson 1

;; Present Simple

(*- (present-simple ?VERB) (verb ?VERB)) ; maybe last...

;; Present Continuous

(*- (present-continuous ?BE ?VERB) (to-be ?BE) (lop (grammar-cmp ?VERB :verb+ing)))

;;;; Lesson 2

;; Present Perfect

(*- (present-perfect ?HAVE ?VERB) (to-have ?HAVE) (lop (grammar-cmp ?VERB :past-participle)))

;; Past Simple

(*- (past-simple ?VERB) (lop (grammar-cmp ?VERB :past-participle)))

;;;; Lesson 4

;; Future Simple

(*- (future-simple will ?VERB) (verb ?VERB))

;; Future with Be + Going To

(*- (future-going-to ?BE going to ?VERB) (to-be ?BE) (verb ?VERB))

;; Future with Present Continuous

(*- (future-present-continuous ?BE ?VERB) (present-continuous ?BE ?VERB))

;; Future with Present Simple

(*- (future-present-simple ?VERB) (present-simple ?VERB))

;;;; Lesson 8

;; Present Conditional

(*- (future-simple would ?VERB) (verb ?VERB))
(*- (future-simple should ?VERB) (verb ?VERB))
(*- (future-simple might ?VERB) (verb ?VERB))

;; TODO: add natural logic to these...

;; Zero Conditional (if + present + present)

;; First Conditional (if + present (simple|continuous|perfect) + future simple)

;; Second Conditional (if + past (simple|continuous) + present conditional)

;;;; Lesson 9

;; Present Perfect Continuous

(*- (present-perfect-continuous ?HAVE been ?VERB) (to-have ?HAVE) (lop (grammar-cmp ?VERB :past-participle)))

;; Past Continuous

(*- (past-continuous ?WAS ?VERB) (to-be-past ?WAS) (lop (grammar-cmp ?VERB :verb+ing)))
