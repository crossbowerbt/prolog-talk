;(asdf:oos 'asdf:load-op :gambol)
;(in-package :gambol)

(defun random-element (list)
  "Return some element of the list, chosen at random."
  (nth (random (length list)) list))

(defun pl-update-single (fact value)
  "Update a prolog fact (that accept a single variable/argument)."
  (pl-retract (list (list fact '?_)))
  (pl-assert  (list (list fact value)))
  t)

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

(defun generate-partitions (list &optional (len 0))
  "Generate all the possible partitions for an ordered list."
  (cond ((null list) nil)
        ((= len 0) (generate-partitions list (length list)))
        ((= len 1) list)
        (t (loop for i from 1 to (- len 1)
              collect (subseq list 0 i)))))


(defun pl-repl ()
  "A simple repl for the system."
  (format t "~&> ")
  (let ((input (read-from-string (concatenate 'string "(" (read-line) ")"))))
    (when (not (equal input '(quit)))
      (let* ((queries (mapcar #'partition-to-pl-vars
                              (list-partitions input)))
             (answers (mapcan (lambda (query)
                                (pl-solve-one (list (append '(category) query '(?answer)))))
                              queries)))
        (mapc (lambda (answer)
                (format t "~a" (cdr answer)))
              answers)
        (pl-repl)))))

; Experimental

; TODO: metasymbolp

; TODO: list-last
; TODO: list-head

; TODO: nounp
; TODO: adjectivep

; TODO: crete-new-context-noun
; TODO: get-context-noun

; indeterminative articles
(*- (parser a ?noun ?sym) (lop (nounp ?noun))
    (= ?sym (lop (create-new-context-noun ?noun))))

(*- (parser an ?noun ?sym) (lop (nounp ?noun))
    (= ?sym (lop (create-new-context-noun ?noun))))

; determinative article
(*- (parser the ?noun ?sym) (lop (nounp ?noun))
    (= ?sym (lop (get-context-noun ?noun))))

; adjective list (create temporary noun)
(*- (parser ?pre-list ?adj ?noun ?sym) (lop (nounp ?noun)) (lop (adjectivep ?adj))
    (= ?sym (create-new-context-noun ?noun))) ; TODO: maybe check that ?sym is not alredy defined...

; adjective list (add adjective to temporary noun)
(*- (parser ?pre-list ?adj ?noun ?sym) (lop (nounp ?noun)) (lop (adjectivep ?adj))
    (lisp (add-noun-adjective ?noun ?adj))
    (= ?head (lop (list-head ?pre-list))) (= ?last (lop (list-last ?pre-list)))
    (parser ?head ?last ?noun ?sym))

; adjective list (unify temporary noun with context noun, when an article is found)
(*- (parser ?pre-list the ?noun ?sym) (lop (nounp ?noun))
    (= ?real-sym (lop (get-context-noun ?noun))) (lisp (unify-temporary-noun ?sym ?real-sym)))

; adjective list (unify temporary noun with context noun, when an article is found)
; alredy unified since is a new noun
(*- (parser ?pre-list a ?noun ?sym) (lop (nounp ?noun)))
(*- (parser ?pre-list an ?noun ?sym) (lop (nounp ?noun)))

; "to be" verb
(*- (parser ?pre-list is ?post-list)
    (parser ?pre-list ?subject) (parser ?post-list ?object)
    (asserta (is ?subject ?object)))
