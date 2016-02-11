#lang racket
(require db)
      
(define member?
  (lambda (a lat)
    (cond
      ((null? lat) #f)
      ((equal? (car lat) a) #t)
      (else (member? a (cdr lat))))))

(define db-conn (virtual-connection 
		 (connection-pool
		  (lambda ()
		    (postgresql-connect #:user "feed"
					#:database "feed"
					#:password "abc123")))))

;; stop words
(define common-words '("the" "be" "to" "of" "and" "a" "in" "that" "have" "I" "it" "for" "not" "on"
                       "with" "he" "as" "you" "do" "at" "this" "but" "his" "by" "from" "they" "we" "again"
                       "say" "her" "she" "or" "an" "will" "my" "one" "all" "would" "wouldn't" "there" "their"
                       "what" "so" "up" "out" "if" "about" "who" "who's" "get" "got" "which" "go" "me"
                       "when" "he'll" "he's" "I've" "isn't" "mustn't" "more" "hers"
                       "doesn't" "did" "must" "every" "make" "making" "can" "like" "is" "am" "are" 
                       "aren't" "been" "were" "was" "no" "both" "cannot" "each" "further" "I'm" "I'd"
                       "two" "how" "our" "back" "also" "think" "over" "its" "day" "us" "most" "give"
                       "these" "any" "because" "want" "new" "even" "way" "well" "first" "work" "after"
                       "use" "take" "person" "into" "your" "good" "some" "look" "only" "tell" "really"
                       "never" "something" "nothing" "anything" "himself" "call" "leave" "try"
                       "where" "while" "since" "now" "although" "without" "among" "around" "too" "here"
                       "before" "nor" "though" "except" "unless" "once" "whether" "until" "very" "yes"
                       "able" "same" "bad" "public" "few" "great" "little" "old" "right" "big" "high"
                       "let" "then" "anyone" "actually" "under" "round" "just" "being" "them" "goes"
                       "away" "factor"  "has" "said" "down" "does" "above" "gave" "why" "sure" "came"
                       "didn't" "-" "seem" "therefore" "had" "see" "come" "part" "via" "possibly" "full"
                       "spoken" "speak" "coming" "itself" "ride" "true" "totally" "name" "hide" "false"
                       "yourselves" "yours" "between" "time" "past" "him" "ask" "maybe" "lot"))

(define stem
  (lambda (word)
    (let ((dictionary '(("videos" . "video")
                        ("running" . "run")
                        ("vmas" . "vma")
                        ("albums" . "album")
                        ("skipping" . "skip")
                        ("runs" . "run")
                        ("songs" . "song")
                        ("beyonce" . "beyoncé")
                        ("beyoncã©" . "beyoncé")
                        ("beyince" . "beyoncé")
                        ("dollars" . "dollar")
                        ("names" . "name")
                        ("asking" . "ask")
                        ("images" . "image")
                        ("changed" . "change")
                        ("defenses" . "defense")
                        ("haunted" . "haunt")
                        ("explained" . "explain"))))
      (cond
        ((equal? (assoc word dictionary) #f) word)
        (else
         (cdr (assoc word dictionary)))))))

(define normalize-word
  (lambda (word)
    (stem
     (string-downcase
      (regexp-replace* #rx"\"" 
                      (regexp-replace* #rx"[?.,;:!()]"
                                       (regexp-replace #rx"'s[.]?$" word "") "") "")))))

;; parse a string into hash in which the values are the number of occurrences
(define parse-string-hash
  (lambda (hsh str)
          (letrec ((parse
                (lambda (word-list)
                  (cond
                    ((null? word-list) #f)
                    ((member? (string-downcase (car word-list)) common-words)
                     (parse (cdr word-list)))
                    ((string->number (car word-list))
                     (parse (cdr word-list)))
                    (else
                     (let ((current-word
                            (normalize-word (car word-list))))
                       (cond
                         ((not (hash-has-key? hsh current-word))
                          (hash-set! hsh current-word 0)))
                       (hash-set! hsh current-word
                                  (add1 (hash-ref hsh current-word)))
                       (parse (cdr word-list))))))))
            (parse (string-split str)))))
`
(define hate-hash (make-hash))
(define love-hash (make-hash))
(for/list (((title desc love) (in-query db-conn "select title, description, love from item where love is not null")))
  (set! desc (regexp-replace* #rx"<[^>]*>" desc ""))
  (cond
    ((equal? love #f)
     (parse-string-hash hate-hash title)
     (parse-string-hash hate-hash desc))
    (else
     (parse-string-hash love-hash title)
     (parse-string-hash love-hash desc))))

;; the public function - 1 for hate and 0 for love
(define check-love
  (lambda (item-id)
    (let* ((product 1)
           (inv-product 1)
           (mystery-hash (make-hash))
           (vec (query-row db-conn "select title, description from item where id = $1" item-id))
           (new-string "")
           (test-string (string-append (vector-ref vec 0) " " (regexp-replace* #rx"<[^>]*>" (vector-ref vec 1) ""))))
      (string-replace test-string (string (integer->char 160)) (string (integer->char 32)))
      (letrec ((replace (lambda (char-list)
                          (cond
                            ((null? char-list) #f)
                            ((equal? (integer->char 160) (car char-list))
                             (set! new-string (string-append new-string (string (integer->char 32))))
                             (replace (cdr char-list)))
                            (else
                             (set! new-string (string-append new-string (string (car char-list))))
                             (replace (cdr char-list))))))) (replace (string->list test-string)))
      (letrec ((parse
                (lambda (word-list)
                  (cond
                    ((null? word-list) #f)
                    ((member? (normalize-word (car word-list)) common-words)
                     (parse (cdr word-list)))
                    ((string->number (normalize-word (car word-list)))
                     (parse (cdr word-list)))
                    (else
                     (let ((current-word
                            (normalize-word (car word-list))))
                       (let ((nlove (hash-ref love-hash current-word 0))
                             (nhate (hash-ref hate-hash current-word 0)))
                         (cond
                           ((member? (string-downcase current-word) common-words)
                            (parse (cdr word-list)))
                           ((string->number current-word)
                            (parse (cdr word-list)))
                           ((and (equal? nlove 0) (equal? nhate 0))
                            (hash-set! mystery-hash current-word .01)
                            (parse (cdr word-list)))
                           (else
                            (hash-set! mystery-hash current-word (max .01 (min .99 (/ nhate (+ nhate nlove)))))
                            (parse (cdr word-list)))))))))))
        (parse (string-split new-string )))
      (letrec ((x
                (lambda (word-list)
                    (cond
                      ((null? word-list) #f)
                      ((member? (normalize-word (car word-list)) common-words)
                       (x (cdr word-list)))
                      ((string->number (normalize-word (car word-list)))
                       (x (cdr word-list)))
                      (else
                       (let ((current-word (normalize-word (car word-list))))
                         (cond
                           ((equal? (* product (hash-ref mystery-hash current-word)) 0.0)
                            (x (cdr word-list)))
                           (else
                            (set! product (* product (hash-ref mystery-hash current-word)))
                            (set! inv-product (* inv-product (- 1 (hash-ref mystery-hash current-word))))
                            (x (cdr word-list))))))))))
        (x (string-split new-string)))
      (/ product (+ product inv-product)))))

(provide check-love)
