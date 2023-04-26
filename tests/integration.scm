(define-module (tests integration)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-64)

  #:use-module (trie trie)
  )

(test-begin "integration test")
(define test-trie (make-trie))
(trie-insert test-trie "test string 1")
(trie-insert test-trie "test string 2")
(trie-insert test-trie "test string 3")
(trie-insert test-trie "test text 1")
(trie-insert test-trie "test text 2")
(trie-insert test-trie "test text 3")
(trie-insert test-trie "other thing 1")
(trie-insert test-trie "other thing 2")
(trie-insert test-trie "other thing 3")

(let (
      (test-string-answers
       '("test string 1"
         "test string 2"
         "test string 3"))
      (test-text-answers
       '("test text 1"
         "test text 2"
         "test text 3"))
      (test-answers
       '("test string 1"
         "test string 2"
         "test string 3"
         "test text 1"
         "test text 2"
         "test text 3"))
      (other-thing-answers
       '("other thing 1"
         "other thing 2"
         "other thing 3"))
      (test-string-node (trie-ref test-trie "test string "))
      (test-node (trie-ref test-trie "test "))
      (other-thing-node (trie-ref test-trie "other thing "))
      (fuzzy-node-1 (car (trie-fuzzy-filter test-trie "tsg")))
      (fuzzy-node-2 (car (trie-fuzzy-filter test-trie "txt")))
      (fuzzy-node-3 (car (trie-fuzzy-filter test-trie "org")))
      )

  (test-assert (string= "test string " (trie-prefix test-string-node)))
  (test-assert
      (every
       identity
       (map (lambda (str) find str test-string-answers)
            (trie-enumerate test-string-node))))

  (test-assert (string= "test " (trie-prefix test-node)))
  (test-assert
      (every
       identity
       (map (lambda (str) find str test-answers)
            (trie-enumerate test-node))))

  (test-assert (string= "other thing " (trie-prefix other-thing-node)))
  (test-assert
      (every
       identity
       (map (lambda (str) find str other-thing-answers)
            (trie-enumerate other-thing-node))))

  (test-assert (string= "test string" (trie-prefix fuzzy-node-1)))
  (test-assert
      (every
       identity
       (map (lambda (str) find str test-string-answers)
            (trie-enumerate fuzzy-node-1))))

  (test-assert (string= "test text" (trie-prefix fuzzy-node-2)))
  (test-assert
      (every
       identity
       (map (lambda (str) find str test-text-answers)
            (trie-enumerate fuzzy-node-2))))

  (test-assert (string= "other thing" (trie-prefix fuzzy-node-3)))
  (test-assert
      (every
       identity
       (map (lambda (str) find str other-thing-answers)
            (trie-enumerate fuzzy-node-3))))
  )

(test-end "integration test")
