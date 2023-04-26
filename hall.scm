(hall-description
  (name "trie")
  (prefix "guile")
  (version "0.1.0")
  (author "Saad Rhoulam")
  (copyright (2023))
  (synopsis "A simple trie library.")
  (description
    "A library modeling tries as doubly-linked hashes,\nwhere each hash represents a single character transition.\nSupports building tries (trie-insert),\ngetting the prefix of a trie node (trie-prefix),\ntraversing a tree up to a prefix (trie-ref),\nenumerating the descendants of a trie node (trie-enumerate),\nfiltering the trie with a fuzzy pattern (trie-fuzzy-filter).")
  (home-page
    "https://github.com/srhoulam/guile-trie")
  (license gpl3+)
  (dependencies `())
  (skip ())
  (files (libraries
           ((directory "trie" ((scheme-file "trie")))))
         (tests ((directory "tests" ((scheme-file "integration")))))
         (programs ((directory "scripts" ())))
         (documentation
           ((text-file "HACKING")
            (text-file "COPYING")
            (org-file "README")
            (symlink "README" "README.org")
            (directory "doc" ((texi-file "trie")))))
         (infrastructure
           ((scheme-file "guix")
            (text-file ".gitignore")
            (scheme-file "hall")))))
