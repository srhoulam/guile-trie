(use-modules
  (guix packages)
  ((guix licenses) #:prefix license:)
  (guix download)
  (guix build-system gnu)
  (gnu packages)
  (gnu packages autotools)
  (gnu packages guile)
  (gnu packages guile-xyz)
  (gnu packages pkg-config)
  (gnu packages texinfo))

(package
  (name "guile-trie")
  (version "0.1.0")
  (source "./guile-trie-0.1.0.tar.gz")
  (build-system gnu-build-system)
  (arguments `())
  (native-inputs
    `(("autoconf" ,autoconf)
      ("automake" ,automake)
      ("pkg-config" ,pkg-config)
      ("texinfo" ,texinfo)))
  (inputs `(("guile" ,guile-3.0)))
  (propagated-inputs `())
  (synopsis "A simple trie library.")
  (description
    "A library modeling tries as doubly-linked hashes,\nwhere each hash represents a single character transition.\nSupports building tries (trie-insert),\ngetting the prefix of a trie node (trie-prefix),\ntraversing a tree up to a prefix (trie-ref),\nenumerating the descendants of a trie node (trie-enumerate),\nfiltering the trie with a fuzzy pattern (trie-fuzzy-filter).")
  (home-page
    "https://github.com/srhoulam/guile-trie")
  (license license:gpl3+))

