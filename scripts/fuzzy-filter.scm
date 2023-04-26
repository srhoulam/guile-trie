#!/usr/bin/env -S guile -e main -s
coding: utf-8
!#

;;
;; Copyright (C) 2023 Saad Rhoulam <saad@rhoulam.tech>
;;
;; Author: Saad Rhoulam <saad@rhoulam.tech>
;;
;; This file is part of guile-trie.
;;
;; guile-trie is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; guile-trie is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

(use-modules
 (ice-9 format)
 (ice-9 getopt-long)
 (ice-9 q)
 (ice-9 rdelim)
 (srfi srfi-1)
 (trie trie)
 )

(define (main args)
  (let* ((option-spec '((version (single-char #\v) (value #f))
                        (help    (single-char #\h) (value #f))))
         (options
          (getopt-long args option-spec #:stop-at-first-non-option #t))
         (help-wanted (option-ref options 'help #f))
         (version-wanted (option-ref options 'version #f))
         (other-args (option-ref options '() (list)))
         (pattern (if (null? other-args)
                      #f
                      (car other-args)))
         (bin-name (car args))
         )

    (when help-wanted
      (display-help bin-name)
      (exit 0))

    (when version-wanted
      (display-version)
      (exit 0))

    (if pattern
        (fuzzy-filter-main pattern)
        (begin
          (display-help bin-name)
          (exit 1)))))

(define (display-help bin-name)
  (format #t "Usage: ~a pattern\n" bin-name))
(define (display-version)
  (format #t "fuzzy-filter v0.1.0\n"))

(define (fuzzy-filter-main pattern)
  (let* ((trie (read-stdin-to-trie))
         (matching-nodes (trie-fuzzy-filter trie pattern))
         )

    (for-each
     (lambda (matching-node)
       (let* ((matching-prefix (trie-prefix matching-node))
              (matching-strings (trie-enumerate matching-node)))
         (for-each (lambda (str) (format #t "~a\n" str)) matching-strings)))
    matching-nodes)
  ))

(define (read-stdin-to-trie)
  (let ((trie (make-hash-table)))
    (do ((line (read-line) (read-line)))
        ((eof-object? line))
      (trie-insert trie line))
    trie))
