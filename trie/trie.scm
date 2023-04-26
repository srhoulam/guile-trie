;; -*- coding: utf-8 -*-
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


(define-module (trie trie)
  #:use-module (ice-9 q)
  #:export (make-trie
            trie-enumerate
            trie-enumerate-suffixes
            trie-fuzzy-filter
            trie-insert
            trie-prefix
            trie-ref))

(define make-trie make-hash-table)
(define (trie-enumerate trie-node)
  (trie-enumerate-suffixes (trie-prefix trie-node) trie-node))
(define (trie-enumerate-suffixes prefix trie-node)
  (if (hash-ref trie-node 'terminal #f)
      (cons prefix (trie-enumerate-down prefix trie-node))
      (trie-enumerate-down prefix trie-node)))
(define (trie-enumerate-down prefix* trie-node*)
  (hash-fold
   (lambda (k v acc)
     (if (char? k)
         (let ((new-prefix (string-append-char prefix* k)))
           (append (trie-enumerate-suffixes new-prefix v) acc))
         acc))
   (list)
   trie-node*))

(define (trie-fuzzy-filter trie-node pattern)
  (let ((down-q (make-q)) ;; :: [(pattern, node)]
        (matches (list))) ;; :: [node]
    (enq! down-q (cons pattern trie-node))
    (let loop ((curr-item (deq! down-q)))
      (let ((pat (car curr-item))
            (node (cdr curr-item))
            )
        (if (string-null? pat)
            (set! matches (cons node matches))
            (let ((pat-head (string-ref pat 0))
                  (pat-tail (substring pat 1))
                  )
              (hash-for-each
               (lambda (k v)
                 (when (char? k)
                     (if (eq? k pat-head)
                         (enq! down-q (cons pat-tail v))
                         (enq! down-q (cons pat v)))))
               node))))

      (if (q-empty? down-q)
          (noop)
          (loop (deq! down-q))))

    (reverse matches)))

(define (trie-insert trie-node str)
  (if (string-null? str)
      (hash-set! trie-node 'terminal #t)
      (let* ((str-head        (string-ref str 0))
             (str-tail        (substring str 1))
             (next-node (hash-ref trie-node str-head #f))
             )
        (unless next-node
          (set! next-node (make-hash-table))
          (hash-set! next-node 'parent trie-node)
          (hash-set! next-node 'value str-head)
          (hash-set! trie-node str-head next-node)
          )
        (trie-insert next-node str-tail))))

(define (trie-prefix trie-node)
  (let ((prefix-stack (list)))
    (do ((node trie-node (hash-ref node 'parent #f)))
        ((not (hash-ref node 'value #f)))

      (set! prefix-stack
            (cons (hash-ref node 'value) prefix-stack)))
    (list->string prefix-stack)))

(define (trie-ref trie-node prefix)
  (if (string-null? prefix)
      trie-node
      (let* ((prefix-head (string-ref prefix 0))
             (prefix-tail (substring prefix 1))
             (next-node   (hash-ref trie-node prefix-head #f))
             )
        (if next-node
            (trie-ref next-node prefix-tail)
            next-node))))

(define (string-append-char str ch)
  (string-append str (char-set->string (char-set ch))))
