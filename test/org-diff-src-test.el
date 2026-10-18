;;; org-diff-src-test.el --- Tests for org-diff-src -*- lexical-binding: t; -*-

;; Copyright (C) 2026 TomoeMami

;; Author: TomoeMami <trembleafterme@outlook.com>
;; Assisted-by: DeepSeek:deepseek-flash
;; SPDX-License-Identifier: GPL-3.0-or-later

;; This file is not part of GNU Emacs.

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; ERT tests for org-diff-src.  Run them with "make test".

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'org-diff-src)

(defmacro org-diff-src-test-with-org (contents &rest body)
  "Create a temporary Org buffer with CONTENTS and evaluate BODY."
  (declare (indent 1))
  `(with-temp-buffer
     (org-mode)
     (insert ,contents)
     (goto-char (point-min))
     ,@body))

(defun org-diff-src-test--faces-at (string)
  "Return the face property of the first character of STRING.
Point is assumed to be in a buffer containing STRING."
  (save-excursion
    (goto-char (point-min))
    (search-forward string)
    (get-text-property (1- (point)) 'face)))

(ert-deftest org-diff-src-test-diff-strings ()
  "A unified diff is produced with the requested labels."
  (let ((diff (org-diff-src-diff-strings "one\ntwo\n" "one\nTWO\n"
                                         "old" "new")))
    (should (string-match-p "\\`--- old\n\\+\\+\\+ new\n" diff))
    (should (string-match-p "^-two\n\\+TWO\n" diff))))

(ert-deftest org-diff-src-test-diff-strings-no-change ()
  "Comparing equal strings returns an empty diff."
  (should (equal "" (org-diff-src-diff-strings "same\n" "same\n"))))

(ert-deftest org-diff-src-test-diff-strings-clean-labels ()
  "Newlines in labels are replaced by spaces."
  (let ((diff (org-diff-src-diff-strings "a\n" "b\n"
                                         "old\nlabel" "new")))
    (should (string-match-p "\\`--- old label\n" diff))))

(ert-deftest org-diff-src-test-diff-files ()
  "Files can be compared and labeled."
  (let ((old (make-temp-file "org-diff-src-test-old-"))
        (new (make-temp-file "org-diff-src-test-new-")))
    (unwind-protect
        (progn
          (with-temp-file old (insert "alpha\nbeta\n"))
          (with-temp-file new (insert "alpha\nBETA\n"))
          (let ((diff (org-diff-src-diff-files old new "old" "new")))
            (should (string-match-p "^-beta\n\\+BETA\n" diff))))
      (delete-file old)
      (delete-file new))))

(ert-deftest org-diff-src-test-diff-buffers ()
  "Buffers can be compared and labeled."
  (let ((old (generate-new-buffer " *org-diff-src-old*"))
        (new (generate-new-buffer " *org-diff-src-new*")))
    (unwind-protect
        (progn
          (with-current-buffer old (insert "left\n"))
          (with-current-buffer new (insert "right\n"))
          (let ((diff (org-diff-src-diff-buffers old new "old" "new")))
            (should (string-match-p "^-left\n\\+right\n" diff))))
      (kill-buffer old)
      (kill-buffer new))))

(ert-deftest org-diff-src-test-mode-mapping ()
  "Org maps the source language to `org-diff-src-mode'."
  (should (eq (org-src-get-lang-mode "org-diff-src")
              'org-diff-src-mode)))

(ert-deftest org-diff-src-test-insert-and-refine ()
  "Inserting a diff creates a block with character-level faces."
  (org-diff-src-test-with-org ""
    (let ((diff (org-diff-src-diff-strings "one\ntwo\n" "one\nTWO\n"
                                           "old" "new")))
      (org-diff-src-insert diff)
      (should (string-match-p "\\`#\\+begin_src org-diff-src\n"
                              (buffer-string)))
      (should (string-match-p "#\\+end_src\n\\'" (buffer-string)))
      (should (memq 'diff-refine-added
                    (org-diff-src-test--faces-at "+TWO")))
      (should (memq 'diff-refine-removed
                    (org-diff-src-test--faces-at "-two"))))))

(ert-deftest org-diff-src-test-insert-empty ()
  "An empty diff inserts the configured no-difference message."
  (org-diff-src-test-with-org ""
    (org-diff-src-insert "")
    (should (string-match-p "# No differences" (buffer-string)))
    (should (string-match-p "#\\+begin_src org-diff-src" (buffer-string)))))

(ert-deftest org-diff-src-test-insert-requires-org ()
  "Insertion outside Org signals a user error."
  (with-temp-buffer
    (fundamental-mode)
    (should-error (org-diff-src-insert "--- a\n+++ b\n")
                  :type 'user-error)))

(ert-deftest org-diff-src-test-insert-refuses-inside-block ()
  "Insertion inside an existing source block is refused."
  (org-diff-src-test-with-org "#+begin_src emacs-lisp\n(+ 1 1)\n#+end_src\n"
    (search-forward "(+ 1 1)")
    (should-error (org-diff-src-insert "--- a\n+++ b\n")
                  :type 'user-error)))

(ert-deftest org-diff-src-test-heading-candidates-unique ()
  "Duplicate heading titles receive unique completion keys."
  (org-diff-src-test-with-org "* Same\none\n* Same\ntwo\n"
    (let ((candidates (org-diff-src--heading-candidates)))
      (should (= 2 (length candidates)))
      (should (= 2 (length (delete-dups (mapcar #'car candidates)))))
      (should (equal '("Same" "Same <2>")
                     (mapcar #'car candidates))))))

(ert-deftest org-diff-src-test-compare-headings ()
  "Two subtrees can be compared without touching the kill ring."
  (org-diff-src-test-with-org "* Original\nfoo bar\n* New\nfoo BAR\n"
    (let ((kill-ring '("sentinel"))
          (kill-ring-yank-pointer nil)
          (answers '("Original" "New")))
      (cl-letf (((symbol-function 'completing-read)
                 (lambda (_prompt collection &rest _)
                   (let ((answer (pop answers)))
                     (or (car (assoc answer collection))
                         (error "Unexpected candidate: %s" answer))))))
        (org-diff-src-compare-headings))
      (should (equal kill-ring '("sentinel")))
      (should (string-match-p "--- original: Original\n" (buffer-string)))
      (should (string-match-p "\\+\\+\\+ new: New\n" (buffer-string)))
      (should (string-match-p "^-\\* Original\n-foo bar\n\\+\\* New\n\\+foo BAR\n"
                              (buffer-string)))
      (should (memq 'diff-refine-added
                    (org-diff-src-test--faces-at "+foo BAR"))))))

(ert-deftest org-diff-src-test-compare-nested-headings ()
  "Nested subtrees are normalized to level 1 before comparison."
  (org-diff-src-test-with-org
      "* Parent\n** Original\nfoo\n*** Child\nold\n** New\nfoo\n*** Child\nnew\n"
    (let ((answers '("Original" "New")))
      (cl-letf (((symbol-function 'completing-read)
                 (lambda (_prompt collection &rest _)
                   (let ((answer (pop answers)))
                     (or (car (assoc answer collection))
                         (error "Unexpected candidate: %s" answer))))))
        (org-diff-src-compare-headings)))
    (should (string-match-p "^-\\* Original\n" (buffer-string)))
    (should (string-match-p "^ \\*\\* Child\n-old\n" (buffer-string)))
    (should (string-match-p "^ \\*\\* Child\n-old\n\\+new\n"
                            (buffer-string)))))

(ert-deftest org-diff-src-test-wrap-region ()
  "A region containing a diff is replaced by a source block."
  (org-diff-src-test-with-org "--- old\n+++ new\n@@ -1 +1 @@\n-a\n+b\n"
    (org-diff-src-wrap-region (point-min) (point-max))
    (should (string-match-p "\\`#\\+begin_src org-diff-src\n"
                            (buffer-string)))
    (should-not (string-match-p "\\`--- old" (buffer-string)))))

(ert-deftest org-diff-src-test-refontify-block ()
  "The block can be refontified after its faces were removed."
  (org-diff-src-test-with-org ""
    (org-diff-src-insert
     (org-diff-src-diff-strings "one\ntwo\n" "one\nTWO\n" "old" "new"))
    (remove-text-properties (point-min) (point-max) '(face nil))
    (search-backward "+TWO")
    (org-diff-src-refontify-block)
    (should (memq 'diff-refine-added
                  (org-diff-src-test--faces-at "+TWO")))))

(provide 'org-diff-src-test)

;;; org-diff-src-test.el ends here
