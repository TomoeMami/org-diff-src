;;; org-diff-src.el --- Display unified diffs in Org source blocks -*- lexical-binding: t; -*-

;; Copyright (C) 2026 TomoeMami

;; Author: TomoeMami <trembleafterme@outlook.com>
;; Assisted-by: DeepSeek:deepseek-flash
;; Maintainer: TomoeMami <trembleafterme@outlook.com>
;; Version: 0.2.0
;; Package-Requires: ((emacs "27.1"))
;; Keywords: convenience
;; URL: https://github.com/TomoeMami/org-diff-src
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

;; This package displays unified diff text as an Org source block with
;; character-level highlighting.
;;
;; Org copies font-lock faces, but not overlays, from the temporary
;; buffer used to fontify a source block.  `diff-mode' implements its
;; fine-grained refinement with overlays, so the refinement is normally
;; lost when a diff is shown inside Org.  The `org-diff-src-mode' defined
;; here enables `diff-refine' = `font-lock' and turns the refinement
;; overlays into `face' text properties, which Org can copy.
;;
;; The default block language is `org-diff-src', which Org resolves
;; to `org-diff-src-mode' without any setup.  If you already have
;; blocks written as "ediff" (or another language), call
;; `org-diff-src-setup' to map those languages to the mode; the
;; languages to map are listed in `org-diff-src-extra-languages'.
;;
;; The core entry point is `org-diff-src-insert', which accepts any
;; unified diff string.  Diffs can be produced by the helpers
;; `org-diff-src-diff-strings', `org-diff-src-diff-files', and
;; `org-diff-src-diff-buffers', or by any external tool.  Ready-to-use
;; commands compare Org subtrees, files, or buffers, and insert an
;; existing diff file, a region, or the current kill.
;;
;; A minimal example:
;;
;;   (require 'org-diff-src)
;;   (with-current-buffer (get-buffer-create "*example*")
;;     (org-mode)
;;     (org-diff-src-insert
;;      (org-diff-src-diff-strings "one\ntwo\n" "one\nTWO\n")))
;;
;; The resulting block looks like this:
;;
;;   #+begin_src org-diff-src
;;   --- old
;;   +++ new
;;   @@ -1,2 +1,2 @@
;;    one
;;   -two
;;   +TWO
;;   #+end_src
;;
;; The characters that actually changed are highlighted with the
;; `diff-refine-removed', `diff-refine-added', and
;; `diff-refine-changed' faces.  No Ediff session is started.

;;; Code:

(require 'diff)
(require 'diff-mode)
(require 'org)
(require 'org-element)
(require 'org-src)
(require 'subr-x)

(defgroup org-diff-src nil
  "Display unified diffs in Org source blocks."
  :group 'org
  :prefix "org-diff-src-"
  :link '(url-link "https://github.com/TomoeMami/org-diff-src"))

(defcustom org-diff-src-diff-switches '("-u")
  "Switches passed to `diff-command' when generating a diff.
The default requests a unified diff, which is what
`org-diff-src-mode' knows how to refine.  If you add switches,
keep \"-u\" in the list."
  :type '(repeat string)
  :group 'org-diff-src)

(defcustom org-diff-src-no-diff-message "# No differences"
  "Text inserted when there is no diff to display."
  :type 'string
  :group 'org-diff-src)

(defconst org-diff-src--language "org-diff-src"
  "Built-in source language used for generated Org blocks.")

(defcustom org-diff-src-default-language org-diff-src--language
  "Default source language for blocks inserted by this package.
The built-in `org-diff-src' language is resolved to
`org-diff-src-mode' automatically.  To use another language, call
`org-diff-src-setup' first so that it maps to the mode."
  :type 'string
  :group 'org-diff-src)

(defcustom org-diff-src-extra-languages '("ediff")
  "Additional source languages to map to `org-diff-src-mode'.
`org-diff-src-setup' installs these mappings.  The default
\"ediff\" keeps blocks written for the original Ediff-style
snippet working.  Mapping \"diff\" here would override the normal
`diff-mode' fontification for every Org diff block."
  :type '(repeat string)
  :group 'org-diff-src)

(defcustom org-diff-src-fontify-immediately t
  "Whether to fontify a newly inserted diff block immediately.
Turning this off can speed up insertion of large diffs.  Org will
fontify the block during a later fontification pass, or you can
run `org-diff-src-refontify-block' explicitly."
  :type 'boolean
  :group 'org-diff-src)

;;;; Diff generation

(defun org-diff-src--clean-label (label fallback)
  "Return LABEL as a single line, or FALLBACK if LABEL is empty.
Tabs and newlines in LABEL are replaced by spaces."
  (let ((label (if (and label (not (string-empty-p label)))
                   label
                 fallback)))
    (replace-regexp-in-string "[\t\n\r]+" " " label)))

(defun org-diff-src--label-arguments (old-label new-label)
  "Return `diff' arguments that label the two sides.
OLD-LABEL and NEW-LABEL are the labels to use.  Prefer the GNU
long option `--label' and fall back to the portable short option
`-L'."
  (when (and old-label new-label)
    (if (eq (diff-check-labels) t)
        (list "--label" old-label "--label" new-label)
      (list "-L" old-label "-L" new-label))))

(defun org-diff-src--write-temp-text (file text coding)
  "Write TEXT to FILE using coding system CODING."
  (let ((coding-system-for-write coding))
    (with-temp-file file
      (insert text))))

(defun org-diff-src--call-diff (old-file new-file old-label new-label coding)
  "Run `diff-command' on OLD-FILE and NEW-FILE.
OLD-LABEL and NEW-LABEL name the two sides.  CODING is used when
reading the output.  Return the unified diff as a string.

Signal an error if `diff' fails for a reason other than finding
differences."
  (let ((output (generate-new-buffer " *org-diff-src-diff*"))
        (error-file (make-temp-file "org-diff-src-error-"))
        status)
    (unwind-protect
        (progn
          (setq status
                (let ((coding-system-for-read coding)
                      (switches (if (listp org-diff-src-diff-switches)
                                    org-diff-src-diff-switches
                                  (list org-diff-src-diff-switches))))
                  (apply #'call-process
                         diff-command nil (list output error-file) nil
                         (append switches
                                 (org-diff-src--label-arguments
                                  old-label new-label)
                                 (list old-file new-file)))))
          (with-current-buffer output
            (if (memq status '(0 1))
                (buffer-string)
              (error "org-diff-src: diff failed with status %s: %s"
                     status
                     (or (with-temp-buffer
                           (insert-file-contents error-file)
                           (string-trim (buffer-string)))
                         "no error output")))))
      (kill-buffer output)
      (ignore-errors (delete-file error-file)))))

;;;###autoload
(defun org-diff-src-diff-strings (old new &optional old-label new-label coding)
  "Return a unified diff between the strings OLD and NEW.
OLD-LABEL and NEW-LABEL are used in the `---' and `+++' headers;
they default to \"old\" and \"new\".  CODING is the coding system
used for the temporary files and for reading the output; it
defaults to the current buffer's coding system.

The diff is generated with `diff-command' and
`org-diff-src-diff-switches'."
  (let* ((coding (or coding buffer-file-coding-system 'utf-8-unix))
         (old-file (make-temp-file "org-diff-src-old-"))
         (new-file (make-temp-file "org-diff-src-new-")))
    (unwind-protect
        (progn
          (org-diff-src--write-temp-text old-file (or old "") coding)
          (org-diff-src--write-temp-text new-file (or new "") coding)
          (org-diff-src--call-diff
           old-file new-file
           (org-diff-src--clean-label old-label "old")
           (org-diff-src--clean-label new-label "new")
           coding))
      (ignore-errors (delete-file old-file))
      (ignore-errors (delete-file new-file)))))

;;;###autoload
(defun org-diff-src-diff-files (old-file new-file &optional old-label new-label)
  "Return a unified diff between files OLD-FILE and NEW-FILE.
OLD-LABEL and NEW-LABEL default to the file names.  Remote files
are copied to local temporary files first."
  (let* ((old-copy (file-local-copy old-file))
         (new-copy (file-local-copy new-file))
         (old-path (or old-copy old-file))
         (new-path (or new-copy new-file)))
    (unwind-protect
        (org-diff-src--call-diff
         old-path new-path
         (org-diff-src--clean-label old-label old-file)
         (org-diff-src--clean-label new-label new-file)
         (or buffer-file-coding-system 'utf-8-unix))
      (when old-copy (ignore-errors (delete-file old-copy)))
      (when new-copy (ignore-errors (delete-file new-copy))))))

(defun org-diff-src--buffer-text (buffer)
  "Return the entire text of BUFFER."
  (let ((resolved (get-buffer buffer)))
    (unless resolved
      (user-error "No such buffer: %s" buffer))
    (with-current-buffer resolved
      (save-restriction
        (widen)
        (buffer-substring-no-properties (point-min) (point-max))))))

(defun org-diff-src--inside-src-block-p ()
  "Return non-nil when point is inside a source block."
  (eq (org-element-type (org-element-at-point)) 'src-block))

;;;###autoload
(defun org-diff-src-diff-buffers (old-buffer new-buffer
                                              &optional old-label new-label)
  "Return a unified diff between buffers OLD-BUFFER and NEW-BUFFER.
OLD-LABEL and NEW-LABEL default to the buffer names.  The coding
system of OLD-BUFFER, or failing that NEW-BUFFER, is used for the
temporary files."
  (let ((old (get-buffer old-buffer))
        (new (get-buffer new-buffer)))
    (unless old
      (user-error "No such buffer: %s" old-buffer))
    (unless new
      (user-error "No such buffer: %s" new-buffer))
    (org-diff-src-diff-strings
     (org-diff-src--buffer-text old)
     (org-diff-src--buffer-text new)
     (or old-label (buffer-name old))
     (or new-label (buffer-name new))
     (or (buffer-local-value 'buffer-file-coding-system old)
         (buffer-local-value 'buffer-file-coding-system new)
         'utf-8-unix))))

;;;; Character-level refinement

(defun org-diff-src--refinement-supported-p ()
  "Return non-nil when diff-mode's refinement internals are available."
  (and (fboundp 'diff--iterate-hunks)
       (fboundp 'diff--refine-hunk)
       (fboundp 'diff--overlay-auto-delete)))

(defun org-diff-src--copy-fine-overlays (beg end)
  "Copy fine-grained refinement faces in BEG..END to text properties.
Only overlays whose `diff-mode' property is `fine' are considered.
Overlay ranges are clipped to BEG..END.  The refinement face is
prepended to the existing `face' property, so it takes precedence
over the line-level `diff-removed' or `diff-added' face."
  (dolist (overlay (overlays-in beg end))
    (when (eq (overlay-get overlay 'diff-mode) 'fine)
      (let ((face (overlay-get overlay 'face))
            (start (max beg (overlay-start overlay)))
            (finish (min end (overlay-end overlay))))
        (when (and face (< start finish))
          (let* ((old (get-text-property start 'face))
                 (faces (cond ((null old) nil)
                              ((listp old) old)
                              (t (list old)))))
            (unless (member face faces)
              (put-text-property start finish 'face (cons face faces)))))))))

(defun org-diff-src--font-lock-refined (max)
  "Apply character-level refinement up to MAX.
This is a replacement for `diff--font-lock-refined' that records
the refinement as `face' text properties instead of overlays, so
that `org-src-font-lock-fontify-block' can copy it into the Org
buffer."
  (when (and (eq diff-refine 'font-lock)
             (org-diff-src--refinement-supported-p))
    (when (get-char-property (point) 'diff--font-lock-refined)
      (goto-char (next-single-char-property-change
                  (point) 'diff--font-lock-refined nil max)))
    (condition-case nil
        (diff--iterate-hunks
         max
         (lambda (beg end)
           (unless (get-char-property beg 'diff--font-lock-refined)
             (diff--refine-hunk beg end)
             (let ((overlay (make-overlay beg end)))
               (overlay-put overlay 'diff--font-lock-refined t)
               (overlay-put overlay 'evaporate t)
               (overlay-put overlay 'modification-hooks
                            '(diff--overlay-auto-delete))))
           ;; Re-copy on every pass: font-lock removes `face'
           ;; properties before refontifying.
           (org-diff-src--copy-fine-overlays beg end)))
      (error nil))))

(defun org-diff-src--font-lock-keywords ()
  "Return `diff-font-lock-keywords' with text-property refinement.
Every occurrence of `diff--font-lock-refined' is replaced by
`org-diff-src--font-lock-refined'.  If the required diff-mode
internals are unavailable, return the keywords unchanged so that
line-level highlighting still works."
  (if (org-diff-src--refinement-supported-p)
      (mapcar (lambda (keyword)
                (if (or (eq keyword 'diff--font-lock-refined)
                        (and (consp keyword)
                             (eq (car keyword) 'diff--font-lock-refined)))
                    (list #'org-diff-src--font-lock-refined)
                  keyword))
              diff-font-lock-keywords)
    diff-font-lock-keywords))

;;;###autoload
(define-derived-mode org-diff-src-mode diff-mode "Org-Diff"
  "Major mode for `org-diff-src' source blocks.
This is `diff-mode' with `diff-refine' set to `font-lock'.  The
refinement is written as `face' text properties so that Org can
copy it from the fontification buffer into the Org buffer.  The
mode is also used for language aliases registered by
`org-diff-src-setup'.

If the required diff-mode refinement internals are unavailable,
the mode keeps the normal line-level highlighting."
  (when (org-diff-src--refinement-supported-p)
    (setq-local diff-refine 'font-lock)
    (setq-local diff-font-lock-keywords (org-diff-src--font-lock-keywords)))
  (font-lock-flush))

;;;; Language registration

(defun org-diff-src--register-language (language)
  "Map LANGUAGE to `org-diff-src-mode' in `org-src-lang-modes'."
  (let ((pair (assoc-string language org-src-lang-modes t)))
    (if pair
        (setcdr pair 'org-diff-src)
      (push (cons language 'org-diff-src) org-src-lang-modes)))
  language)

;;;###autoload
(defun org-diff-src-setup (&optional languages)
  "Register LANGUAGES for `org-diff-src-mode'.
LANGUAGES defaults to `org-diff-src-extra-languages'.  Each
language is mapped in `org-src-lang-modes' so that Org uses
`org-diff-src-mode' when fontifying blocks written in that
language.  The built-in `org-diff-src' language does not need to
be registered.

Return the list of registered languages."
  (interactive)
  (let ((languages (or languages org-diff-src-extra-languages)))
    (dolist (language languages)
      (org-diff-src--register-language language))
    (when (called-interactively-p 'interactive)
      (message "org-diff-src: registered %s"
               (mapconcat #'identity languages ", ")))
    languages))

;;;; Inserting diffs into Org buffers

(defun org-diff-src--fontify-block (language beg end)
  "Fontify the LANGUAGE source block body between BEG and END."
  (if (fboundp 'org-src-font-lock-fontify-block)
      (condition-case err
          (org-src-font-lock-fontify-block language beg end)
        (error
         (message "org-diff-src: could not fontify %s block: %s"
                  language (error-message-string err))))
    (save-excursion
      (goto-char beg)
      (font-lock-ensure beg end))))

(defun org-diff-src--check-language (language)
  "Signal a user error unless LANGUAGE maps to `org-diff-src-mode'."
  (unless (eq (org-src-get-lang-mode language) 'org-diff-src-mode)
    (user-error "Language %s is not mapped to `org-diff-src-mode'; call `org-diff-src-setup' first"
                language)))

(defun org-diff-src--src-block-area ()
  "Return (BEG END LANGUAGE) for the diff block at point.
A block qualifies when its language maps to `org-diff-src-mode'.
Return nil when point is not inside such a source block."
  (let ((element (org-element-at-point)))
    (when (eq (org-element-type element) 'src-block)
      (let ((language (org-element-property :language element)))
        (when (and language
                   (eq (org-src-get-lang-mode language)
                       'org-diff-src-mode))
          (let ((area (org-src--contents-area element)))
            (list (nth 0 area) (nth 1 area) language)))))))

;;;###autoload
(defun org-diff-src-insert (diff &optional language)
  "Insert DIFF as a fontified source block at point.
DIFF should be a unified diff string.  If DIFF is nil or empty,
insert `org-diff-src-no-diff-message' instead.  LANGUAGE defaults
to `org-diff-src-default-language' and must map to
`org-diff-src-mode'; the built-in `org-diff-src' language does so
automatically.  Other languages require `org-diff-src-setup'.

The block is fontified immediately when
`org-diff-src-fontify-immediately' is non-nil.  Return the
position after the inserted block."
  (unless (derived-mode-p 'org-mode)
    (user-error "Not in an Org buffer"))
  (when (org-diff-src--inside-src-block-p)
    (user-error "Point is inside a source block"))
  (let ((language (format "%s" (or language org-diff-src-default-language)))
        (diff (or diff "")))
    (org-diff-src--check-language language)
    (unless (bolp)
      (insert "\n"))
    (insert "#+begin_src " language "\n")
    (let ((beg (point)))
      (if (string-empty-p diff)
          (insert org-diff-src-no-diff-message "\n")
        (insert diff)
        (unless (eq (char-before) ?\n)
          (insert "\n")))
      (let ((end (point)))
        (insert "#+end_src\n")
        (when org-diff-src-fontify-immediately
          (org-diff-src--fontify-block language beg end))
        (point)))))

;;;###autoload
(defun org-diff-src-insert-diff-file (file)
  "Insert the unified diff stored in FILE as a source block at point."
  (interactive "fDiff file: ")
  (org-diff-src-insert
   (with-temp-buffer
     (insert-file-contents file)
     (buffer-string))))

;;;###autoload
(defun org-diff-src-insert-from-kill ()
  "Insert the current kill as a fontified diff source block at point."
  (interactive)
  (org-diff-src-insert (current-kill 0)))

;;;###autoload
(defun org-diff-src-wrap-region (beg end)
  "Replace the region between BEG and END with a diff source block.
Interactively, BEG and END are the region boundaries."
  (interactive "r")
  (unless (derived-mode-p 'org-mode)
    (user-error "Not in an Org buffer"))
  (when (org-diff-src--inside-src-block-p)
    (user-error "Point is inside a source block"))
  (let ((diff (buffer-substring-no-properties beg end)))
    (atomic-change-group
      (delete-region beg end)
      (goto-char beg)
      (org-diff-src-insert diff))))

;;;###autoload
(defun org-diff-src-refontify-block ()
  "Refontify the `org-diff-src' source block at point.
This is useful after Lisp code has changed the block text without
triggering Org's normal fontification."
  (interactive)
  (unless (derived-mode-p 'org-mode)
    (user-error "Not in an Org buffer"))
  (let ((area (org-diff-src--src-block-area)))
    (unless area
      (user-error "Not in an org-diff-src source block"))
    (org-diff-src--fontify-block
     (nth 2 area) (nth 0 area) (nth 1 area))))

;;;; Comparing Org headings

(defvar org-diff-src--heading-history nil
  "Minibuffer history for `org-diff-src-compare-headings'.")

(defun org-diff-src--heading-title ()
  "Return a plain one-line title for the heading at point."
  (let ((heading (substring-no-properties
                  (or (org-get-heading t t t t) ""))))
    (if (string-empty-p heading)
        "(untitled)"
      (replace-regexp-in-string "[\t\n\r]+" " " heading))))

(defun org-diff-src--heading-candidates ()
  "Return completion candidates for headings in the accessible buffer.
The buffer restriction, if any, is respected.  Each element is a
cons (DISPLAY . (MARKER . TITLE)).  DISPLAY is unique; duplicate
titles receive a numeric suffix.  TITLE is the plain heading
text."
  (let ((seen (make-hash-table :test #'equal))
        (candidates nil))
    (org-map-entries
     (lambda ()
       (let* ((title (org-diff-src--heading-title))
              (marker (point-marker))
              (display title)
              (counter 1))
         (while (gethash display seen)
           (setq counter (1+ counter)
                 display (format "%s <%d>" title counter)))
         (puthash display t seen)
         (push (cons display (cons marker title)) candidates)))
     nil nil)
    (nreverse candidates)))

(defun org-diff-src--select-heading (prompt)
  "Read a heading in the current Org buffer, using PROMPT.
Return a cons (MARKER . TITLE)."
  (let ((candidates (org-diff-src--heading-candidates)))
    (unless candidates
      (user-error "No headings in buffer"))
    (let* ((choice (completing-read prompt candidates nil t nil
                                    'org-diff-src--heading-history))
           (entry (cdr (assoc-string choice candidates))))
      (unless entry
        (user-error "No heading selected"))
      entry)))

(defun org-diff-src--copy-subtree ()
  "Return the subtree at point without modifying the kill ring.
The kill ring, the yank pointer, and the system clipboard are
left unchanged."
  (let ((kill-ring kill-ring)
        (kill-ring-yank-pointer kill-ring-yank-pointer)
        (org-subtree-clip org-subtree-clip)
        (org-subtree-clip-folded org-subtree-clip-folded)
        (interprogram-cut-function nil)
        (interprogram-paste-function nil)
        (inhibit-message t))
    (org-copy-subtree)
    org-subtree-clip))

(defun org-diff-src--normalize-subtree (tree)
  "Return TREE pasted as a level-1 subtree.
The subtree is pasted into a temporary Org buffer with
`org-paste-subtree', which normalizes its headline level."
  (with-temp-buffer
    (let ((org-mode-hook nil)
          (org-inhibit-startup t)
          (org-odd-levels-only nil)
          (org-subtree-clip-folded nil))
      (org-mode))
    (org-paste-subtree 1 tree)
    (buffer-string)))

(defun org-diff-src--read-subtree (prompt prefix)
  "Prompt for a heading with PROMPT and return its subtree.
Return a cons (LABEL . TREE), where LABEL is PREFIX followed by
the heading title and TREE is normalized to level 1."
  (save-excursion
    (let* ((entry (org-diff-src--select-heading prompt))
           (marker (car entry))
           (title (cdr entry)))
      (goto-char marker)
      (cons (concat prefix title)
            (org-diff-src--normalize-subtree
             (org-diff-src--copy-subtree))))))

;;;###autoload
(defun org-diff-src-compare-headings ()
  "Insert a fontified diff between two Org subtrees at point.
Prompt for an original heading and a new heading in the current
buffer.  Both subtrees are copied without changing the kill ring,
normalized to level 1, and compared with `diff'.  The unified
diff is inserted as an `org-diff-src' source block."
  (interactive)
  (unless (derived-mode-p 'org-mode)
    (user-error "Not in an Org buffer"))
  (let* ((buffer (current-buffer))
         (origin (point-marker))
         (old (org-diff-src--read-subtree
               "Original heading: " "original: "))
         (new (org-diff-src--read-subtree
               "New heading: " "new: "))
         (diff (org-diff-src-diff-strings
                (cdr old) (cdr new) (car old) (car new))))
    (pop-to-buffer buffer)
    (goto-char origin)
    (org-diff-src-insert diff)))

;;;; Other comparison commands

;;;###autoload
(defun org-diff-src-compare-files (old-file new-file)
  "Insert a fontified diff between OLD-FILE and NEW-FILE at point.
When called interactively, prompt for the two files."
  (interactive
   (list (read-file-name "Original file: " nil nil t)
         (read-file-name "New file: " nil nil t)))
  (org-diff-src-insert
   (org-diff-src-diff-files old-file new-file)))

;;;###autoload
(defun org-diff-src-compare-buffers (old-buffer new-buffer)
  "Insert a fontified diff between OLD-BUFFER and NEW-BUFFER at point.
When called interactively, prompt for the two buffers."
  (interactive
   (list (read-buffer "Original buffer: " (current-buffer) t)
         (read-buffer "New buffer: " nil t)))
  (org-diff-src-insert
   (org-diff-src-diff-buffers old-buffer new-buffer)))

(provide 'org-diff-src)

;;; org-diff-src.el ends here
