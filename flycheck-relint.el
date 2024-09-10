;;; flycheck-relint.el --- A Flycheck checker for elisp regular expressions  -*- lexical-binding: t -*-

;; Copyright (C) 2020  Steve Purcell

;; Author: Steve Purcell <steve@sanityinc.com>
;; Keywords: lisp
;; Package-Version: 0-pre
;; URL: https://github.com/purcell/flycheck-relint
;; Package-Requires: ((emacs "26.1") (flycheck "0.22") (relint "2.0"))

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Provides feedback via flycheck about issues with `rx' and literal
;; regular expressions in Emacs Lisp, using `relint'.

;; To enable, use something like this:

;;    (eval-after-load 'flycheck
;;      '(flycheck-relint-setup))

;;; Code:

(require 'flycheck)
(require 'relint)
(require 'pcase)


(defun flycheck-relint--escape-string (string)
  (replace-regexp-in-string
   (rx (any cntrl ?\177 (#x3fff80 . #x3fffff) ?\\ ?\"))
   (lambda (s)
     (let ((c (logand (string-to-char s) #xff)))
       (or (cdr (assq c '((?\b . "\\b")
                          (?\t . "\\t")
                          (?\n . "\\n")
                          (?\v . "\\v")
                          (?\f . "\\f")
                          (?\r . "\\r")
                          (?\e . "\\e")
                          (?\\ . "\\\\")
                          (?\" . "\\\""))))
           (format "\\%03o" c))))
   string t t))

(defun flycheck-relint--quote-with-caret (string beg end)
  ;; FIXME: It would be better to use a different face for highlighting the
  ;; relevant (BEG..END) part of the string instead of this primitive ASCII
  ;; rendering, but it does not seem possible in flycheck.
  ;; (The ASCII caret is particularly bad in tooltips which typically use
  ;; a proportial typeface.)
  (let* ((esc-str (flycheck-relint--escape-string string))
         (esc-beg (length (flycheck-relint--escape-string
                           (substring string nil beg))))
         (esc-end (if end
                      (length (flycheck-relint--escape-string
                               (substring string nil (1+ end))))
                    (min (length string) (1+ esc-beg))))
         (caret (concat (make-string esc-beg ?.)
                        (make-string (- esc-end esc-beg) ?^))))
    (format "  \"%s\"\n   %s" esc-str caret)))

(defun flycheck-relint--diag-to-flycheck (diag group)
  (let* ((message (relint-diag-message diag))
         (string (relint-diag-string diag))
         (msg (if (and string
                       ;; FIXME: We don't really need to put the string in the
                       ;; message when it is present as a literal in the
                       ;; source code, but flycheck's in-source highlighting
                       ;; isn't always very clear.
                       ;;  (not (eq (relint-diag-pos-type diag) 'string))
                       )
                  (format "%s\n%s"
                          message
                          (flycheck-relint--quote-with-caret
                           string
                           (relint-diag-beg-idx diag)
                           (relint-diag-end-idx diag)))
                message))
         (beg-pos (relint-diag-beg-pos diag))
         (end-pos (relint-diag-end-pos diag)))
    (flycheck-relint--error-at beg-pos
                               ;; add 1 to make interval half-open
                               (and end-pos (1+ end-pos))
                               (relint-diag-severity diag)
                               msg group)))

(defun flycheck-relint--start (_checker callback)
  "Flycheck start function for relint.
CHECKER is this checker, and CALLBACK is the flycheck dispatch function."
  (let ((group nil))
    (funcall callback 'finished
             (mapcar (lambda (diag)
                       ;; In relint, `info'-level diags that follow a
                       ;; non-`info' diag all belong to the same group.
                       (unless (eq (relint-diag-severity diag) 'info)
                         (setq group (make-symbol "relint")))  ;start new group
                       (flycheck-relint--diag-to-flycheck diag group))
                     (relint-buffer (current-buffer))))))

(defun flycheck-relint--error-at (beg end severity message group)
  "Create a flycheck error with MESSAGE and SEVERITY for [BEG,END)."
  (let* ((beg-line (line-number-at-pos beg t))
         (beg-col (save-excursion (goto-char beg) (1+ (current-column))))
         (end-line (and end (line-number-at-pos end t)))
         (end-col (and end (save-excursion (goto-char end)
                                           (1+ (current-column))))))
    (flycheck-error-new-at beg-line beg-col severity message
                           :end-line end-line :end-column end-col
                           :group group
                           :checker 'flycheck-relint)))



;;; Checker definition

(flycheck-define-generic-checker 'emacs-lisp-relint
  "Report errors detected by `relint'."
  :start #'flycheck-relint--start
  :modes '(emacs-lisp-mode lisp-interaction-mode))


;;;###autoload
(defun flycheck-relint-setup ()
  "Setup flycheck-package.
Add `emacs-lisp-relint' to `flycheck-checkers' and set up the checker chain."
  (interactive)
  (add-to-list 'flycheck-checkers 'emacs-lisp-relint t)
  (flycheck-add-next-checker 'emacs-lisp 'emacs-lisp-relint t)
  (when (flycheck-valid-checker-p 'emacs-lisp-package)
    (flycheck-add-next-checker 'emacs-lisp-package 'emacs-lisp-relint t)))


(provide 'flycheck-relint)
;;; flycheck-relint.el ends here
