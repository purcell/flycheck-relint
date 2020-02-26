;;; flycheck-relint.el --- A Flycheck checker for elisp regular expressions

;; Copyright (C) 2020  Steve Purcell

;; Author: Steve Purcell <steve@sanityinc.com>
;; Keywords: lisp
;; Version: 0
;; URL: https://github.com/purcell/flycheck-relint
;; Package-Requires: ((emacs "26.1") (flycheck "0.22") (relint "1.13"))

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


(defvar flycheck-emacs-lisp-relint--running)
(defvar flycheck-emacs-lisp-relint--current-errors)


(defun flycheck-relint--report-hook (file pos path message)
  "Hook used for advice on `relint--report'.
FILE, POS, PATH and MESSAGE are as for that function."
  (when flycheck-emacs-lisp-relint--running
    (let ((pos-line-col (relint--pos-line-col-from-toplevel-pos-path pos path)))
      (unless (relint--suppression (nth 0 pos-line-col) message)
        (push (flycheck-error-new-at (nth 1 pos-line-col)
                                     (nth 2 pos-line-col)
                                     'error
                                     message
                                     :checker 'emacs-lisp-relint)
              flycheck-emacs-lisp-relint--current-errors)))))

(advice-add 'relint--report :after 'flycheck-emacs-lisp-relint--report-hook)


(defun flycheck-relint--start (checker callback)
  "Flycheck start function for relint.
CHECKER is this checker, and CALLBACK is the flycheck dispatch function."
  (let ((source-buf (current-buffer))
        (flycheck-emacs-lisp-relint--running t)
        flycheck-emacs-lisp-relint--current-errors)
    (with-temp-buffer
      (relint--scan-buffer source-buf (current-buffer) t)
      (funcall callback 'finished flycheck-emacs-lisp-relint--current-errors))))




;;; Checker definition

(flycheck-define-generic-checker 'emacs-lisp-relint
  "Report errors detected by `relint'."
  :start #'flycheck-relint--start
  :modes '(emacs-lisp-mode))



;;;###autoload
(defun flycheck-relint-setup ()
  "Setup flycheck-package.
Add `emacs-lisp-relint' to `flycheck-checkers' and set up the checker chain."
  (interactive)
  (add-to-list 'flycheck-checkers 'emacs-lisp-relint t)
  (flycheck-add-next-checker 'emacs-lisp-checkdoc 'emacs-lisp-relint t)
  (when (flycheck-valid-checker-p 'emacs-lisp-package)
    (flycheck-add-next-checker 'emacs-lisp-package 'emacs-lisp-relint t)))


(provide 'flycheck-relint)
;;; flycheck-relint.el ends here
