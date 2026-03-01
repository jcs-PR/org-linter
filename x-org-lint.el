;;; x-org-lint.el --- Run org-lint on Org files  -*- lexical-binding: t; -*-

;; Copyright (C) 2026  Shen, Jen-Chieh

;; Author: Shen, Jen-Chieh <jcs090218@gmail.com>
;; Maintainer: Shen, Jen-Chieh <jcs090218@gmail.com>
;; URL: https://github.com/emacs-eine/x-org-lint
;; Version: 1.0.0
;; Package-Requires: ((emacs "26.1")
;;                    (commander "0.7.0")
;;                    (ansi "0.4.1"))
;; Keywords: convenience

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
;; along with this program. If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:
;;
;; Run org-lint on Org files.
;;

;;; Code:

(require 'org)
(require 'org-lint)
(require 'ansi)

(defun x-org-lint--print-error (file result)
  "Print the error RESULT from FILE."
  (let* ((data (cl-second result))
         (filename (file-name-nondirectory file))
         (line (elt data 0))
         (text (elt data 2))
         (msg (concat filename ":" line ": " text)))
    (message "%s" msg)))

(defun x-org-lint--file (file)
  "Run `org-lint' on FILE."
  (message "`%s` with org-lint" (ansi-green file))
  (with-temp-buffer
    (insert-file-contents file)
    (org-mode)
    (if-let* ((results (org-lint)))
        (mapc (lambda (result)
                (x-org-lint--print-error file result))
              results)
      (message (ansi-cyan "No issues found")))))

;;;###autoload
(defun x-org-lint-run (&rest args)
  "Run org-lint on ARGS."
  (message "")
  (if-let* ((files (mapcar #'expand-file-name args))
            (files (cl-remove-if-not #'file-exists-p files)))
      (mapcar #'x-org-lint--file files)
    (message (ansi-cyan "(No file linted)"))))

(provide 'x-org-lint)
;;; x-org-lint.el ends here
