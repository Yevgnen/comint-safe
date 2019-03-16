;;; comint-truncate.el ---  -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2017 Yevgnen Koh
;;
;; Author: Yevgnen Koh <wherejoystarts@gmail.com>
;; Version: 1.0.0
;; Keywords: comint truncate
;;
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.
;;
;;; Commentary:
;;
;; Truncate long lines in comint buffers.
;;

;;; Code:

(require 'comint)

(defgroup comint-truncate nil
  "Truncate long lines in comint buffers."
  :group 'comint)

(defcustom comint-truncate-display
  " #.#.#.# "
  "String to represent the truncated contents"
  :type 'string)

(defcustom comint-truncate-commands
  '(comint-send-input)
  "List of command after which trigger truncates."
  :type '(repeat symbol))

(defvar comint-truncate--chunks nil)
(make-variable-buffer-local 'comint-truncate--chunks)

(defun comint-truncate-filter-long-lines (str)
  (if (memq last-command comint-truncate-commands)
      (progn
        (push str comint-truncate--chunks)
        (if (not (string-match comint-prompt-regexp str))
            ""
          (let* ((out (mapconcat #'identity (nreverse comint-truncate--chunks) ""))
                 (split-str (split-string out "\n"))
                 (max-len (* 10 (window-width)))
                 (disp-left (round (* (/ 1.0 3) (window-width))))
                 (disp-right disp-left)
                 (truncated (mapconcat
                             (lambda (x)
                               (if (> (length x) max-len)
                                   (concat (substring x 0 disp-left) comint-truncate-display (substring x (- disp-right)))
                                 x))
                             split-str "\n")))
            (setq comint-truncate--chunks nil)
            truncated)))
    str))

(defun comint-truncate-highlight-display ()
  (font-lock-add-keywords
   nil `((,(regexp-quote comint-truncate-display) 0 'error prepend))))

;;;###autoload
(define-minor-mode comint-truncate-mode
  "Truncate mode for comint buffers."
  nil "" nil
  (if comint-truncate-mode
      (progn
        (add-hook 'comint-preoutput-filter-functions #'comint-truncate-filter-long-lines nil t)
        (add-hook 'comint-mode-hook #'comint-truncate-highlight-display))
    (progn
      (remove-hook 'comint-preoutput-filter-functions #'comint-truncate-filter-long-lines t)
      (remove-hook 'comint-mode-hook #'comint-truncate-highlight-display))))

(provide 'comint-truncate)

;;; comint-truncate.el ends here
