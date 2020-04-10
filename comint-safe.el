;;; comint-safe.el ---  -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2017 Yevgnen Koh
;;
;; Author: Yevgnen Koh <wherejoystarts@gmail.com>
;; Version: 1.0.0
;; Keywords: comint
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
;; Help displaying large texts in comint buffers.
;;

;;; Code:

(require 'comint)

(defgroup comint-safe nil
  "Help displaying large texts in comint buffers."
  :group 'comint)

(defcustom comint-safe-commands
  '(comint-send-input)
  "List of command after which trigger safe displaying."
  :type '(repeat symbol))

(defcustom comint-safe-threshold
  10000
  "Maximum string length for triggering safe displaying."
  :type 'integer)

(defcustom comint-safe-buffer-setup-function
  #'comint-safe-buffer-setup-default
  "Setup function for *comint-safe* buffer."
  :type 'function)

(defcustom comint-safe-buffer-name
  "*comint-safe*"
  "Buffer name for comint-safe."
  :type 'string)

(defvar comint-safe--chunks nil)
(defvar comint-safe--size 0)
(make-variable-buffer-local 'comint-safe--chunks)
(make-variable-buffer-local 'comint-safe--size)

(defun comint-safe-cache-string (str)
  (push str comint-safe--chunks)
  (incf comint-safe--size (length str)))

(defun comint-safe-buffer-setup-default ()
  (fundamental-mode)
  (view-mode 1)
  (show-paren-mode -1)
  (if (featurep 'anzu) (anzu-mode -1))
  (if (featurep 'pangu-spacing) (pangu-spacing-mode -1))
  (buffer-disable-undo))

(defun comint-safe-filter-long-lines (str)
  (if (memq last-command comint-safe-commands)
      (if (not (string-match comint-prompt-regexp str))
          (progn (comint-safe-cache-string str) "")
        ;; The prompt may come with the last trunk.
        (let* ((last-lines (split-string str "\n"))
               (prompt (car (last last-lines)))
               (lines (append (nreverse comint-safe--chunks)
                              `(,(mapconcat #'identity (subseq last-lines 0 -1) "\n")))))
          (if (> comint-safe--size comint-safe-threshold)
              (progn
                (display-buffer
                 (with-current-buffer
                     (get-buffer-create comint-safe-buffer-name)
                   (if view-mode (view-mode -1))
                   (erase-buffer)
                   (dolist (line lines)
                     (insert line))
                   (goto-char (point-min))
                   (funcall comint-safe-buffer-setup-function)
                   (current-buffer)))
                (setq str (concat "\n" prompt)))
            (setq str (concat (mapconcat #'identity lines "") "\n" prompt)))
          (setq comint-safe--chunks nil
                comint-safe--size 0)
          str))
    (setq comint-safe--chunks nil
          comint-safe--size 0)
    str))

;;;###autoload
(define-minor-mode comint-safe-mode
  "Safe mode for comint buffers."
  nil "" nil
  (if comint-safe-mode
      (add-hook 'comint-preoutput-filter-functions #'comint-safe-filter-long-lines nil t)
    (remove-hook 'comint-preoutput-filter-functions #'comint-safe-filter-long-lines t)))

(provide 'comint-safe)

;;; comint-safe.el ends here
