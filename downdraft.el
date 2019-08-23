;;; downdraft.el --- Places a time limit on writing drafts

;; Copyright (C) 2019 Ruin0x11

;; Author: Ruin0x11 <ipickering2@gmail.com>
;; URL: https://github.com/Ruin0x11/downdraft.el
;; Created: 22th August 2019
;; Version: 0.0.1
;; Keywords: tools
;; Package-Requires: ((emacs "24"))

;; This file is NOT part of GNU Emacs.

;;; License:

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING. If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:

;; A timer for draft writing, to force you to write first drafts of a
;; text. If time runs out, the created buffer is deleted.
;;
;; It is set up so time will not be reset if previous parts of the
;; text are deleted, until the maximum amount of characters that have
;; been typed so far is reached again. This is to discourage premature
;; editing and force you to write new text.
;;
;; This code was adapted from pomodoro.el by David Kerschner. The idea
;; was taken from "The Most Dangerous Writing Prompt Generator"
;; (https://www.squibler.io/writing-prompt-generator).

;;; Usage:

;; Add these lines to your init script.
;;
;; (require 'downdraft)
;; (downdraft-add-to-mode-line)
;;
;; Then, call either `downdraft-time' or `downdraft-word-count' to
;; start a new draft.

;;; Code:

(defgroup downdraft nil
  "Timer for first draft writing."
  :prefix "downdraft-"
  :group 'tools)

(defcustom downdraft-time-limit-seconds 5
  "Maximum seconds between new text before the buffer is forcibly
deleted."
  :group 'downdraft
  :type 'integer)

(defcustom downdraft-failure-action 'kill
  "The action to take if the drafting time limit expires.

Possible options:
'kill: Kill all text to the kill ring.
'delete: Delete all text without modifying the kill ring, making
         it unrecoverable."
  :group 'downdraft
  :type 'symbol
  :options '(kill delete))

(defcustom downdraft-default-goal-time 3
  "Default session length in minutes."
  :group 'downdraft
  :type 'integer)

(defcustom downdraft-default-goal-word-count 150
  "Default target word count."
  :group 'downdraft
  :type 'integer)

(defcustom downdraft-default-goal 'time
  "Default goal type."
  :group 'downdraft
  :type 'symbol
  :options '(time word-count))

(defcustom downdraft-time-format "%.2m:%.2s"
  "Time string to display in mode line for countdowns.
Formatted with `format-seconds'."
  :group 'downdraft
  :type 'string)

(defvar downdraft-start-hook nil
  "Hook run when downdraft starts.")

(defvar downdraft-stop-hook nil
  "Hook run when downdraft stops.")

(defvar downdraft-timer nil)
(defvar downdraft-mode-line-string "")
(defvar downdraft-end-time) ; the data type should be time instead of integer
(defvar downdraft-buffer)
(defvar downdraft-changed nil)
(make-variable-buffer-local 'downdraft-changed)
(defvar downdraft-last-char-count)
(make-variable-buffer-local 'downdraft-initial-count)
(defvar downdraft-last-change)
(make-variable-buffer-local 'downdraft-last-change)
(defvar downdraft-goal)
(make-variable-buffer-local 'downdraft-goal)
(defvar downdraft-goal-amount)
(make-variable-buffer-local 'downdraft-goal-amount)

(defun downdraft-set-end-time (seconds)
  "Set how long the downdraft timer should run"
  (setq downdraft-end-time (time-add (current-time) (list 0 seconds 0))))

(defun downdraft-calc-seconds (start end)
  (round (float-time (time-subtract end start))))

(defun downdraft-change (beg end l)
  (setq downdraft-changed t))

(defun downdraft-validate ()
  (let ((count (point-max)))
    (when (> count downdraft-last-char-count)
      (setq downdraft-last-change (current-time)))
    (setq downdraft-last-char-count
          (max downdraft-last-char-count count)) ))

(defun downdraft-kill-save-buffer (&optional arg)
  "Save the current buffer (if needed) and then kill it.
 Also, delete its windows according to `kill-save-buffer-delete-windows'.
 A prefix argument ARG reverses this behavior."
  (interactive "P")
  (let ((del (not arg))
        (save-silently t))
    (when (and (buffer-file-name) (not (file-directory-p (buffer-file-name))))
      (save-buffer))
    (let ((buf (current-buffer)))
      (when del (delete-windows-on buf))
      (kill-buffer buf))))

(defun downdraft-fail ()
  (when (buffer-live-p downdraft-buffer)
    (with-current-buffer downdraft-buffer
      (if (eq downdraft-failure-action 'kill)
          (kill-region (point-min) (point-max))
        (delete-region (point-min) (point-max)))
      (if (local-variable-p 'buffer-undo-list)
          (setq buffer-undo-list nil))
      (if (local-variable-p 'buffer-undo-tree)
          (setq buffer-undo-tree nil))
      (downdraft-stop)
      (downdraft-kill-save-buffer t)
      (message "Time expired."))))

(defun downdraft-finish ()
  (downdraft-stop)
  (message "Drafting finished!"))

(defun downdraft-clear-overlay ()
  (remove-overlays nil nil 'downdraft-marker t))

(defun downdraft-refresh-overlay ()
  (downdraft-clear-overlay)
  (let ((ov (make-overlay downdraft-last-char-count (1- downdraft-last-char-count) downdraft-buffer)))
    (overlay-put ov 'downdraft-marker t)
    (overlay-put ov 'face 'whitespace-trailing)))

(defun downdraft-calc-progress ()
  (if (eq downdraft-goal 'word-count)
      (count-words (point-min) (point-max))
    (downdraft-calc-seconds (current-time) downdraft-end-time)))

(defun downdraft-format-progress (progress)
  (if (eq downdraft-goal 'word-count)
      (format "%s/%s words" progress downdraft-goal-amount)
    (format-seconds downdraft-time-format progress)))

(defun downdraft-goal-reached (progress)
  (if (eq downdraft-goal 'word-count)
      (>= progress downdraft-goal-amount)
    (<= progress 0)))

(defun downdraft-do-tick()
  (let ((progress (downdraft-calc-progress))
        (remaining (1+ (- downdraft-time-limit-seconds (downdraft-calc-seconds downdraft-last-change (current-time))))))
    (downdraft-validate)
    (downdraft-refresh-overlay)
    (setq downdraft-mode-line-string
          (format "%s|%s "
                  (downdraft-format-progress progress)
                  (format-seconds downdraft-time-format remaining)))
    (force-mode-line-update)
    (cond
     ((<= remaining 0) (downdraft-fail))
     ((downdraft-goal-reached progress) (downdraft-finish)))))

(defun downdraft-tick ()
  (if (not (buffer-live-p downdraft-buffer))
      (downdraft-stop)
    (downdraft-do-tick)))

(defun downdraft-start (goal amount)
  (let* ((file (make-temp-file "downdraft")))
    (find-file file)
    (setq downdraft-goal goal)
    (setq downdraft-goal-amount amount)
    (setq downdraft-buffer (current-buffer))
    (setq downdraft-time-elapsed 0)
    (when downdraft-timer
      (cancel-timer downdraft-timer))
    (setq downdraft-last-char-count 0)
    (run-hooks 'downdraft-start-hook)
    (let ((end-time (if (eq goal 'time) (* 60 amount) 0)))
      (downdraft-set-end-time end-time))
    (setq downdraft-last-change (current-time))
    (downdraft-clear-overlay)
    (add-to-list 'after-change-functions 'downdraft-change)
    (setq downdraft-timer (run-with-timer 0 1 'downdraft-tick))))

(defun downdraft-stop ()
  (interactive)
  (cancel-timer downdraft-timer)
  (downdraft-clear-overlay)
  (setq downdraft-mode-line-string "")
  (setq downdraft-buffer nil)
  (force-mode-line-update)
  (run-hooks 'downdraft-stop-hook))

;;;###autoload
(defun downdraft-time (&optional arg)
  "Starts a new writing draft with a session time.
With prefix argument, prompt for the session time."
  (interactive "P")
  (downdraft-start 'time
                   (or (and arg (read-number "Time (in minutes):"
                                             downdraft-default-goal-time))
                       downdraft-default-goal-time)))

;;;###autoload
(defun downdraft-word-count (&optional arg)
  "Starts a new writing draft with a target word count.
With prefix argument, prompt for the target word count."
  (interactive "P")
  (downdraft-start 'word-count
                   (or (and arg (read-number "Word count:"
                                             downdraft-default-goal-word-count))
                       downdraft-default-goal-word-count)))

(defun downdraft-add-to-mode-line ()
  (setq-default mode-line-format
                (cons '(downdraft-mode-line-string downdraft-mode-line-string)
                      mode-line-format)))

(provide 'downdraft)
;;; downdraft.el ends here
