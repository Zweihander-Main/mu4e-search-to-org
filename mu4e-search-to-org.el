;;; mu4e-search-to-org.el --- WIP-*-lexical-binding:t-*-

;; Copyright (C) 2021, Zweihänder <zweidev@zweihander.me>
;;
;; Author: Zweihänder
;; Keywords: org-mode
;; Homepage: https://github.com/Zweihander-Main/mu4e-search-to-org
;; Version: 0.0.1

;; This file is not part of GNU Emacs.

;;; License:

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU Affero General Public License as published
;; by the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU Affero General Public License for more details.
;;
;; You should have received a copy of the GNU Affero General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:
;;
;; WIP
;;
;;; Code:

(require 'mu4e)
(require 'org)
(require 'org-mu4e)
(require 'org-capture)

(defgroup mu4e-search-to-org nil
  "Customization for 'mu4e-search-to-org' package."
  :group 'mu4e
  :prefix "mu4e-search-to-org-")

(defconst mu4e-search-to-org--const-for-state "MU4ESEARCHTOORG_")

(defvar mu4e-search-to-org--state-name "mu4e-memo-to-inbox"
  "Name of state to save for memo to inbox save/restore functionality.")

(defvar mu4e-search-to-org-file-for-output "inbox.org"
  "Org file to output to.")

(defvar mu4e-search-to-org-context "")

(defvar mu4e-search-to-org-search-bookmark "flag:unread"
  "Org file to output to.")

(defun mu4e-search-to-org--save-and-restore-state (ref-name &optional action)
  "Save point, buffer, and mark into variable defined by REF-NAME.

ACTION values:
Write: Will write values.
Clear: Will set to nil.
Read: Will return values in (point,buffer,mark) form.
Restore: Will restore state and clear variable.

For read, can get data using:
(setf (values point buffer mark)
    (mu4e-search-to-org--save-and-restore-state nil 'read'))

Will clear data on non-write if old buffer doesn't exist anymore.

Intended for short term usage - not designed to survive restart."
  (let* ((constant mu4e-search-to-org--const-for-state)
         (var-string (concat constant ref-name))
         (stored-data (ignore-errors (symbol-value (intern var-string))))
         (old-point (gensym "old-point"))
         (old-buff) (gensym "old-buff")
         (old-mark) (gensym "old-mark"))
    (when stored-data
      (setq old-point (nth 0 stored-data))
      (setq old-buff (nth 1 stored-data))
      (setq old-mark (nth 2 stored-data)))
    (cond ((string= action "write")
           (let ((old-point (point))
                 (old-buff (current-buffer))
                 (old-mark (save-mark-and-excursion--save)))
             (set (intern var-string) (list old-point old-buff old-mark))))
          ((or
            (string= action "clear")
            (not (buffer-live-p old-buff))
            (not stored-data))
           (set (intern var-string) nil))
          ((string= action "read") stored-data)
          (t (progn (unless (eq (current-buffer) old-buff)
                      (switch-to-buffer old-buff))
                    (goto-char old-point)
                    (save-mark-and-excursion--restore old-mark)
                    (set (intern var-string) nil))))))

(defun mu4e-search-to-org-script-mode (&optional disable)
  "Will enable script mode unless DISABLE is non-nil."
  (if disable
      (progn
        (setq mu4e-split-view 'vertical)
        (mu4e-search-to-org--save-and-restore-state
         mu4e-search-to-org--state-name
         "clear"))
    (setq mu4e-split-view nil)))

(defun mu4e-search-to-org--buffer-manage (buffer &optional switch)
  "BUFFER can be 'view' or 'headers'.
  Returns true if current buffer is specified buffer.
  Will switch to that buffer is SWITCH is non-nil."
  (let* ((target-buffer (cond ((string= buffer "view")
                               (mu4e-get-view-buffer))
                              ((string= buffer "headers")
                               (mu4e-get-headers-buffer))
                              (t
                               (progn
                                 (user-error
                                  "BUFFER should be 'view' or 'headers'")
                                 (current-buffer)))))
         (is-buffer (eq (current-buffer) target-buffer)))
    (when (and target-buffer (not is-buffer) switch)
      (switch-to-buffer target-buffer))
    (setq is-buffer (eq (current-buffer) target-buffer))))

(defun mu4e-search-to-org--view-message-and-process ()
  "Process the message, add to the org inbox file, and restore the original
  state if it's the last message."
  (when (and ;; Only execute if in view and we're processing the inbox
         (mu4e-search-to-org--save-and-restore-state
          mu4e-search-to-org--state-name
          "read")
         (mu4e-search-to-org--buffer-manage "view"))
    (princ (mu4e-message-at-point t))
    (let ((msg (mu4e-message-at-point t)))
      (when msg
        (let* ((body (string-trim (mu4e-body-text msg)))
               (subject (string-trim (mu4e-message-field msg :subject)))
               (body-to-set (cond
                             ((string= "" body) nil)
                             ((string= subject body) nil)
                             (t (string-trim body))))
               (header-to-set (cond
                               ((string= "" subject) (car (split-string
                                                           body
                                                           "\\n"
                                                           nil
                                                           " ")))
                               (t (string-trim subject)))))
          (org-capture nil "i")
          (funcall-interactively 'org-edit-headline header-to-set)
          (when body-to-set
            (insert "\n")
            (insert body-to-set))
          (org-capture-finalize)
          (mu4e-search-to-org--buffer-manage "headers" t)
          (mu4e-mark-set 'read)
          (let ((next (mu4e-headers-next)))
            (if next
                (mu4e-headers-view-message)
              (when (buffer-live-p (mu4e-get-view-buffer))
                (kill-buffer (mu4e-get-view-buffer)))
              (mu4e-mark-execute-all t) ;; Execute all marks, there should be at least one from this call
              (mu4e-search-to-org--save-and-restore-state
               mu4e-search-to-org--state-name
               "restore") ;; Done with memo-to-inbox
              (mu4e-search-to-org-script-mode t))))))))

(defun mu4e-search-to-org--process-found-headers ()
  "Hooked to call after a search for memos is completed.
  Will setup the mu4e-view which is then hooked into the process function."
  (when (and ;; Only execute if in headers view and the previous function was called
         (mu4e-search-to-org--save-and-restore-state
          mu4e-search-to-org--state-name
          "read")
         (mu4e-search-to-org--buffer-manage "headers" t))
    (when (buffer-live-p (mu4e-get-view-buffer))
      (kill-buffer (mu4e-get-view-buffer)))
    (if (mu4e-message-at-point t)
        (progn (append-to-file "\n" nil mu4e-search-to-org-file-for-output)
               (mu4e-headers-view-message))
      (mu4e-search-to-org--save-and-restore-state
       mu4e-search-to-org--state-name
       "restore") ;; no messages, reset it right out the gate
      (mu4e-search-to-org-script-mode t))))

;;;###autoload
(defun mu4e-search-to-org-search ()
  "Pull in emails to memo folder and convert them to org headings in the inbox.
  Use the email body for content and mark the emails as read. This is the first
  part of the function which calls the search and saves the location to restore
  after the search is completed."
  (interactive)
  (mu4e-context-switch nil mu4e-search-to-org-context)
  (mu4e-search-to-org--save-and-restore-state
   mu4e-search-to-org--state-name
   "write")
  (mu4e-search-to-org-script-mode)
  (mu4e-headers-search-bookmark mu4e-search-to-org-search-bookmark))

;; Hooks
;; TODO: All of this needs to change -- this is only done due to poor async
(add-hook 'mu4e-view-mode-hook 'mu4e-search-to-org--view-message-and-process)
(add-hook 'mu4e-headers-found-hook 'mu4e-search-to-org--process-found-headers)
(add-hook 'mu4e-index-updated-hook 'mu4e-search-to-org-search)

;;; +memo-to-inbox.el ends here
(provide 'mu4e-search-to-org)

;; Local Variables:
;; coding: utf-8
;; End:

;;; mu4e-search-to-org.el ends here
