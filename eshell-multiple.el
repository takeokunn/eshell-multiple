;;; eshell-multiple.el --- summary -*- lexical-binding: t -*-

;; Author: takeo obara
;; Maintainer: takeoo obara
;; Version: 1.0.0
;; Package-Requires: ()
;; Homepage: https://github.com/takeokunn/eshell-multiple
;; Keywords: eshell


;; This file is not part of GNU Emacs

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; For a full copy of the GNU General Public License
;; see <http://www.gnu.org/licenses/>.


;;; Commentary:

;; commentary

;;; Code:

;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Require ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'eshell)
(require 'cl-lib)
(require 'subr-x)
(require 'seq)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Customize ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defgroup eshell-multiple nil
  "Multi eshell manager."
  :group 'eshell-multiple)

(defcustom eshell-multiple-dedicated-window-height 14
  "The height of `eshell' dedicated window."
  :type 'integer
  :group 'eshell-multiple)

(defvar eshell-multiple-buffer-list nil
  "The list of non-dedicated eshell buffers.")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Hook ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(add-hook 'kill-buffer-hook 'eshell-multiple-kill-buffer-hook)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Utilise Functions ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun eshell-multiple-kill-buffer-hook ()
  "Function that hook `kill-buffer-hook'."
  (when (eq major-mode 'eshell-mode)
    (let ((killed-buffer (current-buffer)))
      (setq eshell-multiple-buffer-list
            (delq killed-buffer eshell-multiple-buffer-list)))))

(defun eshell-multiple-get-buffer-index ()
  (let ((eshell-buffer-index-list (eshell-multiple-get-buffer-index-list))
        (eshell-buffer-index-counter 1))
    (if eshell-buffer-index-list
        (progn
          (dolist (buffer-index eshell-buffer-index-list)
            (if (equal buffer-index eshell-buffer-index-counter)
                (setq eshell-buffer-index-counter (+ 1 eshell-buffer-index-counter))
              (return eshell-buffer-index-counter)))
          eshell-buffer-index-counter)
      1)))

(defun eshell-multiple-get-buffer-names ()
  (let (eshell-buffer-names)
    (dolist (frame (frame-list))
      (dolist (buffer (buffer-list frame))
        (with-current-buffer buffer
          (if (eq major-mode 'eshell-mode)
              (add-to-list 'eshell-buffer-names (buffer-name buffer))))))
    eshell-buffer-names))

(defun eshell-multiple-get-buffer-index-list ()
  (let ((eshell-buffer-names (eshell-multiple-get-buffer-names)))
    (if eshell-buffer-names
        (let* ((eshell-buffer-index-strings
                (seq-filter (function
                             (lambda (buffer-index)
                               (and (stringp buffer-index)
                                    (not (equal 0 (string-to-number buffer-index))))))
                            (mapcar (function
                                     (lambda (buffer-name)
                                       (if (integerp (string-match "\\*eshell\\*\<\\([0-9]+\\)\>" buffer-name))
                                           (subseq buffer-name (match-beginning 1) (match-end 1))
                                         nil)))
                                    eshell-buffer-names)))
               (eshell-buffer-index-list (sort (seq-map 'string-to-number eshell-buffer-index-strings) '<)))
          eshell-buffer-index-list)
      nil)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Interactive Functions ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun eshell-multiple-toggle (&optional arg)
  "Toggle eshell."
  (interactive "p")
  (if (equal major-mode 'eshell-mode)
      ;; toggle off
      (while (equal major-mode 'eshell-mode)
        (switch-to-prev-buffer))
    ;; toggle on
    (if (eq arg 4)
        ;; open in current dir
        (let* ((dir default-directory)
               (existing-buffer
                (catch 'found
                  (dolist (eshell-multiple-buffer eshell-multiple-buffer-list)
                    (with-current-buffer eshell-multiple-buffer
                      (when (equal dir default-directory)
                        (throw 'found eshell-multiple-buffer)))))))
          ;; found the buffer with the same dir
          ;; or create a new one
          (if existing-buffer
              (switch-to-buffer existing-buffer)
            (message "No eshell buffer with current dir found, creating a new one.")
            (switch-to-buffer (car (last (eshell-multiple-new))))
            (eshell/cd dir)))
      ;; simply open
      (eshell-multiple-next))))

(defun eshell-multiple-new ()
  "Create new eshell buffer."
  (interactive)
  (setq eshell-multiple-buffer-list (nconc eshell-multiple-buffer-list (list (eshell (eshell-multiple-get-buffer-index))))))

(defun eshell-multiple-next ()
  "Select next eshell buffer.
Create new one if no eshell buffer exists."
  (interactive)
  (if (or (not eshell-multiple-buffer-list) (equal (length eshell-multiple-buffer-list) 0))
      (eshell-multiple-new)
    (let* ((current-buffer-index (cl-position (current-buffer) eshell-multiple-buffer-list))
           (switch-index (if current-buffer-index
                             (if (>= current-buffer-index (- (length eshell-multiple-buffer-list) 1))
                                 0
                               (+ 1 current-buffer-index))
                           0)))
      (switch-to-buffer (nth switch-index eshell-multiple-buffer-list)))))

(defun eshell-multiple-prev ()
  "Select previous eshell buffer. Create new one if no eshell buffer exists."
  (interactive)
  (if (or (not eshell-multiple-buffer-list) (equal (length eshell-multiple-buffer-list) 0))
      (eshell-multiple-new)
    (let* ((current-buffer-index (cl-position (current-buffer) eshell-multiple-buffer-list))
           (switch-index (if current-buffer-index
                             (if (<= current-buffer-index 0)
                                 (- (length eshell-multiple-buffer-list) 1)
                               (- current-buffer-index 1))
                           (- (length eshell-multiple-buffer-list) 1))))
      (switch-to-buffer (nth switch-index eshell-multiple-buffer-list)))))

(defun eshell-multiple-clear-buffer ()
  "Clear eshell buffer."
  (interactive)
  (let ((inhibit-read-only t))
    (erase-buffer)
    (eshell-send-input)))

(defun eshell-multiple-switch-buffer ()
  "Switch to another eshell buffer."
  (interactive)
  (let ((live-eshell-buffer-list (cl-remove-if-not #'buffer-live-p eshell-multiple-buffer-list)))
    (cond ((= 0 (length live-eshell-buffer-list))
           (eshell-multiple-new)
           (message "No eshell buffer yet, create a new one."))
          ((= 1 (length live-eshell-buffer-list)) ; only one eshell buffer, just switch to it
           (switch-to-buffer (nth 0 live-eshell-buffer-list)))
          (t
           (let* ((completion-extra-properties '(:annotation-function eshell-multiple-switch-buffer--annotate))
                  (buffer-alist (mapcar (lambda (buffer) `(,(buffer-name buffer) . ,buffer))
                                        live-eshell-buffer-list))
                  (pwd default-directory)
                  (preselect))
             ;; find most suitable preselect buffer
             (dolist (buffer live-eshell-buffer-list)
               (with-current-buffer buffer
                 (when (and
                        (or (not preselect) (< (length preselect) (length default-directory)))
                        (file-in-directory-p pwd default-directory))
                   (setq preselect (propertize default-directory :buffer-name (buffer-name buffer))))))
             (let ((result-buffer (completing-read "Switch to eshell buffer: " buffer-alist nil t nil nil
                                                   (get-text-property 0 :buffer-name (or preselect "")))))
               (switch-to-buffer (alist-get result-buffer buffer-alist nil nil #'equal))))))))

(defun eshell-multiple-switch-buffer--annotate (candidate)
  (let* ((buffer-alist
          (mapcar (lambda (buffer) `(,(buffer-name buffer) . ,buffer)) eshell-multiple-buffer-list))
         (candidate-buffer (alist-get candidate buffer-alist nil nil #'equal)))
    (with-current-buffer candidate-buffer
      ;; display the last command of eshell buffer
      (format "  <%s> %s" (eshell-get-history 0) (if eshell-current-command "(Running)" "")))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; eshell dedicated window ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar eshell-multiple-dedicated-window nil
  "The dedicated `eshell' window.")

(defvar eshell-multiple-dedicated-buffer nil
  "The dedicated `eshell' buffer.")

(defun eshell-multiple-current-window-take-height (&optional window)
  "Return the height the `window' takes up.
Not the value of `window-height', it returns usable rows available for WINDOW.
If `window' is nil, get current window."
  (let ((edges (window-edges window)))
    (- (nth 3 edges) (nth 1 edges))))

(defun eshell-multiple-dedicated-exist-p ()
  (and (eshell-multiple-buffer-exist-p eshell-multiple-dedicated-buffer)
       (eshell-multiple-window-exist-p eshell-multiple-dedicated-window)))

(defun eshell-multiple-window-exist-p (window)
  "Return `non-nil' if WINDOW exist.
Otherwise return nil."
  (and window (window-live-p window)))

(defun eshell-multiple-buffer-exist-p (buffer)
  "Return `non-nil' if `BUFFER' exist.
Otherwise return nil."
  (and buffer (buffer-live-p buffer)))

(defun eshell-multiple-dedicated-open ()
  "Open dedicated `eshell' window."
  (interactive)
  (if (eshell-multiple-buffer-exist-p eshell-multiple-dedicated-buffer)
      (if (eshell-multiple-window-exist-p eshell-multiple-dedicated-window)
          (eshell-multiple-dedicated-select-window)
        (eshell-multiple-dedicated-pop-window))
    (eshell-multiple-dedicated-create-window)))

(defun eshell-multiple-dedicated-close ()
  "Close dedicated `eshell' window."
  (interactive)
  (if (eshell-multiple-dedicated-exist-p)
      (let ((current-window (selected-window)))
        ;; Remember height.
        (eshell-multiple-dedicated-select-window)
        (delete-window eshell-multiple-dedicated-window)
        (if (eshell-multiple-window-exist-p current-window)
            (select-window current-window)))
    (message "`ESHELL DEDICATED' window is not exist.")))

(defun eshell-multiple-dedicated-toggle ()
  "Toggle dedicated `eshell' window."
  (interactive)
  (if (eshell-multiple-dedicated-exist-p)
      (eshell-multiple-dedicated-close)
    (eshell-multiple-dedicated-open)))

(defun eshell-multiple-dedicated-select-window ()
  "Select eshell dedicated window."
  (select-window eshell-multiple-dedicated-window)
  (set-window-dedicated-p (selected-window) t))

(defun eshell-multiple-dedicated-pop-window ()
  "Pop eshell dedicated window if it exists."
  (setq eshell-multiple-dedicated-window (display-buffer (car (eshell-multiple-get-buffer-names)) `(display-buffer-in-side-window (side . bottom) (window-height . ,eshell-multiple-dedicated-window-height))))
  (select-window eshell-multiple-dedicated-window)
  (set-window-buffer eshell-multiple-dedicated-window eshell-multiple-dedicated-buffer)
  (set-window-dedicated-p (selected-window) t))

(defun eshell-multiple-dedicated-create-window ()
  "Create eshell dedicated window if it not existing."
  (eshell)
  (setq eshell-multiple-dedicated-buffer (current-buffer))
  (previous-buffer)
  (eshell-multiple-dedicated-pop-window))

(defun eshell-multiple-dedicated-split-window ()
  "Split dedicated window at bottom of frame."
  ;; Select bottom window of frame.
  (ignore-errors
    (dotimes (i 50)
      (windmove-down)))
  ;; Split with dedicated window height.
  (split-window (selected-window) (- (eshell-multiple-current-window-take-height) eshell-multiple-dedicated-window-height))
  (other-window 1)
  (setq eshell-multiple-dedicated-window (selected-window)))

(defun eshell-multiple-dedicated-create-buffer ()
  "Create eshell dedicated buffer."
  (eshell)
  (setq header-line-format nil)
  (setq eshell-multiple-dedicated-buffer (current-buffer)))

(defadvice delete-other-windows (around eshell-multiple-delete-other-window-advice activate)
  "This is advice to make `eshell' avoid dedicated window deleted.
Dedicated window can't deleted by command `delete-other-windows'."
  (unless (eq (selected-window) eshell-multiple-dedicated-window)
    (let ((eshell-multiple-dedicated-active-p (eshell-multiple-window-exist-p eshell-multiple-dedicated-window)))
      (if eshell-multiple-dedicated-active-p
          (let ((current-window (selected-window)))
            (cl-dolist (win (window-list))
              (when (and (window-live-p win)
                         (not (eq current-window win))
                         (not (window-dedicated-p win)))
                (delete-window win))))
        ad-do-it))))

(defadvice other-window (after eshell-multiple-dedicated-other-window-advice)
  "Default, can use `other-window' select window in cyclic ordering of windows.
But sometimes we don't want to select `sr-speedbar' window,
but use `other-window' and just make `eshell' dedicated
window as a viewable sidebar.
This advice can make `other-window' skip `eshell' dedicated window."
  (let ((count (or (ad-get-arg 0) 1)))
    (when (and (eshell-multiple-window-exist-p eshell-multiple-dedicated-window)
               (eq eshell-multiple-dedicated-window (selected-window)))
      (other-window count))))

(provide 'eshell-multiple)

;;; eshell-multiple.el ends here
