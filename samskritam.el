;;; samskritam.el --- Show samskrit word definitions and translations -*- lexical-binding: t -*-

;; Copyright (C) 2022 Krishna Thapa

;; Description: Samskrit definitions and translations
;; Author: Krishna Thapa <thapakrish@gmail.com>
;; URL: https://github.com/thapakrish/samskritam
;; Version: 0.1.0
;; Package-Requires: ((emacs "28.1") (google-translate "0.12.0") (popper "0.4.6"))
;; Keywords: samskrit, sanskrit, संस्कृत, dictionary, devanagari, translation, convenience, language

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
;;
;; This package uses:
;; Google Translate to translate to/from Sanskrit
;; Sends request to https://ambuda.org/ for dictionary definition of words
;; Add Vedic characters to devanagari-inscript
;;
;;; Code:


(require 'url-parse)
(require 'url-http)
(require 'google-translate)
(require 'google-translate-smooth-ui)
(require 'popper)
(require 'transient)

(defvar samskritam-mode nil
  "Toggle `samskritam-mode'.")


(defgroup samskritam nil
  "Provide functions for word definitions and translations."
  :group 'convenience)

(defcustom samskritam-keymap-prefix "C-c C-s"
  "The prefix for `samskritam-mode' key bindings."
  :type 'string
  :group 'samskritam)

(defcustom samskritam-mode-line '(:eval (propertize " SKT" 'face 'mode-line-emphasis))
  "String to show in the mode-line of Samskritam.
Setting this tonil removes from the mode-line."
  :group 'samskritam
  :type '(choice (const :tag "Off" nil)))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;; Dictionary ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defvar samskritam-ambuda-dict-choices '(
			      ("Apte" . "apte")
			      ("Apte-Kosh" . "apte-sh")
			      ("MW" . "mw")
			      ("Shabdasagara" . "shabdasagara")
			      ("Vacaspatyam" . "vacaspatyam")
			      ("Shabdarthakausubha" . "shabdakalpadruma")
			      ("Amarakosha" . "amara"))
  "Dictionaries in Ambuda.org to crawl from.")

(defun samskritam--fetch-and-display-definition (word dict-name)
  "Fetch definition for WORD from DICT-NAME and display it."
  (let* ((dict-code (alist-get dict-name samskritam-ambuda-dict-choices nil nil #'string=))
         (buffer-name (concat "*" dict-name "*"))
         (url (format "https://ambuda.org/tools/dictionaries/%s/%s" dict-code word)))
    (if (not dict-code)
        (message "Invalid dictionary: %s" dict-name)
      (let ((buffer (get-buffer-create buffer-name)))
        (with-current-buffer buffer
          (let ((inhibit-read-only t))
            (erase-buffer)
            (insert (format "\n;;;;;;;;;;;;;\n;;; %s\n;;;;;;;;;;;;;\n" word)))
          (let ((tbuf (url-retrieve-synchronously url t t)))
            (with-current-buffer tbuf
              (shr-render-buffer (current-buffer))
              (goto-char (point-min))
              (let ((beg (+ (search-forward "clear" nil t) 2))
                    (end (- (search-forward "ambuda" nil t) 7)))
                (with-current-buffer buffer
                  (insert (buffer-substring-no-properties beg end)))
                (kill-buffer (current-buffer))))))
        (popper-toggle-type 'frame (get-buffer buffer-name))))))


(declare-function pdf-view-active-region-text "ext:pdf-view")


(defvar samskritam-dictionary-key-bindings
  '(("a" "Apte")
    ("k" "Apte-Kosh")
    ("m" "MW")
    ("s" "Shabdasagara")
    ("v" "Vacaspatyam")
    ("h" "Shabdarthakausubha")
    ("o" "Amarakosha"))
  "Key bindings for dictionaries in the transient menu.")

(transient-define-prefix samskritam-dictionary-transient (word)
  "Choose a dictionary for WORD."
  `("Dictionaries"
    ,@(mapcar (lambda (binding)
                (let ((key (car binding))
                      (dict-name (cadr binding)))
                  `((,key ,dict-name
                         (lambda ()
                           (interactive)
                           (samskritam--fetch-and-display-definition word ,dict-name))))))
              samskritam-dictionary-key-bindings)))

;;;###autoload
(defun samskritam-dictionary-at-point ()
  "Show dictionary definitions for word at point."
  (interactive)
  (let ((word
         (cond
          ((eq major-mode 'pdf-view-mode)
           (car (pdf-view-active-region-text)))
          ((use-region-p)
           (buffer-substring-no-properties
            (region-beginning)
            (region-end)))
          (t
           (substring-no-properties
            (thing-at-point 'word))))))
    (if word
        (samskritam-dictionary-transient word)
      (message "No word at point."))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;; Devanagari Script ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Input method and key binding configuration.
(defvar samskritam-alternative-input-methods
  '(("devanagari-inscript" . [?\C-\\])))

(defvar samskritam-default-input-method
  (caar samskritam-alternative-input-methods))


(defun samskritam-toggle-alternative-input-method (method &optional arg interactive)
  "METHOD to toggle input method.  Takes in INTERACTIVE ARG."
  (if arg
      (toggle-input-method arg interactive)
    (let ((previous-input-method current-input-method))
      (when current-input-method
        (deactivate-input-method))
      (unless (and previous-input-method
                   (string= previous-input-method method))
        (activate-input-method method)))))

(defun samskritam-reload-alternative-input-methods ()
  "Load alternative input method."
  (dolist (config samskritam-alternative-input-methods)
    (let ((method (car config)))
      (global-set-key (cdr config)
                      `(lambda (&optional arg interactive)
                         ,(concat "Behaves similar to `toggle-input-method', but uses \""
                                  method "\" instead of `default-input-method'")
                         (interactive "P\np")
                         (samskritam-toggle-alternative-input-method ,method arg interactive))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defvar samskritam-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "d") 'samskritam-dictionary-at-point)
    map)
  "Keymap for `samskritam-mode'.")

;;;###autoload
(define-minor-mode samskritam-mode
  "Toggle Samskritam mode."
  :global t
  :version "0.2.0"
  :lighter " SKT"
  :group 'samskritam
  :keymap (let ((map (make-sparse-keymap))) map)

  (if samskritam-mode
      (progn
        (global-set-key (kbd samskritam-keymap-prefix) samskritam-mode-map)
	;;	(google-translate--search-tkk)
	(samskritam-reload-alternative-input-methods)
	(add-to-list 'google-translate-supported-languages-alist '("Sanskrit"  . "sa"))
	(message "samskritam mode activated!"))
    (progn
      (global-unset-key (kbd samskritam-keymap-prefix))
      (message "samskritam mode deactivated!")))


  (add-hook 'samskritam-mode-on-hook (lambda () (message "Samskritam turned on!")))
  (add-hook 'samskritam-mode-off-hook (lambda () (message "Samskritam turned off!"))))

;;;; राम
(provide 'samskritam)

;;; samskritam.el ends here
