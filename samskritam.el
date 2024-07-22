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

(defvar samskritam-mode nil
  "Toggle samskritam-mode.")


(defgroup samskritam nil
  "Provide functions for word definitions and translations."
  :group 'convenience)

(defcustom samskritam-keymap-prefix "C-c C-s"
  "The prefix for samskritam-mode key bindings."
  :type 'string
  :group 'samskritam)

(defcustom samskritam-transient-buffer nil
  "When nil, transient buffer is turned off."
  :type 'boolean
  :group 'samskritam)


(defun samskritam--key (key)
  (kbd (concat samskritam-keymap-prefix " " key)))


(defcustom samskritam-mode-line '(:eval (propertize " SKT" 'face 'mode-line-emphasis))
  "String to show in the mode-line of Samskritam. Setting this to
nil removes from the mode-line."
  :group 'samskritam
  :type '(choice (const :tag "Off" nil)))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;; Dictionary ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar samskritam-word-limit 20
  "Maximum amount of results to display.")


(defcustom samskritam-mode-line-position 0
  "Position in mode-line to place `samskritam-mode-line'."
  :type 'integer
  :group 'samskritam)


(defcustom samskritam-word-displayfn-alist nil
  "Alist for display functions per dict.
By default, `message' is used."
  :type '(alist
          :key-type (symbol :tag "Name of dict")
          :value-type (function :tag "Display function")))


(defcustom message-buffer-display-dict 'apte
  "Default dictionary to display in message buffer"
  :type 'string
  :group 'samskritam)



(defvar ambuda-dict-choices '(
			      ("Apte" . "apte")
			      ("Apte-Kosh" . "apte-sh")
			      ("MW" . "mw")
			      ("Shabdasagara" . "shabdasagara")
			      ("Vacaspatyam" . "vacaspatyam")
			      ("Shabdarthakausubha" . "shabdakalpadruma")
			      ("Amarakosha" . "amara")
			      )
  "Dictionaries in Ambuda.org to crawl from")



;;;###autoload
(defun samskritam-word (word dict &optional choose-dict)
  "Define WORD by referencing various dictionary DICT.
By default uses `samskritam-word-default-dict', but a prefix arg
lets the user CHOOSE-DICT."
  (interactive "MWord: \ni\nP")
  (let* ((previous-buffer (current-buffer))
	 (dict-choices ambuda-dict-choices
		       ))
    (dolist (dict-choice dict-choices)
      (setq buffer-name (concat "*" (car dict-choice) "*"))
      (setq dict (cdr dict-choice))
      (unless (buffer-live-p buffer-name)
	(get-buffer-create buffer-name))


      (with-current-buffer (get-buffer buffer-name)
	(end-of-buffer)
	(insert (format "\n;;;;;;;;;;;;;\n;;; %s\n;;;;;;;;;;;;;\n"  word)))
      (setq url (format "https://ambuda.org/tools/dictionaries/%s/%s" dict word))
      (setq tbuf (url-retrieve-synchronously url t t))
      (shr-render-buffer tbuf)

      ;; Goto some chars after the word "Clear"
      (setq beg (+ (search-forward "clear" nil t) 2))
      ;; Toto some chars before "Ambuda"
      (setq end (- (search-forward "ambuda" nil t) 7))
      (append-to-buffer buffer-name beg end)

      (if (string= dict message-buffer-display-dict)
	  (message "\n%s" (buffer-substring beg end))
	)

      (delete-window)
      )))


(declare-function pdf-view-active-region-text "ext:pdf-view")

;;;###autoload
(defun samskritam-word-at-point (arg)
  "Use `samskritam-word' to define word at point.
When the region is active, define the marked phrase.
Prefix ARG lets you choose dict.

In a non-interactive call DICT can be passed."
  (interactive "P")
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
    (samskritam-word word dict arg)))





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
	;;	(google-translate--search-tkk)
	(samskritam-reload-alternative-input-methods)
	(add-to-list 'google-translate-supported-languages-alist '("Sanskrit"  . "sa"))
	(message "samskritam mode activated!"))
    (message "samskritam mode deactivated!"))


  (add-hook 'samskritam-mode-on-hook (lambda () (message "Samskritam turned on!")))
  (add-hook 'samskritam-mode-off-hook (lambda () (message "Samskritam turned off!")))
  )

(provide 'samskritam)
;;;; राम
;;;; samskritam.el ends here
