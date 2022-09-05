;;; samskritam.el --- define samskrit word, translate to and from samskrit. -*- lexical-binding: t -*-

;; Copyright (C) 2022 Krishna Thapa

;; Description: Samskrit definitions and translations
;; Author: Krishna Thapa <thapakrish@gmail.com>
;; URL: https://github.com/thapakrish/samskritam
;; Version: 0.1.0
;; Package-Requires: ((emacs "28.1"))
;; Keywords: samskrit, sanskrit, dictionary, translation

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
;; Sends request to https://ambuda.org/ for dictionary definition
;;
;;; Code:


(require 'url-parse)
(require 'url-http)
(require 'google-translate)
(require 'google-translate-smooth-ui)


(defvar samskritam-mode)

(defgroup samskritam nil
  "Provide functions for word definitions and translations"
  :group 'convenience)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;; Translate ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun google-translate--search-tkk ()
  "Search TKK."
  (list 430675 2721866130))


(defcustom samskritam-mode-line '(:eval (propertize " SKT" 'face 'mode-line-emphasis))
  "String to show in the mode-line of Samskritam.
Setting this to nil removes from the mode-line."
  :group 'samskritam
  :type '(choice (const :tag "Off" nil)
		 ))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;; Dictionary ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar define-sanskrit-word-limit 20
  "Maximum amount of results to display.")


(defcustom samskritam-mode-line-position 0
  "Position in mode-line to place `samskritam-mode-line'."
  :type 'integer
  :group 'samskritam)


(defcustom define-sanskrit-word-displayfn-alist nil
  "Alist for display functions per service.
By default, `message' is used."
  :type '(alist
          :key-type (symbol :tag "Name of service")
          :value-type (function :tag "Display function")))

(defun define-sanskrit-word-displayfn (service)
  "Return the display function for SERVICE."
  (or (cdr (assoc service define-sanskrit-word-displayfn-alist))
      #'message))

(defcustom define-sanskrit-word-services
  '((vacaspatyam "https://ambuda.org/tools/dictionaries/vacaspatyam/%s" define-sanskrit-word--parse-vacaspatyam)
    (mw "https://ambuda.org/tools/dictionaries/mw/%s" define-sanskrit-word--parse-mw)
    )
  "Services for define-sanskrit-word, A list of lists of the
  format (symbol url function-for-parsing).
Instead of an url string, url can be a custom function for retrieving results."
  :type '(alist
          :key-type (symbol :tag "Name of service")
          :value-type (group
                       (string :tag "Url (%s denotes search word)")
                       (function :tag "Parsing function"))))

(defcustom define-sanskrit-word-default-service 'mw
  "The default service for define-sanskrit-word commands. Must be one of
  `define-sanskrit-word-services'"
  :type '(choice
	  (const mw)
          (const vacaspatyam)
          symbol))

(defun define-sanskrit-word--to-string (word service)
  "Get definition of WORD from SERVICE."
  (let* ((servicedata (assoc service define-sanskrit-word-services))
         (retriever (nth 1 servicedata))
         (parser (nth 2 servicedata))
         (url-user-agent
          (if (eq (nth 0 servicedata) 'vacaspatyam)
              "Mozilla/5.0 (Macintosh; Intel Mac OS X 11_5_2) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/93.0.4577.63 Safari/537.36"
            url-user-agent)))
    (if (functionp retriever)
        (funcall retriever word)
      ;; adapted `url-insert-file-contents'
      (let* ((url (format retriever (downcase word)))
             (buffer (url-retrieve-synchronously url t t)))
        (with-temp-buffer
          (url-insert-buffer-contents buffer url)
	  (message "Got from url %s" url)
          (funcall parser))))))

(defun define-sanskrit-word--expand (regex definition service)
  (let ((case-fold-search nil))
    (when (string-match regex definition)
      (concat
       definition
       "\n" (match-string 1 definition) ":\n"
       (mapconcat (lambda (s) (concat "  " s))
                  (split-string
                   (define-sanskrit-word--to-string (match-string 1 definition) service)
                   "\n")
                  "\n")))))

(defun define-sanskrit-word (word service &optional choose-service)
  "Define WORD using various services.

By default uses `define-sanskrit-word-default-service', but a prefix arg
lets the user choose service."
  (interactive "MWord: \ni\nP")
  (let* ((service (or service
                      (if choose-service
                          (intern
                           (completing-read
                            "Service: " define-sanskrit-word-services))
                        define-sanskrit-word-default-service)))
         (results (define-sanskrit-word--to-string word service)))

    (funcall
     (define-sanskrit-word-displayfn service)
     (cond ((not results)
            "0 definitions found")
           ((define-sanskrit-word--expand "Plural form of \\(.*\\)\\.$" results service))
           ((define-sanskrit-word--expand "Past participle of \\(.*\\)\\.$" results service))
           ((define-sanskrit-word--expand "Present participle of \\(.*\\)\\.$" results service))
           (t
            results))))
  )

(declare-function pdf-view-active-region-text "ext:pdf-view")

(defun define-sanskrit-word-at-point (arg &optional service)
  "Use `define-sanskrit-word' to define word at point.
When the region is active, define the marked phrase.
Prefix ARG lets you choose service.

In a non-interactive call SERVICE can be passed."
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
    (define-sanskrit-word word service arg)))

(defface define-sanskrit-word-face-1
  '((t :inherit font-lock-keyword-face))
  "Face for the part of speech of the definition.")

(defface define-sanskrit-word-face-2
  '((t :inherit default))
  "Face for the body of the definition")

(defun define-sanskrit-word--join-results (results)
  (mapconcat
   #'identity
   (if (> (length results) define-sanskrit-word-limit)
       (cl-subseq results 0 define-sanskrit-word-limit)
     results)
   "\n"))

(defun define-sanskrit-word--regexp-to-face (regexp face)
  (goto-char (point-min))
  (while (re-search-forward regexp nil t)
    (let ((match (match-string 1)))
      (replace-match
       (propertize match 'face face)))))

(defconst define-sanskrit-word--tag-faces
  '(("<\\(?:em\\|i\\)>\\(.*?\\)</\\(?:em\\|i\\)>" italic)
    ("<\\(?:b\\|i\\)>\\(.*?\\)</\\(?:b\\|i\\)>" bold)
    ("<cite>\\(.*?\\)</cite>" link)
    ("<abbr>\\(.*?\\)</abbr>" bold-italic)
    ("<strong>\\(.*?\\)</strong>" match)
    ("<span lang=\"[^\"]*\">\\([^<]*\\)</span>" variable-pitch)
    ("<span class=\"[^\"]*\">\\([^<]*\\)</span>" error)
    ("<span>\\(.*?\\)</span>" shadow)
))

(defun define-sanskrit-word--convert-html-tag-to-face (str)
  "Replace semantical HTML markup in STR with the relevant faces."
  (with-temp-buffer
    (insert str)
    (cl-loop for (regexp face) in define-sanskrit-word--tag-faces do
             (define-sanskrit-word--regexp-to-face regexp face))
    (buffer-string)))



(defun define-sanskrit-word--parse-mw ()
  "Parse output from mw site and return formatted list"
  (message "This message is from MW word parser!")
  (save-match-data
    (let (results beg part)
      ;;      (message "Output is: %s" results)
      (while (re-search-forward "<li class=\"dict-entry mw-entry\">" nil t)
        (skip-chars-forward " ")
        (setq beg (point))
        (when (re-search-forward "</li>")
          (push (concat
		 (propertize
                  (buffer-substring-no-properties beg (match-beginning 0))
                  'face 'define-sanskrit-word-face-2))
                results)))
      (when (setq results (nreverse results))
        (define-sanskrit-word--convert-html-tag-to-face (define-sanskrit-word--join-results results))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;; Devanagari Script ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Input method and key binding configuration.
(setq alternative-input-methods
      '(("devanagari-inscript" . [?\C-\\])
        ))

(setq default-input-method
      (caar alternative-input-methods))



(defun toggle-alternative-input-method (method &optional arg interactive)
  (if arg
      (toggle-input-method arg interactive)
    (let ((previous-input-method current-input-method))
      (when current-input-method
        (deactivate-input-method))
      (unless (and previous-input-method
                   (string= previous-input-method method))
        (activate-input-method method)))))

(defun reload-alternative-input-methods ()
  (dolist (config alternative-input-methods)
    (let ((method (car config)))
      (global-set-key (cdr config)
                      `(lambda (&optional arg interactive)
                         ,(concat "Behaves similar to `toggle-input-method', but uses \""
                                  method "\" instead of `default-input-method'")
                         (interactive "P\np")
                         (toggle-alternative-input-method ,method arg interactive))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;###autoload
(define-minor-mode samskritam-mode
  "Toggle Samskritam mode. "
  :global t
  :version "0.1.0"
  :lighter ""
  :group 'samskritam
  :keymap (let ((map (make-sparse-keymap))) map)
  (if samskritam-mode
      ;; Turning the mode ON
      (progn
	(google-translate--search-tkk)
	(reload-alternative-input-methods)
	(add-to-list 'google-translate-supported-languages-alist '("Sanskrit"  . "sa"))
	(add-hook 'translate-mode-hook
		  (lambda () (local-set-key (kbd "\C-ct") #'google-translate-smooth-translate)))
	)

    ;; Turning the mode OFF
    (remove-hook 'translate-mode-hook #'google-translate-smooth-translate)


    ;; TODO: Clean UP
    ;; ()
    ))

(provide 'samskritam)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; samskritam.el ends here
;; 
