;;; samskritam.el --- Show samskrit word definitions -*- lexical-binding: t -*-

;; Copyright (C) 2022 Krishna Thapa

;; Description: Samskrit definitions
;; Author: Krishna Thapa <thapakrish@gmail.com>
;; URL: https://github.com/thapakrish/samskritam
;; Version: 0.2.0
;; Package-Requires: ((emacs "28.1"))
;; Keywords: samskrit, sanskrit, संस्कृत, dictionary, devanagari, convenience, language

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
;; Sends request to https://ambuda.org/ for dictionary definition of words
;; Add Vedic characters to devanagari-inscript
;;
;;; Code:


(require 'url-parse)
(require 'url-http)
(require 'transient)
(require 'custom)
(require 'subr-x)

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

(defcustom samskritam-default-dictionary "MW"
  "The default dictionary to use."
  :type '(choice (const "MW")
                 (const "Apte")
                 (const "Apte-Kosh")
                 (const "Shabdasagara")
                 (const "Vacaspatyam")
                 (const "Shabdarthakausubha")
                 (const "Amarakosha"))
  :group 'samskritam)





;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;; Dictionary ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar samskritam-last-dictionary-buffer nil
  "The last dictionary buffer that was active.")

(defvar samskritam-original-window nil
  "The original window before switching to dictionary buffer.")

(defvar samskritam-original-buffer nil
  "The original buffer before switching to dictionary buffer.")

(defvar samskritam-original-position nil
  "The original position in the buffer before switching to dictionary buffer.")

(defvar samskritam-dictionary-window nil
  "The window created for the dictionary buffer.")

(defun samskritam--capture-original-context ()
  "Capture the original context (buffer, position, window) for restoration."
  (setq samskritam-original-buffer (current-buffer))
  (setq samskritam-original-position (point))
  (setq samskritam-original-window (selected-window)))

;; Delimiter formatting for word sections inside dictionary buffers
(defconst samskritam--delimiter-prefix "## "
  "Prefix used to mark the start of a word definition block.")

(defconst samskritam--delimiter-regex
  (concat "^" (regexp-quote samskritam--delimiter-prefix) "\\s-*\\(.*\\)\\s-*$")
  "Regex that matches a delimiter line. Capturing group 1 is the word (trimmed).")

(defvar samskritam-ambuda-dict-choices '(
			      ("Apte" . "apte")
			      ("Apte-Kosh" . "apte-sh")
			      ("MW" . "mw")
			      ("Shabdasagara" . "shabdasagara")
			      ("Vacaspatyam" . "vacaspatyam")
			      ("Shabdarthakausubha" . "shabdakalpadruma")
			      ("Amarakosha" . "amara"))
  "Dictionaries in Ambuda.org to crawl from.")


(declare-function pdf-view-active-region-text "ext:pdf-view")

;;;###autoload
(defun samskritam--get-word-at-point ()
  "Get word at point, considering pdf-view-mode and region."
  (cond
   ((eq major-mode 'pdf-view-mode)
    (car (pdf-view-active-region-text)))
   ((use-region-p)
    (buffer-substring-no-properties
     (region-beginning)
     (region-end)))
   (t
    (let ((w (thing-at-point 'word)))
      (when w
        (substring-no-properties w))))))

;;;###autoload
(defun samskritam--get-word-from-user ()
  "Prompt user for a word to look up."
  (read-string "Enter word to look up: "))

;;;###autoload
(defun samskritam--show-dictionary-buffer (dict-name)
  "Show the buffer for DICT-NAME dictionary."
  (let ((buffer-name (concat "*" dict-name "*")))
    (if (get-buffer buffer-name)
        (let ((buffer (get-buffer buffer-name)))
          (with-current-buffer buffer
            ;; Ensure we're in dictionary mode
            (unless (eq major-mode 'samskritam-dictionary-mode)
              (samskritam-dictionary-mode))
            ;; Standardize delimiter formats
            (samskritam--standardize-delimiters))
          ;; Clean up any existing dictionary window
          (when (and samskritam-dictionary-window (window-live-p samskritam-dictionary-window))
            (delete-window samskritam-dictionary-window))
          (let ((new-window (split-window-below)))
            (set-window-buffer new-window buffer)
            (setq samskritam-dictionary-window new-window)
            (select-window new-window)))
      (message "No buffer found for %s dictionary" dict-name))))

;;;###autoload
(defun samskritam--list-all-dictionary-buffers ()
  "List all available dictionary buffers."
  (interactive)
  (let ((buffers (cl-remove-if-not
                  (lambda (buf)
                    (string-match "\\*\\(Apte\\|Apte-Kosh\\|MW\\|Shabdasagara\\|Vacaspatyam\\|Shabdarthakausubha\\|Amarakosha\\)\\*"
                                 (buffer-name buf)))
                  (buffer-list))))
    (if buffers
        (dolist (buf buffers)
          (switch-to-buffer buf)))
      (message "No dictionary buffers found")))

;;;###autoload
(defun samskritam--jump-to-last-dict-buffer ()
  "Switch to the last active dictionary buffer. Return t on success."
  (if (and samskritam-last-dictionary-buffer (buffer-live-p samskritam-last-dictionary-buffer))
      (progn
        (switch-to-buffer samskritam-last-dictionary-buffer)
        t)
    (message "No dictionary buffer active.")
    nil))

;;;###autoload
(defun samskritam-next-definition ()
  "Jump to the next word definition in the current/last dictionary buffer."
  (interactive)
  (let ((pos (point)))
    ;; If we're currently at a delimiter, move past it first
    (when (looking-at samskritam--delimiter-regex)
      (forward-line))
    (if (re-search-forward samskritam--delimiter-regex nil t)
        (progn
          (beginning-of-line)
          (message "Next definition: %s" (match-string 0)))
      (goto-char pos)
      (message "No more definitions found"))))

;;;###autoload
(defun samskritam-previous-definition ()
  "Jump to the previous word definition in the current/last dictionary buffer."
  (interactive)
  (let ((pos (point)))
    (if (re-search-backward samskritam--delimiter-regex nil t)
        (progn
          (beginning-of-line)
          (message "Previous definition: %s" (match-string 0)))
      (goto-char pos)
      (message "No previous definitions found"))))

(defun samskritam--fetch-definition (word dict-name)
  "Fetch definition for WORD from DICT-NAME and return the buffer.
This function does NOT handle window display."
  (let* ((dict-code (alist-get dict-name samskritam-ambuda-dict-choices nil nil #'string=))
         (buffer-name (concat "*" dict-name "*"))
         (url (format "https://ambuda.org/tools/dictionaries/%s/%s" dict-code word)))
    (if (not dict-code)
        (progn
          (message "Invalid dictionary: %s" dict-name)
          nil) ; Return nil on failure
      (let ((buffer (get-buffer-create buffer-name)))
        (with-current-buffer buffer
          (unless (eq major-mode 'samskritam-dictionary-mode)
            (samskritam-dictionary-mode))
          ;; Standardize any legacy delimiters to the new format first
          (samskritam--standardize-delimiters)
          (goto-char (point-min))
          ;; Only fetch if definition isn't already in the buffer
          (unless (re-search-forward (format "%s%s$" (regexp-quote samskritam--delimiter-prefix) (regexp-quote word)) nil t)
            (message "Fetching definition for '%s'..." word)
            ;; This block prevents the web request from splitting the window
            (let* ((display-buffer-alist '((".*" display-buffer-no-window)))
                   (pop-up-windows nil)
                   (tbuf (url-retrieve-synchronously url t t)))
              (with-current-buffer tbuf
                (shr-render-buffer (current-buffer))
                (goto-char (point-min))
                (let* ((beg (search-forward "clear" nil t))
                       (end (search-forward "ambuda" nil t))
                       (definition (when (and beg end) (buffer-substring-no-properties (+ beg 2) (- end 7)))))
                  (if definition
                      (let ((clean-definition (replace-regexp-in-string "^\\([[:space:]]*\\)\\*" "\\1•" definition)))
                        (with-current-buffer buffer
                          (goto-char (point-max))
                          (unless (bolp) (insert "\n"))
                          (insert (format "\n%s%s\n%s\n" samskritam--delimiter-prefix word clean-definition))
                          (goto-char (point-max))))
                    (message "Could not parse definition for '%s'" word)))
                (kill-buffer (current-buffer)))))
          (message "Fetched definition for '%s'" word))
        ;; Return the buffer on success
        (setq samskritam-last-dictionary-buffer (get-buffer buffer-name))
        buffer))))

;;;###autoload
(defun samskritam-define-word-at-point ()
  "Fetch and display definition for the word at point."
  (interactive)
  (samskritam--capture-original-context)
  (let* ((word (samskritam--get-word-at-point)))
    (if word
        ;; 1. Fetch the data and get the buffer
        (let ((dict-buffer (samskritam--fetch-definition word samskritam-default-dictionary)))
          ;; 2. Display the buffer
          (samskritam--display-dictionary-buffer dict-buffer))
      (message "No word at point."))))

;;;###autoload
(defun samskritam-define-word-at-point-choice ()
  "Fetch definition for the word at point with dictionary choice."
  (interactive)
  (let ((word (samskritam--get-word-at-point)))
    (if word
        (samskritam-transient word)
      (message "No word at point."))))

;;;###autoload
(defun samskritam--lookup-all-dictionaries ()
  "Look up current word in all available dictionaries."
  (interactive)
  (if samskritam--current-word
      (progn
        (dolist (dict-pair samskritam-ambuda-dict-choices)
          (let ((dict-name (car dict-pair)))
            (condition-case err
                (samskritam--fetch-definition samskritam--current-word dict-name)
              (error (message "Error looking up in %s: %s" dict-name (error-message-string err))))))
        (let ((mw-buffer (get-buffer "*MW*")))
          (if (and mw-buffer (> (buffer-size mw-buffer) 0))
              (progn
                (setq samskritam-last-dictionary-buffer mw-buffer)
                (samskritam--display-dictionary-buffer mw-buffer)
                (message "Looked up '%s' in all dictionaries, showing MW buffer" samskritam--current-word))
            (message "MW buffer not found or empty after population"))))
    (message "No word selected")))


;;;###autoload
(defun samskritam--enter-word-action ()
  "Enter a word and open the dictionary transient."
  (interactive)
  (let ((entered-word (samskritam--get-word-from-user)))
    (if (not (string-empty-p entered-word))
        (samskritam-transient entered-word)
      (message "No word entered."))))



;;;###autoload
(defun samskritam--toggle-input-method ()
  "Toggle Devanagari input method."
  (interactive)
  (samskritam-toggle-alternative-input-method "devanagari-inscript"))

;; Helper functions for transient actions
(defvar samskritam--current-word nil
  "The current word being processed in the transient.")



;;;###autoload
(defun samskritam--jump-to-word-definition (word)
  "Jump to the definition of WORD in the current dictionary buffer."
  (interactive "sEnter word to jump to: ")
  (goto-char (point-min))
  (if (re-search-forward (format "^%s%s$" (regexp-quote samskritam--delimiter-prefix) (regexp-quote word)) nil t)
      (progn
        (beginning-of-line)
        (message "Found definition for '%s'" word))
    (message "Definition for '%s' not found in current buffer" word)))

;;;###autoload
(defun samskritam--jump-to-word-definition-complete ()
  "Jump to a word definition using completion from words in the current buffer."
  (interactive)
  (let ((words (samskritam--extract-words-from-buffer)))
    (if words
        (let ((selected-word (completing-read "Jump to word: " words nil t)))
          (if (not (string-empty-p selected-word))
              (samskritam--jump-to-word-definition selected-word)
            (message "No word selected")))
      (message "No word definitions found in current buffer"))))

;;;###autoload
(defun samskritam--jump-to-word-definition-complete-annotated ()
  "Jump to a word definition using completion with annotations."
  (interactive)
  (let ((words (samskritam--extract-words-from-buffer)))
    (if words
        (let ((selected-word (completing-read "Jump to word: " words nil t)))
          (if (not (string-empty-p selected-word))
              (samskritam--jump-to-word-definition selected-word)
            (message "No word selected")))
      (message "No word definitions found in current buffer"))))

;;;###autoload
(defun samskritam--create-annotated-completion-table (words)
  "Create a completion table with annotations for WORDS."
  (let ((table (make-hash-table :test 'equal)))
    (dolist (word words)
      (puthash word (format "%s" word) table)))
    table)

;;;###autoload
(defun samskritam--quit-dictionary-buffer ()
  "Close the current dictionary buffer window and return to original buffer and position."
  (interactive)
  (if (derived-mode-p 'samskritam-dictionary-mode)
      (let ((current-window (selected-window)))
        ;; Delete the dictionary window (keeps the buffer alive)
        (when (and samskritam-dictionary-window (window-live-p samskritam-dictionary-window))
          (delete-window samskritam-dictionary-window))
        ;; Reset the dictionary window variable
        (setq samskritam-dictionary-window nil)
        ;; Restore original context
        (when (and samskritam-original-window (window-live-p samskritam-original-window))
          (select-window samskritam-original-window)
          (when (and samskritam-original-buffer (buffer-live-p samskritam-original-buffer))
            (switch-to-buffer samskritam-original-buffer)
            (when samskritam-original-position
              (goto-char samskritam-original-position)))))
    (message "Not in a dictionary buffer")))

;;;###autoload
(defun samskritam--extract-words-from-buffer ()
  "Extract all word definitions from the current buffer."
  (let ((words '()))
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward samskritam--delimiter-regex nil t)
        (let ((word (string-trim (match-string 1))))
          (when (and word (not (string-empty-p word)))
            (push word words)))))
    (reverse words)))

;;;###autoload
(transient-define-prefix samskritam-navigation-transient ()
  "Navigation transient for dictionary buffers."
  [:description (lambda () (format "Navigation - Definitions: %d"
                                   (length (samskritam--extract-words-from-buffer))))
   :transient-suffix 'transient--do-stay]
  ["Navigation"
   ("n" "Next Definition" samskritam-next-definition :transient t)
   ("p" "Previous Definition" samskritam-previous-definition :transient t)
   ("j" "Jump to word (completion)" samskritam--jump-to-word-definition-complete-annotated :transient t)
   ("f" "Fold all" samskritam-fold-all-definitions :transient t)
   ("u" "Unfold all" samskritam-unfold-all-definitions :transient t)
   ("t" "Toggle current" samskritam-toggle-definition-folding :transient t)
   ("q" "Quit" transient-quit-one :transient t)]
  )

;;;###autoload
(defun samskritam-fold-all-definitions ()
  "Fold all word definitions in the current dictionary buffer."
  (interactive)
  (samskritam-unfold-all-definitions)
  (save-excursion
    (goto-char (point-min))
    (while (re-search-forward samskritam--delimiter-regex nil t)
      (let* ((delimiter-end (match-end 0))
             (content-start (save-excursion (goto-char delimiter-end) (forward-line 1) (point)))
             (content-end (save-excursion
                            (goto-char content-start)
                            (if (re-search-forward samskritam--delimiter-regex nil t)
                                (match-beginning 0)
                              (point-max)))))
        (when (> content-end content-start)
          (let ((overlay (make-overlay content-start content-end)))
            (overlay-put overlay 'invisible 'samskritam-fold)
            (overlay-put overlay 'evaporate t)
            (overlay-put overlay 'face '(:foreground "gray50"))))))))

;;;###autoload
(defun samskritam-unfold-all-definitions ()
  "Unfold all word definitions in the current dictionary buffer."
  (interactive)
  (remove-overlays (point-min) (point-max)))

;;;###autoload
(defun samskritam-toggle-definition-folding ()
  "Toggle folding of the current word definition."
  (interactive)
  (save-excursion
    (let ((is-on-delimiter (progn (beginning-of-line) (looking-at samskritam--delimiter-regex))))
      (if (or is-on-delimiter (re-search-backward samskritam--delimiter-regex nil t))
          (let* ((delimiter-end (match-end 0))
                 (content-start (save-excursion (goto-char delimiter-end) (forward-line 1) (point)))
                 (content-end (save-excursion
                                (goto-char content-start)
                                (if (re-search-forward samskritam--delimiter-regex nil t)
                                    (match-beginning 0)
                                  (point-max))))
                 (overlays (overlays-in content-start content-end)))
            (if overlays
                (mapc #'delete-overlay overlays)
              (let ((overlay (make-overlay content-start content-end)))
                (overlay-put overlay 'invisible 'samskritam-fold)
                (overlay-put overlay 'evaporate t)
                (overlay-put overlay 'face '(:foreground "gray50")))))
        (message "Not within a definition section.")))))

;; Define a special mode for dictionary buffers (like magit-log-mode)
(define-derived-mode samskritam-dictionary-mode fundamental-mode "Samskritam-Dict"
  "Major mode for Samskritam dictionary buffers."
  :group 'samskritam
  (setq-local transient-current-prefix 'samskritam-navigation-transient)
  ;; Ensure our folding overlays become invisible in this buffer
  (add-to-invisibility-spec 'samskritam-fold)
  (setq-local header-line-format
              '(:eval
                (let* ((buffer-name (buffer-name))
                       (dict-name (replace-regexp-in-string "\\*" "" buffer-name)))
                  (propertize
                   (format " %s | [n]ext [p]rev | [j]ump | [f]old [u]nfold [t]oggle | [s]how menu | [q]uit"
                           dict-name)
                   'face 'header-line)))))

;; Key bindings for dictionary mode
(define-key samskritam-dictionary-mode-map (kbd "n") 'samskritam-next-definition)
(define-key samskritam-dictionary-mode-map (kbd "p") 'samskritam-previous-definition)
(define-key samskritam-dictionary-mode-map (kbd "j") 'samskritam--jump-to-word-definition-complete-annotated)
(define-key samskritam-dictionary-mode-map (kbd "f") 'samskritam-fold-all-definitions)
(define-key samskritam-dictionary-mode-map (kbd "u") 'samskritam-unfold-all-definitions)
(define-key samskritam-dictionary-mode-map (kbd "t") 'samskritam-toggle-definition-folding)
(define-key samskritam-dictionary-mode-map (kbd "s") 'samskritam)
(define-key samskritam-dictionary-mode-map (kbd "q") 'samskritam--quit-dictionary-buffer)
(define-key samskritam-dictionary-mode-map (kbd "?") (lambda () (interactive)
                                                         (message "Nav keys: n/p (next/prev), j (jump), f/u (fold/unfold), t (toggle), s (menu), q (quit)")))

;; Helper function to set up dictionary buffer keymap (for backward compatibility)
(defun samskritam--setup-dictionary-keymap ()
  "Set up local key bindings for dictionary buffers."
  (samskritam-dictionary-mode))

;; Helper function to standardize delimiter formats
(defun samskritam--standardize-delimiters ()
  "Standardize delimiter formats in the current buffer."
  (save-excursion
    (goto-char (point-min))
    ;; Convert various old formats to new format
    (let ((converted 0))
      (while (re-search-forward "^;;;;;;;;;;;;\n;;; \\([^;]*\\)\n;;;;;;;;;;;;$" nil t)
        (let ((word (match-string 1)))
          (replace-match (format ";;;;;;;;;;;;;;; %s ;;;;;;;;;;;;;;;" word) t)
          (setq converted (1+ converted))))
      (when (> converted 0)
        (message "Standardized %d old-format delimiters" converted)))
    ;; Also handle single-line format variations
    (goto-char (point-min))
    (while (re-search-forward "^;;;;;;;;;;;;;;; \\([^;]*\\) ;;;;;;;;;;;;;;;$" nil t)
      (let ((word (match-string 1)))
        (replace-match (format ";;;;;;;;;;;;;;; %s ;;;;;;;;;;;;;;;" word) t)))))

;; Dictionary buffer keymap for navigation
(defvar samskritam-dictionary-keymap
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "n") 'samskritam-next-definition)
    (define-key map (kbd "p") 'samskritam-previous-definition)
    (define-key map (kbd "j") 'samskritam--jump-to-word-definition)
    (define-key map (kbd "f") 'samskritam-fold-all-definitions)
    (define-key map (kbd "u") 'samskritam-unfold-all-definitions)
    (define-key map (kbd "t") 'samskritam-toggle-definition-folding)
    (define-key map (kbd "SPC") 'samskritam-next-definition)
    (define-key map (kbd "DEL") 'samskritam-previous-definition)
    (define-key map (kbd "?") (lambda () (interactive)
                                 (message "Navigation keys: n/p (next/prev), j (jump), f/u (fold/unfold), t (toggle), SPC/DEL (next/prev)")))
    map)
  "Keymap for dictionary buffers with navigation shortcuts.")

(transient-define-prefix samskritam-settings-transient ()
  "Samskritam Settings"
  ["Preferences"
   ("d" "Default Dictionary" (lambda () (interactive) 
                                (let ((new-dict (completing-read "Default Dictionary: " 
                                                               '("MW" "Apte" "Apte-Kosh" "Shabdasagara" "Vacaspatyam" "Shabdarthakausubha" "Amarakosha") 
                                                               nil t)))
                                  (when new-dict
                                    (setq samskritam-default-dictionary new-dict)
                                    (message "Default dictionary set to: %s" new-dict)))) :transient t)
   ("k" "Keymap Prefix" (lambda () (interactive) 
                            (let ((new-prefix (read-string "Keymap Prefix: " samskritam-keymap-prefix)))
                              (when new-prefix
                                (setq samskritam-keymap-prefix new-prefix)
                                (message "Keymap prefix set to: %s" new-prefix)))) :transient t)]
  ["Persistence"
   ("s" "Save Settings" (lambda () (interactive) 
                         (if (fboundp 'customize-save-variables)
                             (customize-save-variables)
                           (message "Settings saved.")) 
                         (message "Settings saved.")) :transient t)
   ("q" "Quit" transient-quit-one)])

(transient-define-prefix samskritam--transient-display ()
  "Samskritam Transient"
  [:description (lambda () (format "Samskritam - Word: %s | Buffer: %s"
                                   (or samskritam--current-word "none")
                                   (buffer-name)))]
  [
   ["Actions"
    ("W" "Enter Word" samskritam--enter-word-action)
    ("d" "Define at Point" samskritam-define-word-at-point-choice
     :if-mode '(text-mode org-mode))]
   ["Dictionaries"
    ("a" "Apte"       (lambda () (interactive) 
                     (let ((dict-buffer (samskritam--fetch-definition samskritam--current-word "Apte")))
                       (samskritam--display-dictionary-buffer dict-buffer)))
     :inapt-if-not (lambda () samskritam--current-word))
    ("k" "Apte-Kosh"  (lambda () (interactive) 
                     (let ((dict-buffer (samskritam--fetch-definition samskritam--current-word "Apte-Kosh")))
                       (samskritam--display-dictionary-buffer dict-buffer)))
     :inapt-if-not (lambda () samskritam--current-word))
    ("m" "MW" (lambda () (interactive) 
             (let ((dict-buffer (samskritam--fetch-definition samskritam--current-word "MW")))
               (samskritam--display-dictionary-buffer dict-buffer)))
     :inapt-if-not (lambda () samskritam--current-word))
    ("s" "Shabdasagara" (lambda () (interactive) 
                       (let ((dict-buffer (samskritam--fetch-definition samskritam--current-word "Shabdasagara")))
                         (samskritam--display-dictionary-buffer dict-buffer)))
     :inapt-if-not (lambda () samskritam--current-word))
    ("v" "Vacaspatyam" (lambda () (interactive) 
                      (let ((dict-buffer (samskritam--fetch-definition samskritam--current-word "Vacaspatyam")))
                        (samskritam--display-dictionary-buffer dict-buffer)))
     :inapt-if-not (lambda () samskritam--current-word))
    ("h" "Shabdarthakausubha" (lambda () (interactive) 
                             (let ((dict-buffer (samskritam--fetch-definition samskritam--current-word "Shabdarthakausubha")))
                               (samskritam--display-dictionary-buffer dict-buffer)))
     :inapt-if-not (lambda () samskritam--current-word))
    ("o" "Amarakosha" (lambda () (interactive) 
                     (let ((dict-buffer (samskritam--fetch-definition samskritam--current-word "Amarakosha")))
                       (samskritam--display-dictionary-buffer dict-buffer)))
     :inapt-if-not (lambda () samskritam--current-word))
    ("*" "All" samskritam--lookup-all-dictionaries
     :inapt-if-not (lambda () samskritam--current-word))]
   ]
  [
   ["Show Buffers"
    ("A" "Show Apte" (lambda () (interactive) (samskritam--show-dictionary-buffer "Apte")))
    ("K" "Show Apte-Kosh" (lambda () (interactive) (samskritam--show-dictionary-buffer "Apte-Kosh")))
    ("M" "Show MW" (lambda () (interactive) (samskritam--show-dictionary-buffer "MW")))
    ("S" "Show Shabdasagara" (lambda () (interactive) (samskritam--show-dictionary-buffer "Shabdasagara")))
    ("V" "Show Vacaspatyam" (lambda () (interactive) (samskritam--show-dictionary-buffer "Vacaspatyam")))
    ("H" "Show Shabdarthakausubha" (lambda () (interactive) (samskritam--show-dictionary-buffer "Shabdarthakausubha")))
    ("O" "Show Amarakosha" (lambda () (interactive) (samskritam--show-dictionary-buffer "Amarakosha")))]
   ]
  [
   ["Settings"
    ("i" "Toggle IME" samskritam--toggle-input-method :transient t)
    ("S" "Settings" samskritam-settings-transient)]
   [""
    ("q" "Quit" transient-quit-one)]
   ]
  )

;; User-facing wrapper function
;;;###autoload
(defun samskritam-transient (&optional word)
  "Set up the dynamic environment and display the Samskritam transient."
  (setq samskritam--current-word word)
  (samskritam--transient-display))


;; Main command called by user
;;;###autoload
(defun samskritam ()
  "Open the Samskritam transient. Captures word at point for context."
  (interactive)
  (samskritam--capture-original-context)
  (samskritam-transient (samskritam--get-word-at-point)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;; Devanagari Script ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Input method and key binding configuration.
(defvar samskritam-alternative-input-methods
  '(("devanagari-inscript" . [?\C-\\])))

(defvar samskritam-default-input-method
  (caar samskritam-alternative-input-methods))


;;;###autoload
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

;;;###autoload
(defun samskritam--display-dictionary-buffer (buffer)
  "Display BUFFER in a dedicated, reusable dictionary window."
  (when buffer
    ;; Clean up any existing dictionary window before creating a new one
    (when (and samskritam-dictionary-window (window-live-p samskritam-dictionary-window))
      (delete-window samskritam-dictionary-window))
    ;; Create the new window and display the buffer
    (let ((new-window (split-window-below)))
      (set-window-buffer new-window buffer)
      (with-current-buffer buffer
        (unless (eq major-mode 'samskritam-dictionary-mode)
          (samskritam-dictionary-mode))
        (samskritam--standardize-delimiters))
      (setq samskritam-dictionary-window new-window)
      (select-window new-window))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defvar samskritam-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c d") 'samskritam-define-word-at-point)
    (define-key map (kbd "C-c D") 'samskritam--enter-word-action)
    map)
  "Keymap for `samskritam-mode'.")

;;;###autoload
(define-minor-mode samskritam-mode
  "Toggle Samskritam mode."
  :global t
  :version "0.5.2"
  :lighter " SKT"
  :group 'samskritam
  :keymap samskritam-mode-map

  (if samskritam-mode
      (progn
        (global-set-key (kbd samskritam-keymap-prefix) 'samskritam)
	(samskritam-reload-alternative-input-methods)
	(message "samskritam mode activated!"))
    (progn
      (global-unset-key (kbd samskritam-keymap-prefix))
      (message "samskritam mode deactivated!")))


  (add-hook 'samskritam-mode-on-hook (lambda () (message "Samskritam turned on!")))
  (add-hook 'samskritam-mode-off-hook (lambda () (message "Samskritam turned off!"))))

;; Enable the mode by default
(samskritam-mode 1)

;; Set up key binding immediately
(global-set-key (kbd samskritam-keymap-prefix) 'samskritam)

;;;; राम
(provide 'samskritam)

;;; samskritam.el ends here
