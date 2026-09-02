;;; samskritam-test.el --- Tests for samskritam.el -*- lexical-binding: t; -*-

(require 'ert)
(require 'cl-lib)

(load-file (expand-file-name "../samskritam.el" (file-name-directory load-file-name)))

(ert-deftest samskritam-ambuda-response-text-extracts-dictionary-items ()
  (with-temp-buffer
    (insert "HTTP/1.1 200 OK\n\n"
            "<html><body>"
            "<div id=\"dict--response\">"
            "<ul>"
            "<li class=\"dict-entry mw-entry\"><span lang=\"sa\">राम</span> m. Rama</li>"
            "<li class=\"dict-entry mw-entry\">pleasing, charming</li>"
            "</ul>"
            "</div>"
            "</body></html>")
    (should (equal (samskritam--ambuda-response-text)
                   "• राम m. Rama\n• pleasing, charming"))))

(ert-deftest samskritam-ambuda-response-text-returns-empty-string-with-no-items ()
  (with-temp-buffer
    (insert "HTTP/1.1 200 OK\n\n"
            "<html><body><div id=\"dict--response\"></div></body></html>")
    (should (equal (samskritam--ambuda-response-text) ""))))

(ert-deftest samskritam-ambuda-response-text-returns-nil-without-response ()
  (with-temp-buffer
    (insert "HTTP/1.1 200 OK\n\n<html><body></body></html>")
    (should-not (samskritam--ambuda-response-text))))

(ert-deftest samskritam-mode-preserves-global-bindings ()
  (let* ((prefix (kbd samskritam-keymap-prefix))
         (input-key (kbd "C-\\"))
         (old-prefix-binding (lookup-key global-map prefix))
         (old-input-binding (lookup-key global-map input-key)))
    (unwind-protect
        (progn
          (define-key global-map prefix #'ignore)
          (define-key global-map input-key #'ignore)
          (samskritam-mode 1)
          (should (eq (key-binding prefix) #'samskritam))
          (should-not (eq (key-binding input-key) #'ignore))
          (samskritam-mode -1)
          (should (eq (key-binding prefix) #'ignore))
          (should (eq (key-binding input-key) #'ignore)))
      (samskritam-mode -1)
      (define-key global-map prefix
                  (unless (integerp old-prefix-binding) old-prefix-binding))
      (define-key global-map input-key
                  (unless (integerp old-input-binding) old-input-binding)))))

(ert-deftest samskritam-keymap-prefix-setter-refreshes-mode-map ()
  (let ((original samskritam-keymap-prefix)
        (new-prefix "C-c z"))
    (unwind-protect
        (progn
          (customize-set-variable 'samskritam-keymap-prefix new-prefix)
          (should (eq (lookup-key samskritam-mode-map (kbd new-prefix))
                      #'samskritam))
          (should-not (lookup-key samskritam-mode-map (kbd original))))
      (customize-set-variable 'samskritam-keymap-prefix original))))

(ert-deftest samskritam-main-transient-uses-unique-settings-key ()
  (let ((samskritam--current-word "rama"))
    (unwind-protect
        (progn
          (transient-setup 'samskritam--transient-display)
          (let ((show-command
                 (lookup-key transient--transient-map (kbd "S")))
                (settings-command
                 (lookup-key transient--transient-map (kbd "c"))))
            (should (commandp show-command))
            (should (eq settings-command #'samskritam-settings-transient))
            (should-not (eq show-command settings-command))))
      (transient--emergency-exit))))

(ert-deftest samskritam-enter-word-captures-current-context ()
  (let ((source-a (generate-new-buffer " source-a"))
        (source-b (generate-new-buffer " source-b"))
        samskritam-original-buffer
        captured-word)
    (unwind-protect
        (progn
          (with-current-buffer source-a
            (samskritam--capture-original-context))
          (with-current-buffer source-b
            (cl-letf (((symbol-function 'samskritam--get-word-from-user)
                       (lambda () "rama"))
                      ((symbol-function 'samskritam-transient)
                       (lambda (word) (setq captured-word word))))
              (samskritam--enter-word-action)))
          (should (eq samskritam-original-buffer source-b))
          (should (equal captured-word "rama")))
      (kill-buffer source-a)
      (kill-buffer source-b))))

(ert-deftest samskritam-folding-preserves-unrelated-overlays ()
  (with-temp-buffer
    (insert "## rama\nmeaning\n")
    (samskritam-dictionary-mode)
    (let ((external (make-overlay 10 13)))
      (overlay-put external 'external t)
      (goto-char 11)
      (samskritam-toggle-definition-folding)
      (should (overlay-buffer external))
      (should (= (seq-count
                  (lambda (overlay)
                    (overlay-get overlay 'samskritam-fold))
                  (overlays-in (point-min) (point-max)))
                 1))
      (samskritam-unfold-all-definitions)
      (should (overlay-buffer external))
      (should-not (seq-find
                   (lambda (overlay)
                     (overlay-get overlay 'samskritam-fold))
                   (overlays-in (point-min) (point-max)))))))

(ert-deftest samskritam-standardizes-legacy-delimiters ()
  (with-temp-buffer
    (insert ";;;;;;;;;;;;\n;;; rama\n;;;;;;;;;;;;\nmeaning\n"
            ";;;;;;;;;;;;;;; sita ;;;;;;;;;;;;;;;\nmeaning\n")
    (samskritam--standardize-delimiters)
    (should (equal (samskritam--extract-words-from-buffer)
                   '("rama" "sita")))
    (should-not (string-match-p ";;;;;;;;;;;;;;;" (buffer-string)))))

(ert-deftest samskritam-save-settings-persists-exposed-values ()
  (let ((samskritam-default-dictionary "MW")
        (samskritam-keymap-prefix "C-c C-s")
        saved)
    (cl-letf (((symbol-function 'customize-save-variable)
               (lambda (symbol value &optional _comment)
                 (push (cons symbol value) saved))))
      (samskritam--save-settings))
    (should (equal saved
                   '((samskritam-keymap-prefix . "C-c C-s")
                     (samskritam-default-dictionary . "MW"))))))

(ert-deftest samskritam-fetch-definition-uses-network-timeout ()
  (let ((samskritam-ambuda-dict-choices '(("Test" . "test")))
        (samskritam-network-timeout 7)
        request-args)
    (unwind-protect
        (cl-letf (((symbol-function 'url-retrieve-synchronously)
                   (lambda (&rest args)
                     (setq request-args args)
                     nil)))
          (should-not (samskritam--fetch-definition "rama" "Test"))
          (should (equal (nth 3 request-args) 7)))
      (let ((buffer (get-buffer "*Test*")))
        (when buffer
          (kill-buffer buffer))))))

;;; samskritam-test.el ends here
