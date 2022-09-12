# samskritam.el

;; Run M-x package-install RET samskritam-mode RET

```
  (use-package samskritam
    :ensure t
    :bind (("\C-cd"   . samskritam-word-at-point)
	   ("\C-cD"   . samskritam-word))
    :init
    (setq google-translate-backend-method 'curl)
    (defun google-translate--search-tkk () "Search TKK." (list 430675 2721866130))
    (global-set-key "\C-ct" 'google-translate-smooth-translate)
    )
  ;; Activate mode with M-x samskritam-mode
  ;; Turn ON Devanagari Inscript for typing: C + \
  ;; राम
```
