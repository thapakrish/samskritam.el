;;; samskritam-test.el --- Tests for samskritam.el -*- lexical-binding: t; -*-

(require 'ert)

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

;;; samskritam-test.el ends here
