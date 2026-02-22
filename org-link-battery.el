;;; org-link-battery.el --- Simple completion setup with company and bookmarks to org-headers with bookmark+ -*- lexical-binding: t -*-

;; Author: lordnik22
;; Version 1.0
;; Package-Requires: ((emacs "29.0"))

;;; Commentary
;; Little package that consolidates my setup code which was scadered
;; around now consolidated into this package. The code adds a new
;; bookmark+-bookmark-type for org-headers. It adds an company-backend
;; that get's it's completion-candidates from the the bookmark-list
;; (which contain enough information to create an org-id-link). This
;; is a minimal solution, for a complete linking-solution look into
;; org-roam or org-brain. I had bad experience with these packages,
;; always breaking something because these are quite some behemoths.

;;; Code
(require 'org)
(require 'org-id)
(require 'bookmark+)
(require 'company)

(defvar org-link-battery-id-prefix "id:")

;;;###autoload
(defun org-link-battery-ido-bmkp-jump ()
  "Uses ido to search for the bookmark"
  (interactive)
  (bookmark-jump
   (bookmark-get-bookmark
    (ido-completing-read "Find bookmark: " (bookmark-all-names)))))

(substitute-key-definition
     'bookmark-jump 'org-link-battery-ido-bmkp-jump (current-global-map))

(defun org-link-battery-id-complete-link (&optional arg)
  "Create an id: link using completion"
  (concat org-link-battery-id-prefix (org-id-get-with-outline-path-completion '((org-agenda-files . (:maxlevel . 9))))))

;;;###autoload
(defun org-link-battery-bmkp-make-org-id-bookmark ()
  "Used to create bookmarks to org-header identified by ID-Property."
  (interactive)
  (cond ((derived-mode-p 'org-mode)
         (let* ((hid (org-id-get-create))
                (bn (org-link-display-format (org-get-heading t t t t)))
                (bm-bn (bmkp-get-bookmark bn 'NOERROR)))
           (call-interactively 'org-store-link)
           (when (not (null bm-bn))
             (setq bn (concat bn
                       " (" (file-name-nondirectory buffer-file-name)
                       ":" (number-to-string (line-number-at-pos))
                       ")")))
           (if (and bm-bn
                    (equal hid (cadr (caddr (bookmark-prop-get bm-bn 'function)))))
               bm-bn
             (bmkp-make-function-bookmark bn (backquote (lambda () (org-id-goto ,hid)))))))))



;;;###autoload
(defun org-link-battery-id-description-function (link desc)
  "Generate description for links"
  (interactive)
  (cond (desc desc)
        ((region-active-p) (progn (buffer-substring (region-beginning) (region-end))))
        ((or (string-prefix-p "http" link)
             (string-prefix-p "https" link))
         (let ((title "")
               (url-b (url-retrieve link
                                    (lambda (_)
                                      (setq title (dom-text (dom-by-tag (libxml-parse-html-region (point-min) (point-max) nil t) 'title)))))))
           (let ((fallover 0))
             (while (or (< fallover 1) (ignore-errors (process-status url-b)))
               (setq fallover (+ fallover 1))
               (sleep-for 0 1)))
           (cond ((or (string-prefix-p "https://www.youtube.com/watch?v=" link)
                      (string-prefix-p "https://youtu.be/" link))
                  (concat "YOUTUBE: " title))
                 ((string-match-p "duden" link)
                  (let ((s (split-string title "|" t "[[:space:]]")))
                    (concat (upcase (car s)) ": " (cadr s))))
                 ((string-match-p "wikipedia" link) (concat "WIKI: " title))
                 (t title))))
        ((string-prefix-p org-link-battery-id-prefix link)
         (save-window-excursion
           (save-excursion
             (org-id-goto (string-remove-prefix org-link-battery-id-prefix link))
             (org-link-display-format (org-get-heading t t t t)))))
        (t (save-excursion (word-at-point)))))


(defvar company-org-bookmark-available 'unknown)

(defun company-org-bookmark-available ()
  "When bookmarks successfully loaded will return non-nil."
  (when (eq company-org-bookmark-available 'unknown)
    (condition-case err
        (progn
          (list-bookmarks)
          (setq company-org-bookmark-available (not (null bookmark-alist))))
      (error
       (message "Company-Org-Bookmark: %s" (error-message-string err))
       (setq company-org-bookmark-available nil))))
  company-org-bookmark-available)

(defun company-org-bookmark--lookup-words (word)
  "Filter bookmark-alist by WORD and return candidates."
  (all-completions word bookmark-alist))

(defun company-org-bookmark (command &optional arg &rest _ignored)
  "`company-mode' completion backend using bookmark-alist."
  (interactive (list 'interactive))
  (cl-case command
    (interactive (company-begin-backend 'company-org-bookmark))
    (prefix (when (company-org-bookmark-available) (company-grab-word)))
    (candidates
     (let ((words (company-org-bookmark--lookup-words arg))
           (completion-ignore-case t))
       words))
    (post-completion
     (kill-backward-chars (length arg))
     (insert (concat "[[" org-link-battery-id-prefix (cadr (caddr (bookmark-prop-get arg 'function))) "]" "[" arg "]]")))
    (kind 'text)
    (sorted t)
    (ignore-case 'keep-prefix)))


(defun company-org-bookmark-hook ()
              (set (make-local-variable 'company-backends) '(company-files company-org-bookmark)))
(add-hook 'org-mode-hook 'company-org-bookmark-hook)

;;;; Custom Variables
;;; org-link-battery.el ends here
