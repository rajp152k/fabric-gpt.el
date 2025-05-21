;;; fabric-gpt.el --- convenient crowd sourced prompts -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2025 Raj Patil
;;
;; Author: Raj Patil <rajp152k@gmail.com>
;; Maintainer: Raj Patil <rajp152k@gmail.com>
;; Created: March 12, 2025
;; Modified: March 12, 2025
;; Version: 1.0.0
;; Homepage: https://github.com/rajp152k/fabric-gpt.el
;; Package-Requires: ((emacs "24.3"))
;;
;; This file is not part of GNU Emacs.

(defvar fabric-gpt.el-root "")
(defvar fabric-gpt.el--remote-url  "https://github.com/danielmiessler/fabric")
(defvar fabric-gpt.el--patterns-subdirectory "/patterns")
(defvar fabric-gpt.el--patterns-path "fabric/patterns")
(defvar fabric-gpt.el--patterns nil)

(defun fabric-gpt.el-sparse-checkout-subdir (repo-url path branch)
  "sparse pull fabric patterns"
  (let* ((repo-name (file-name-nondirectory (file-name-sans-extension repo-url)))
         (repo-pos (concat fabric-gpt.el-root repo-name))
         (repo-exists (file-directory-p repo-pos)))
    (if (not repo-exists)
        (progn
          (message "creating directory %s" repo-pos)
          (make-directory repo-pos t))
      (message "repo already exists at %s" repo-pos))
    (let ((reporter (make-progress-reporter "sparse pulling patterns " 0 4)))
      (with-temp-buffer
        (cd repo-pos)
        (unless repo-exists
          (progress-reporter-update reporter 0 "| cloning repo")
          (unless (zerop (call-process "git" nil t nil "clone" "--no-checkout" repo-url "."))
            (error "Failed to clone repository %s" repo-url))
          (progress-reporter-update reporter 1 "| sparse checkout config")

          (unless (zerop (call-process "git" nil t nil "sparse-checkout" "init"))
            (error "Failed to initialize sparse checkout"))
          (progress-reporter-update reporter 2 "| sparse-checkout set")

          (unless (zerop (call-process "git" nil t nil "sparse-checkout" "set" "--no-cone" path))
            (error "Failed to set sparse checkout for %s" path))
          (progress-reporter-update reporter 3 "| main pull"))

        (let ((checkout-status (call-process "git" nil t nil "checkout" branch)))
          (unless (zerop checkout-status)
            (if repo-exists
                (warn "Failed to checkout branch %s in %s. Patterns might be outdated." branch repo-pos)
              (error "Failed to checkout branch %s in %s" branch repo-pos)))) ;; Added repo-pos to error

        (progress-reporter-update reporter 4)
        (message "Sparse pulled patterns for %s on branch %s" repo-name branch)))))


(defun fabric-gpt.el-populate-patterns ()
  "filter out invalid directories"
  (with-temp-buffer
    (let ((patterns-dir (concat fabric-gpt.el-root fabric-gpt.el--patterns-path)))
      (unless (file-directory-p patterns-dir)
        (error "Patterns directory not found: %s. Please run fabric-gpt.el-sync-patterns first." patterns-dir))
      (setq fabric-gpt.el--patterns (cl-remove-if-not
                                     (lambda (pattern)
                                       (f-exists-p (format "%s/%s/system.md" patterns-dir pattern))) ;; Use patterns-dir variable
                                     (directory-files patterns-dir nil "^[^.]" t)))))) ;; Use patterns-dir variable


(defun fabric-gpt.el-yield-prompt ()
  "completing read fabric patterns"
  (let ((pattern (completing-read "fabric-patterns: " fabric-gpt.el--patterns)))
    (with-temp-buffer
      (insert-file-contents (format "%s%s/%s/system.md" fabric-gpt.el-root fabric-gpt.el--patterns-path pattern))
      (buffer-string))))

(defun fabric-gpt.el-sync-patterns ()
  "sparse pull patterns and populate cache"
  (interactive)
  (fabric-gpt.el-sparse-checkout-subdir fabric-gpt.el--remote-url fabric-gpt.el--patterns-subdirectory "main")
  (fabric-gpt.el-populate-patterns))

(defun fabric-gpt.el-send ()
  "dispatch pattern for context preceding the cursor with the selected pattern"
  (interactive)
  (let ((gptel--system-message (fabric-gpt.el-yield-prompt)))
    (insert "\n\n")
    (gptel-send)))

(provide 'fabric-gpt.el)
;;; fabric-gpt.el ends here
