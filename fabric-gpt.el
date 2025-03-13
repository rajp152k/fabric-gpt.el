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

(defvar fabric-gptel--remote-url  "https://github.com/danielmiessler/fabric")
(defvar fabric-gptel--patterns-subdirectory "/patterns")
(defvar fabric-gptel--patterns-path "fabric/patterns")
(defvar fabric-gptel--patterns nil)

(defun fabric-gptel-sparse-checkout-subdir (repo-url path branch)
  (let* ((repo-name (file-name-nondirectory (file-name-sans-extension repo-url)))
         (repo-exists (file-directory-p repo-name)))
    (if (not repo-exists)
        (progn
          (message "creating directory")
          (make-directory repo-name t))
      (message "repo already exists"))
    (let ((reporter (make-progress-reporter "sparse pulling patterns " 0 4)))
      (with-temp-buffer
        (cd repo-name)
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

        (unless (zerop (call-process "git" nil t nil "checkout" branch))
          (error "Failed to checkout branch %s" branch))
        (progress-reporter-update reporter 4)
        (message "Sparse pulled patterns for %s on branch %s" repo-name branch)))))

;; (fabric-gptel-sparse-checkout-subdir fabric-gptel--remote-url fabric-gptel--patterns-subdirectory "main")


(defun fabric-gptel-populate-patterns ()
  "filter out invalid directories"
  (setq fabric-gptel--patterns (cl-remove-if-not
                                (lambda (pattern)
                                  (f-exists-p (format "%s/%s/system.md" fabric-gptel--patterns-path pattern)))
                                (directory-files fabric-gptel--patterns-path nil "^[^.]" t))))

;; (fabric-gptel-populate-patterns)

(defun fabric-gptel-yield-prompt ()
  (interactive)
  (let ((pattern (completing-read "fabric-patterns: " fabric-gptel--patterns)))
    (with-temp-buffer (insert-file-contents (format "%s/%s/system.md" fabric-gptel--patterns-path pattern))
                      (buffer-string))))

(defun fabric-gptel-bootstrap ()
  (fabric-gptel-sparse-checkout-subdir fabric-gptel--remote-url fabric-gptel--patterns-subdirectory "main")
  (fabric-gptel-populate-patterns))

;; final usage
(defun fabric-gptel-send ()
  (interactive)
  (let ((gptel--system-message (fabric-gptel-yield-prompt)))
    (insert "\n\n")
    (gptel-send)))


(provide 'fabric-gpt.el)
;;; fabric-gpt.el ends here
