;;; ob-imgen.el --- Org-babel backend for AI image generation -*- lexical-binding: t -*-

;; Author: Pablo Munoz Haro <contact@slashpablo.com>
;; Version: 0.1.0
;; Package-Requires: ((emacs "27.1") (org "9.0"))
;; Keywords: org, babel, image, ai, dalle, stable-diffusion
;; URL: https://github.com/slashpablo/ob-imgen

;;; Commentary:
;;
;; Org-babel backend that generates images from text prompts.
;;
;; Usage:
;;
;;   #+begin_src imgen
;;   A red fox sitting on a snowy mountain at sunset
;;   #+end_src
;;
;;   #+begin_src imgen :model gpt-image-1 :size 1024x1024 :quality medium :file fox.png
;;   A red fox sitting on a snowy mountain at sunset
;;   #+end_src
;;
;; Header args:
;;   :model      Model name (default: dall-e-3)
;;   :size       Image dimensions (default: 1024x1024)
;;   :quality    Quality setting — model-dependent:
;;                 dall-e-3:    standard | hd  (default: standard)
;;                 gpt-image-1: low | medium | high  (default: medium)
;;   :style      vivid | natural (dall-e-3 only, default: vivid)
;;   :file       Output filename (default: auto-generated)
;;   :output-dir Directory for auto-generated files (overrides `ob-imgen-output-directory')
;;   :results    file (default and only supported value)
;;
;; Configuration:
;;   Set OPENAI_API_KEY environment variable, or customize
;;   `ob-imgen-api-key-fn' to fetch it from another source (e.g. auth-source).
;;   Set `ob-imgen-output-directory' to save auto-generated images to a
;;   dedicated directory instead of alongside the org file.
;;
;; Backends:
;;   Currently supports OpenAI (dall-e-2, dall-e-3, gpt-image-1).
;;   Set `ob-imgen-backend' to add support for other providers.

;;; Code:

(require 'ob)
(require 'url)
(require 'json)
(require 'base64)

;;; Customization

(defgroup ob-imgen nil
  "Org-babel image generation."
  :group 'org-babel
  :prefix "ob-imgen-")

(defcustom ob-imgen-default-model "dall-e-3"
  "Default image generation model."
  :type 'string
  :group 'ob-imgen)

(defcustom ob-imgen-default-size "1024x1024"
  "Default image dimensions."
  :type 'string
  :group 'ob-imgen)

(defcustom ob-imgen-default-quality-dalle3 "standard"
  "Default image quality for dall-e-3 (standard or hd)."
  :type '(choice (const "standard") (const "hd"))
  :group 'ob-imgen)

(defcustom ob-imgen-default-quality-gpt-image-1 "medium"
  "Default image quality for gpt-image-1 (low, medium, or high)."
  :type '(choice (const "low") (const "medium") (const "high"))
  :group 'ob-imgen)

(defcustom ob-imgen-default-style "vivid"
  "Default image style (dall-e-3 only: vivid or natural)."
  :type '(choice (const "vivid") (const "natural"))
  :group 'ob-imgen)

(defcustom ob-imgen-api-key-fn #'ob-imgen--env-api-key
  "Function called with no arguments to retrieve the API key."
  :type 'function
  :group 'ob-imgen)

(defcustom ob-imgen-output-directory nil
  "Directory where auto-generated image files are saved.
When nil, images are saved alongside the org file.
Explicit :file header args are still resolved relative to the org file."
  :type '(choice (const :tag "Same as org file" nil)
                 (directory :tag "Custom directory"))
  :group 'ob-imgen)

(defcustom ob-imgen-display-inline t
  "If non-nil, display the generated image inline after execution."
  :type 'boolean
  :group 'ob-imgen)

;;; Default header args

(defvar org-babel-default-header-args:imgen
  '((:results    . "file")
    (:exports    . "results")
    (:output-dir . nil))
  "Default header arguments for imgen source blocks.")

;;; API key helpers

(defun ob-imgen--env-api-key ()
  "Return OPENAI_API_KEY from environment."
  (or (getenv "OPENAI_API_KEY")
      (user-error "ob-imgen: OPENAI_API_KEY is not set")))

;;; Output file helpers

(defun ob-imgen--output-file (params)
  "Determine output file path from PARAMS."
  (let* ((explicit   (cdr (assq :file       params)))
         (output-dir (cdr (assq :output-dir params)))
         (org-dir    (file-name-directory (buffer-file-name))))
    (if (and explicit (not (string-empty-p explicit)))
        (expand-file-name explicit org-dir)
      (let ((dir (cond
                  ((and output-dir (not (string-empty-p output-dir)))
                   (expand-file-name output-dir))
                  (ob-imgen-output-directory
                   (expand-file-name ob-imgen-output-directory))
                  (t org-dir))))
        (make-directory dir t)
        (expand-file-name
         (format "imgen-%s.png" (format-time-string "%Y%m%d_%H%M%S"))
         dir)))))

;;; HTTP helpers

(defun ob-imgen--curl-post (body api-key)
  "POST JSON BODY to the OpenAI image API using curl.
Returns the parsed JSON response alist, or signals an error."
  (let ((req-file  (make-temp-file "ob-imgen-req"  nil ".json"))
        (resp-file (make-temp-file "ob-imgen-resp" nil ".json")))
    (unwind-protect
        (progn
          (let ((coding-system-for-write 'utf-8-unix))
            (write-region body nil req-file nil 'silent))
          (let ((exit (call-process
                       "curl" nil (list :file resp-file) nil
                       "--silent"
                       "-X" "POST"
                       "https://api.openai.com/v1/images/generations"
                       "-H" (concat "Authorization: Bearer " api-key)
                       "-H" "Content-Type: application/json"
                       "--data-binary" (concat "@" req-file))))
            (unless (zerop exit)
              (user-error "ob-imgen: curl failed (exit %d)" exit))
            (with-temp-buffer
              (insert-file-contents resp-file)
              (let* ((data (json-read-from-string (buffer-string)))
                     (err  (alist-get 'error data)))
                (when err
                  (user-error "ob-imgen: API error: %s" (alist-get 'message err)))
                data))))
      (ignore-errors (delete-file req-file))
      (ignore-errors (delete-file resp-file)))))

;;; OpenAI backend

(defun ob-imgen--gpt-image-1-p (model)
  "Return non-nil if MODEL is a gpt-image variant."
  (string-prefix-p "gpt-image-" model))

(defun ob-imgen--openai-generate (prompt params outfile)
  "Call OpenAI image API with PROMPT and PARAMS.  Write result to OUTFILE."
  (let* ((model   (or (cdr (assq :model   params)) ob-imgen-default-model))
         (size    (or (cdr (assq :size    params)) ob-imgen-default-size))
         (quality (cdr (assq :quality params)))
         (style   (or (cdr (assq :style   params)) ob-imgen-default-style))
         (gpt-image-1-p (ob-imgen--gpt-image-1-p model))
         (default-quality (if gpt-image-1-p
                              ob-imgen-default-quality-gpt-image-1
                            ob-imgen-default-quality-dalle3))
         (quality (or (and quality (not (string-empty-p quality)) quality)
                      default-quality))
         (api-key (funcall ob-imgen-api-key-fn))
         (body
          (let ((args `((prompt . ,prompt)
                        (model  . ,model)
                        (n      . 1)
                        (size   . ,size))))
            (cond
             (gpt-image-1-p
              ;; gpt-image-1: quality yes, no style, always returns b64_json
              (push `(quality . ,quality) args))
             ((string= model "dall-e-3")
              ;; dall-e-3: quality yes, style yes, response url
              (push `(quality         . ,quality) args)
              (push `(style           . ,style)   args)
              (push `(response_format . "url")    args))
             (t
              ;; dall-e-2: no quality/style, response url
              (push `(response_format . "url") args)))
            (json-encode args))))
    (let* ((response (ob-imgen--curl-post body api-key))
           (item     (aref (alist-get 'data response) 0)))
      (if gpt-image-1-p
          (let ((b64 (alist-get 'b64_json item)))
            (with-temp-file outfile
              (set-buffer-multibyte nil)
              (insert (base64-decode-string b64))))
        (url-copy-file (alist-get 'url item) outfile t)))))

;;; Main execute function

(defun org-babel-execute:imgen (body params)
  "Generate an image from BODY prompt using PARAMS header args."
  (let* ((prompt  (string-trim body))
         (outfile (ob-imgen--output-file params)))
    (when (string-empty-p prompt)
      (user-error "ob-imgen: prompt is empty"))
    (message "ob-imgen: generating image…")
    (ob-imgen--openai-generate prompt params outfile)
    (message "ob-imgen: done → %s" (file-name-nondirectory outfile))
    (when ob-imgen-display-inline
      (run-with-timer 0.3 nil #'org-display-inline-images nil t))
    outfile))

;;; No sessions

(defun org-babel-prep-session:imgen (_session _params)
  (user-error "ob-imgen: sessions are not supported"))

(provide 'ob-imgen)
;;; ob-imgen.el ends here
