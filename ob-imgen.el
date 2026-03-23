;;; ob-imgen.el --- Org-babel backend for AI image generation -*- lexical-binding: t -*-

;; Author: Pablo
;; Version: 0.1.0
;; Package-Requires: ((emacs "27.1") (org "9.0") (plz "0.7"))
;; Keywords: org, babel, image, ai, dalle, stable-diffusion

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
;;   #+begin_src imgen :model dall-e-3 :size 1792x1024 :quality hd :file fox.png
;;   A red fox sitting on a snowy mountain at sunset
;;   #+end_src
;;
;; Header args:
;;   :model    Model name (default: dall-e-3)
;;   :size     Image dimensions (default: 1024x1024)
;;   :quality  standard | hd (default: standard, dall-e-3 only)
;;   :style    vivid | natural (default: vivid, dall-e-3 only)
;;   :file     Output filename (default: auto-generated in org file's directory)
;;   :results  file (default and only supported value)
;;
;; Configuration:
;;   Set OPENAI_API_KEY environment variable, or customize
;;   `ob-imgen-api-key-fn' to fetch it from another source (e.g. auth-source).
;;
;; Backends:
;;   Currently supports OpenAI (dall-e-2, dall-e-3).
;;   Set `ob-imgen-backend' to add support for other providers.

;;; Code:

(require 'ob)
(require 'plz)
(require 'json)

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

(defcustom ob-imgen-default-quality "standard"
  "Default image quality (dall-e-3 only: standard or hd)."
  :type '(choice (const "standard") (const "hd"))
  :group 'ob-imgen)

(defcustom ob-imgen-default-style "vivid"
  "Default image style (dall-e-3 only: vivid or natural)."
  :type '(choice (const "vivid") (const "natural"))
  :group 'ob-imgen)

(defcustom ob-imgen-api-key-fn #'ob-imgen--env-api-key
  "Function called with no arguments to retrieve the API key."
  :type 'function
  :group 'ob-imgen)

(defcustom ob-imgen-display-inline t
  "If non-nil, display the generated image inline after execution."
  :type 'boolean
  :group 'ob-imgen)

;;; Default header args

(defvar org-babel-default-header-args:imgen
  '((:results . "file")
    (:exports . "results"))
  "Default header arguments for imgen source blocks.")

;;; API key helpers

(defun ob-imgen--env-api-key ()
  "Return OPENAI_API_KEY from environment."
  (or (getenv "OPENAI_API_KEY")
      (user-error "ob-imgen: OPENAI_API_KEY is not set")))

;;; Output file helpers

(defun ob-imgen--output-file (params)
  "Determine output file path from PARAMS."
  (let ((explicit (cdr (assq :file params))))
    (if (and explicit (not (string-empty-p explicit)))
        (expand-file-name explicit (file-name-directory (buffer-file-name)))
      (expand-file-name
       (format "imgen-%s.png" (format-time-string "%Y%m%d_%H%M%S"))
       (file-name-directory (buffer-file-name))))))

;;; OpenAI backend

(defun ob-imgen--openai-generate (prompt params)
  "Call OpenAI image API with PROMPT and PARAMS.  Return image URL."
  (let* ((model   (or (cdr (assq :model   params)) ob-imgen-default-model))
         (size    (or (cdr (assq :size    params)) ob-imgen-default-size))
         (quality (or (cdr (assq :quality params)) ob-imgen-default-quality))
         (style   (or (cdr (assq :style   params)) ob-imgen-default-style))
         (api-key (funcall ob-imgen-api-key-fn))
         (body    (let ((args `((prompt . ,prompt)
                                (model  . ,model)
                                (n      . 1)
                                (size   . ,size)
                                (response_format . "url"))))
                    ;; dall-e-3 supports quality and style; dall-e-2 does not
                    (when (string= model "dall-e-3")
                      (push `(quality . ,quality) args)
                      (push `(style   . ,style)   args))
                    (json-encode args))))
    (condition-case err
        (let* ((response (plz 'post "https://api.openai.com/v1/images/generations"
                            :headers `(("Authorization" . ,(concat "Bearer " api-key))
                                       ("Content-Type"  . "application/json"))
                            :body body
                            :as #'json-read))
               (url (alist-get 'url (aref (alist-get 'data response) 0))))
          url)
      (plz-error
       (user-error "ob-imgen: API error: %s" (error-message-string err))))))

;;; Main execute function

(defun org-babel-execute:imgen (body params)
  "Generate an image from BODY prompt using PARAMS header args."
  (let* ((prompt  (string-trim body))
         (outfile (ob-imgen--output-file params)))
    (when (string-empty-p prompt)
      (user-error "ob-imgen: prompt is empty"))
    (message "ob-imgen: generating image…")
    (let ((url (ob-imgen--openai-generate prompt params)))
      (message "ob-imgen: downloading…")
      (url-copy-file url outfile t)
      (message "ob-imgen: done → %s" (file-name-nondirectory outfile))
      (when ob-imgen-display-inline
        (run-with-timer 0.3 nil #'org-display-inline-images nil t))
      outfile)))

;;; No sessions

(defun org-babel-prep-session:imgen (_session _params)
  (user-error "ob-imgen: sessions are not supported"))

(provide 'ob-imgen)
;;; ob-imgen.el ends here
