(defvar canvas-base-url "https://canvas.myuni.edu/api/v1/courses/xxxx/"
"The base url of the course for the canvas API")

(defvar canvas-authorization-token
  (with-temp-buffer
    (insert-file-contents "~/.local/canvas_token")
    (buffer-string))
  "Authorization token for canvas")

(defvar canvas-local-base-diretory
  (expand-file-name "~/Documents/teaching/your-course-name")
  "Local base directory")

(defvar canvas-log-buffer-name
  "*canvas log*"
  "Buffer name for canvas log")

(defun canvas--log (object)
  "Construct a string represetation of the object and write the string to the log buffer."
  (with-current-buffer (get-buffer-create canvas-log-buffer-name)
    (end-of-buffer)
    (print object (current-buffer))
    ))

(cl-defun canvas--generic-error-handler (&key data &key error-thrown &allow-other-keys)
  (canvas--log error-thrown)
  (canvas--log data)
  (message "Error!  See the canvas log buffer"))

(cl-defun canvas--generic-success-handler (&key data &allow-other-keys)
  (canvas--log data)
  (when-let* ((id (assoc-default 'id data)))
    (org-set-property "canvas__id" (format "%s" id)))
  (message "Success.  See canvas log buffer for return data."))

(cl-defun canvas--dump-properties (&key data &allow-other-keys)
  (message "Get success.")
  (canvas--log (print data))
  (dolist (kv data)
    (org-set-property (format "canvas_%s" (car kv))
		      (string-replace "\n" "\\n" (format "%s" (cdr kv))))))

(defun canvas-time (timestamp)
  "Format org mode time stamp as ISO 8601"
  (format-time-string  "%FT%T" (date-to-time timestamp)))

(defun canvas-get-file (filename)
  "Return the contents of the file as a string."
  (with-temp-buffer
    (insert-file-contents filename)
    (buffer-string)))

(defun canvas-get-title ()
  "Get title from ITEM"
  (org-entry-get nil "ITEM"))

(defun canvas-get-entry-as-html ()
  "Return the body of the subtree as an html string"
  (let ((temp-file (make-temp-file "canvas-html")))
    (org-export-to-file 'html temp-file nil t nil t)
    (with-temp-buffer
      (insert-file-contents temp-file)
      (buffer-string))))

(defun canvas--construct-data ()
  (let ((data nil))
    (dolist (kv (org-entry-properties nil 'standard))
      (message (car kv))
      (when-let* ((key (and (string-match "^canvas_\\(.*\\)" (car kv))
			    (match-string-no-properties 1 (car kv))))
		  (val (cdr kv)))
	(push (cons (downcase key)
		    (if (listp (read val)) ;; if S-expression
			(eval (read val))  ;; evaluate
		      val))                ;; else as-is
	      data)))
    data))

(defun canvas-act-on-subtree ()
  "The default action is POST to the url indicated by the property canvas__url.  If this property is nil, do nothing.  All properties of the form canvas_PROP are sent as data. If canvas__id is set, then it is appended to canvas__url and the request is PUT instead of POST.  This has the effect of updating canvas.  If canvas__action is set to DELETE or GET, then the properties are ignored and a DELETE or GET is issued."
  (interactive)
  (let ((full-url (org-entry-get nil "canvas__full_url"))
	(url (org-entry-get nil "canvas__url"))
	(id (org-entry-get nil "canvas__id"))
	(action (org-entry-get nil "canvas__action"))
	(data nil))
    (if (and (not full-url) (not url))
	(message "No canvas URL.  Doing nothing.")
      (setq url (or full-url
		    (if id
			(format "%s%s/%s" canvas-base-url url id)
		      (format "%s%s" canvas-base-url url))))
      (canvas--log (format "URL: %s" url))
      (setq action (or action
		       (and id "PUT")
		       "POST"))
      (canvas--log (format "Action: %s" action))
      (when (member (upcase action) '("PUT" "POST"))
	(setq data (canvas--construct-data))
	(canvas--log (format "Data: %s" data)))
      (if (y-or-n-p (format "%s at %s?" action url))
	  (request url
	    :type action
	    :headers `(("Authorization" . ,(concat "Bearer " canvas-authorization-token)))
	    :parser 'json-read
	    :data data
	    :error 'canvas--generic-error-handler
	    :success (if-let* ((given (org-entry-get nil "canvas__success_handler")))
			 (eval (read given))
		       (if (member action '("GET" "DELETE"))
			   'canvas--dump-properties
			 'canvas--generic-success-handler)))
	(message "Aborted.")
	))))

(defun canvas-upload-file-at-point ()
  "Upload the file at point.
   The location is the same as the local location relative to the local base directory."
(interactive)
(when-let* ((full-filename
	       (or (and (derived-mode-p 'dired-mode)
			(dired-get-filename))
		   (thing-at-point 'existing-filename))))
  (let* ((base-relative-filename
	    (file-relative-name full-filename canvas-local-base-diretory))
	   (filename (file-name-nondirectory base-relative-filename))
	   (folder (file-name-directory base-relative-filename)))
    (if (y-or-n-p (format "Uploading %s in folder %s." filename folder))
	(canvas--log (format "Uploading %s in folder %s." filename folder))
      (request (concat canvas-base-url "files")
	:type "POST"
	:data `((name . ,filename)
		(parent_folder_path . ,folder))
	:parser 'json-read
	:headers 
	`(("Authorization" . ,(concat "Bearer " canvas-authorization-token)))
	:success (canvas--upload-handler full-filename)
	:error 'canvas--generic-error-handler)))))

(defun canvas--upload-handler (filename)
;; For some reason, I cannot make it work with the request.el library.
;; So I am going to use straight up curl.
(message filename)
(cl-function
 (lambda (&key data &allow-other-keys)
   (let* ((params (assoc-default 'upload_params data))
	    (url (assoc-default 'upload_url data))
	    (temp-file (make-temp-file "curl")))
     (call-process "curl"
		     nil
		     `((:file ,temp-file) nil)
		     nil
		     url
		     (format "-F \'%s\'" (request--urlencode-alist params))
		     (format "-F \'file=@\"%s\"\'" filename))
     (with-current-buffer (get-buffer-create canvas-log-buffer-name)
	 (insert-file-contents temp-file))
     (let ((data (json-read-file temp-file)))
	 (request (assoc-default 'location data)
	   :type "GET"
	   :headers
	   `(("Authorization" . ,(concat "Bearer " canvas-authorization-token)))
	   :parser 'json-read
	   :success
	   (cl-function
	    (lambda (&key data &allow-other-keys)
	      (kill-new (assoc-default 'url data))
	      (message "Upload success.  URL in kill ring.")))
	   :error 'canvas--generic-error-handler))))))
