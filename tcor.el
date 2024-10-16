;;; tcor.el --- ocr.space OCR -*- lexical-binding: t -*-
;; Copyright (C) 2016 Lars Magne Ingebrigtsen

;; Author: Lars Magne Ingebrigtsen <larsi@gnus.org>
;; Keywords: tools

;; tcor.el is free software; you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; tcor.el is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:

(require 'json)
(require 'mm-url)

(defvar tcor-api-key nil)
(defvar tcor-inhibit-split nil)

(defun tcor-upload (file &optional no-process language)
  (let* ((url-request-method "POST")
	 (boundary "--14--")
	 (url-request-extra-headers
	  `(("Content-Type" . ,(format "multipart/form-data; boundary=%s"
				       boundary))))
	 (url-request-data
	  (mm-url-encode-multipart-form-data
	   `(("apikey" . ,tcor-api-key)
	     ("isOverlayRequired" . "true")
	     ("scale" . "true")
	     ("language" . ,(or language "eng"))
	     ;;("OCREngine" . "2")
	     ("file" . (("filedata" . ,(with-temp-buffer
					 (set-buffer-multibyte nil)
					 (insert-file-contents file)
					 (unless no-process
					   (call-process-region
					    (point-min) (point-max)
					    "convert" t (current-buffer) nil
					    "-morphology" "close" "diamond"
					    "-" "png:-"))
					 ;; ocr.space has a 5MB image limit.
					 (when (> (buffer-size) 4500000)
					   (call-process-region
					    (point-min) (point-max)
					    "convert" t (current-buffer) nil
					    "-" "jpg:-"))
					 (buffer-string)))
			("name" . "file")
			("filename" . ,(file-name-nondirectory file)))))
	   boundary)))
    (with-current-buffer (or (ignore-errors
			       (url-retrieve-synchronously
				"https://apipro1.ocr.space/parse/image" t nil
				600))
			     (generate-new-buffer "*error*"))
      (goto-char (point-min))
      (prog1
	  (when (re-search-forward "\n\n" nil t)
	    (buffer-substring (point) (point-max)))
	(kill-buffer (current-buffer))))))

(defun tcor-language (file)
  (let* ((mag (car (last (file-name-split file) 3)))
	 (data (assq (intern mag) (tcor-magazines))))
    (cdr (assq 'lang (cdr data)))))

(defun tcor-ocr (file &optional no-process)
  (message "OCR-ing %s" file)
  (let ((data (tcor-upload file no-process (tcor-language file)))
	(coding-system-for-write 'utf-8)
	(got-error nil)
	json)
    (with-temp-buffer
      (insert (or data ""))
      (goto-char (point-min))
      (setq json (ignore-errors
		   (json-read)))
      (when (eq (cdr (assq 'IsErroredOnProcessing json))
		t)
	(message "%s: %s" (cdr (assq 'ErrorMessage json))
		 (ignore-errors
		   (cdr (assq 'ErrorMessage
			      (elt (cdr (assq 'ParsedResults json)) 0)))))
	(setq got-error t)
	(sleep-for 2))
      (when (and json
		 (eq (cdr (assq 'IsErroredOnProcessing json))
		     :json-false))
	(write-region (point-min) (point-max)
		      (replace-regexp-in-string "[.][^.]+\\'" ".json" file))))
    (let ((ocr (cdr (assq 'ParsedResults json))))
      (when (and ocr
		 (plusp (length ocr)))
	(with-temp-buffer
	  (let ((text (cdr (assq 'ParsedText (aref ocr 0)))))
	    (insert text)
	    (goto-char (point-min))
	    (while (re-search-forward "\r\n" nil t)
	      (replace-match " " t t))
	    (goto-char (point-min))
	    (while (re-search-forward " +" nil t)
	      (replace-match " " t t))
	    (goto-char (point-min))
	    (while (re-search-forward "- " nil t)
	      (replace-match "" t t))
	    (write-region (point-min) (point-max)
			  (replace-regexp-in-string
			   "[.][^.]+\\'" ".txt" file))))))
    got-error))

(defun tcor--identify (file)
  (with-temp-buffer
    (set-buffer-multibyte nil)
    (call-process "identify" nil t nil "-format" "%w %h"
		  (expand-file-name file))
    (buffer-string)))

(defun tcor-image-size (file)
  (let ((size (mapcar #'string-to-number (split-string (tcor--identify file)))))
    (cons (car size) (cadr size))))

(defun tcor-resize-images ()
  (dolist (file (directory-files-recursively
		 "~/src/kwakk/magscan/" "page.*jpg$"))
    (let (size)
      (when (and (not (file-exists-p (file-name-with-extension file "json")))
		 (or (> (car (setq size (tcor-image-size file))) 3000)
		     (> (cdr size) 5000)))
	(message "Resizing %s" file)
	(call-process "mogrify" nil nil nil "-resize" "3000x5000>"
		      (expand-file-name file))))))

(defun tcor-fix-big-images ()
  (dolist (file (directory-files-recursively
		 "~/src/kwakk/magscan/" "page.*jpg$"))
    (when (and (not (file-exists-p (file-name-with-extension file "json")))
	       (> (file-attribute-size (file-attributes file))
		  (* 5000 1024)))
      (message "Resizing for kb %s" file)
      (when (file-exists-p "/tmp/tcor.jpg")
	(delete-file "/tmp/tcor.jpg"))
      (cl-loop for size from 3000 downto 1000 by 100
	       do (call-process "convert" nil nil nil
				"-resize" (format "%dx" size)
				(expand-file-name file) "/tmp/tcor.jpg")
	       while (> (file-attribute-size (file-attributes "/tmp/tcor.jpg"))
			(* 5000 1024)))
      (when (file-exists-p "/tmp/tcor.jpg")
	(rename-file "/tmp/tcor.jpg" file t)))))

(defun tcor-resize ()
  (tcor-resize-images)
  (tcor-fix-big-images))

(defun tcor-all ()
  (let ((errors nil))
    (dolist (file (directory-files-recursively
		   "~/src/kwakk/magscan/" "page.*jpg$"))
      (unless (file-exists-p
	       (replace-regexp-in-string "[.][^.]+\\'" ".json" file))
	(when (tcor-ocr file t)
	  (setq errors t))))
    errors))

(defun tcor-sharded (instance instances)
  (dolist (file (directory-files-recursively
		 "~/src/kwakk/magscan/" "page.*jpg$"))
    (when (and (not (file-exists-p (file-name-with-extension file "json")))
	       (= (mod (string-to-number (sha1 file) 16) instances) instance))
      (tcor-ocr file t)))
  (kill-emacs))

(defun tcor-sort-by-size (files)
  (nreverse
   (sort files (lambda (f1 f2)
		 (< (file-attribute-size (file-attributes f1))
		    (file-attribute-size (file-attributes f2)))))))

(defun tcor-import-files (mag &optional prefix)
  (tcor-import-file mag prefix
		    (directory-files "." t "jp2.*zip\\|[.]cb[rz]$\\|pdf$")))

(defun tcor-import-file (mag &optional prefix files)
  (dolist (file (tcor-sort-by-size (or files (dired-get-marked-files nil current-prefix-arg))))
    (dolist (dir
	     (cond
	      ((string-match "pdf$" file)
	       (tcor-unpack-pdf (list file) mag prefix))
	      ((string-match "[.]cb[rz]$" file)
	       (tcor-unpack-cbr (list file) mag prefix))
	      ((string-match "jp2.*zip" file)
	       (tcor-unpack-jp2 (list file) mag prefix))
	      ((file-directory-p file)
	       (tcor-unpack-directories (list file) mag prefix))))
      (when (file-exists-p dir)
	(let ((first (car (directory-files dir t "page.*jpg"))))
	  (if (not first)
	      (message "Error: No pages in %s" dir)
	    (find-file first))))))
  (tcor-find-empty-directories))

(defun tcor-unpack (mag &optional prefix)
  (dolist (file (tcor-sort-by-size
		 (directory-files default-directory
				  t
				  "[.]cb[zr]$\\|[.]pdf$\\|jp2.*[.]zip$")))
    (tcor-import-file mag prefix (list file))))

(defun tcor-issue-match (mag)
  (if (member mag '("CBG" "CSN"))
      "\\b[0-9][0-9][0-9][0-9]\\b"
    "\\b[0-9][0-9][0-9]\\b"))

(defun tcor-unpack-cbr (dir mag &optional prefix)
  (let ((subs nil))
    (dolist (cbr (tcor-sort-by-size (if (consp dir)
					dir
				      (directory-files dir t "[.]cb[zr]$"))))
      (when (string-match (tcor-issue-match mag) cbr)
	(message "%s" cbr)
	(let* ((issue (match-string 0 cbr))
	       (sub (expand-file-name (concat (or prefix "") issue)
				      (format "~/src/kwakk/magscan/%s/" mag))))
	  (unless (file-exists-p sub)
	    (make-directory sub t)
	    (let ((default-directory sub))
	      (if (string-match "[.]cbr$" cbr)
		  (with-environment-variables (("LANG" "C.UTF-8"))
		    (call-process "unrar" nil nil nil "e" cbr))
		(unless (zerop (call-process "unzip" nil nil nil "-j" cbr))
		  (call-process "7z" nil nil nil "e" cbr)))
	      ;; Delete Macos resource files.
	      (dolist (file (directory-files sub t "\\`[.]_"))
		(delete-file file))
	      (cl-loop
	       with i = 0
	       for file in (let ((case-fold-search t))
			     (directory-files sub t "[.]\\([jJ][pP][eE]?[gG]\\|[wW][eE][bB][pP]\\)\\'"))
	       for size = (tcor-image-size file)
	       do
	       (call-process "chmod" nil nil nil "u+rw" file)
	       (if (or tcor-inhibit-split
		       (< (car size) (cdr size)))
		   ;; Vertical
		   (if (let ((case-fold-search t))
			 (string-match "[.]jpe?g\\'" file))
		       (rename-file file
				    (expand-file-name (format "page-%03d.jpg"
							      (cl-incf i))
						      sub))
		     (call-process "convert" nil nil nil
				   (expand-file-name file)
				   (expand-file-name (format "page-%03d.jpg"
							     (cl-incf i))
						     sub)))
		 ;; Horizontal.
		 (call-process
		  "convert" nil nil nil
		  "-crop" (format "%sx%s+0+0" (/ (car size) 2) (cdr size))
		  (file-truename file)
		  (expand-file-name (format "page-%03d.jpg" (cl-incf i))
				    sub))
		 (call-process
		  "convert" nil nil nil
		  "-crop" (format "%sx%s+%s-0"
				  (/ (car size) 2)
				  (cdr size)
				  (/ (car size) 2))
		  ;; "1949x3064+1949-0"
		  (file-truename file)
		  (expand-file-name (format "page-%03d.jpg" (cl-incf i))
				    sub))
		 (delete-file file))))
	    (push sub subs)))))
    subs))

(defun tcor-unpack-cbr-unnumered (dir)
  (cl-loop for cbr in (directory-files dir t "[.]cb[zr]$")
	   for issue from 1
	   do
	   (progn
	     (message "%s" cbr)
	     (let* ((sub (expand-file-name (format "WS%03d" issue)
					   "~/src/kwakk/magscan/W/")))
	       (unless (file-exists-p sub)
		 (make-directory sub)
		 (let ((default-directory sub))
		   (if (string-match "[.]cbr$" cbr)
		       (call-process "unrar" nil nil nil "e" cbr)
		     (call-process "unzip" nil nil nil "-j" cbr))
		   (cl-loop for page from 1
			    for file in (directory-files
					 sub t "[.][jJ][pP][eE]?[gG]$")
			    do (rename-file file
					    (expand-file-name
					     (format "page-%03d.jpg" page)
					     sub)))))))))

(defun tcor-find-archive-org-urls (search-url)
  (interactive "sURL: ")
  (pop-to-buffer "*archive*")
  (erase-buffer)
  (let ((data
	 (with-current-buffer (url-retrieve-synchronously search-url)
	   (goto-char (point-min))
	   (search-forward "\n\n")
	   (prog1
	       (json-parse-buffer)
	     (kill-buffer (current-buffer))))))
    (cl-loop for hit across
	     (gethash "hits"
		      (gethash "hits"
			       (gethash "body"
					(gethash "response" data))))
	     collect
	     (cons (gethash "identifier" (gethash "fields" hit))
		   (gethash "title" (gethash "fields" hit))))))

(defun tcor-unpack-jp2 (dir mag &optional prefix)
  (let ((subs nil))
    (dolist (zip (if (consp dir)
		     dir
		   (directory-files dir t "[.]zip$")))
      (when (string-match (tcor-issue-match mag) zip)
	(message "%s" zip)
	(let* ((issue (match-string 0 zip))
	       (sub (expand-file-name (concat (or prefix "") issue)
				      (format "~/src/kwakk/magscan/%s/" mag))))
	  (unless (file-exists-p sub)
	    (make-directory sub t)
	    (let ((default-directory sub))
	      (call-process "unzip" nil nil nil "-j" zip)
	      ;; Delete Macos resource files.
	      (dolist (file (directory-files sub t "\\`[.]_"))
		(delete-file file))
	      (cl-loop
	       with i = 0
	       for jp-file in (directory-files sub t "[.][jJ][pP]2$")
	       for file = (file-name-with-extension jp-file ".jpg")
	       do (call-process "convert" nil nil nil jp-file file)
	       for size = (tcor-image-size file)
	       do (when (> (car size) 3000)
		    (call-process "gm" nil nil nil
				  "mogrify" "-resize" "3000x"
				  file)
		    (setq size (tcor-image-size file)))
	       do (if (or tcor-inhibit-split
			  (< (car size) (cdr size)))
		      ;; Vertical
		      (rename-file file
				   (expand-file-name (format "page-%03d.jpg"
							     (cl-incf i))
						     sub))
		    ;; Horizontal.
		    (call-process
		     "convert" nil nil nil
		     "-crop" (format "%sx%s+0+0" (/ (car size) 2) (cdr size))
		     (file-truename file)
		     (expand-file-name (format "page-%03d.jpg" (cl-incf i))
				       sub))
		    (call-process
		     "convert" nil nil nil
		     "-crop" (format "%sx%s+%s-0"
				     (/ (car size) 2)
				     (cdr size)
				     (/ (car size) 2))
		     ;; "1949x3064+1949-0"
		     (file-truename file)
		     (expand-file-name (format "page-%03d.jpg" (cl-incf i))
				       sub))
		    (delete-file file))
	       (delete-file jp-file)))
	    (push sub subs)))))
    subs))

(defun tcor-unpack-pdf (dir mag &optional prefix inhibit-split)
  (let ((subs nil))
    (dolist (pdf (if (consp dir)
		     dir
		   (directory-files dir t "[.]pdf$")))
      (when (string-match (tcor-issue-match mag) pdf)
	(let* ((issue (match-string 0 pdf))
	       (sub (expand-file-name (concat (or prefix "") issue)
				      (format "~/src/kwakk/magscan/%s/" mag))))
	  (message "%s" pdf)
	  (unless (file-exists-p sub)
	    (make-directory sub t)
	    (let ((default-directory sub))
	      (call-process "pdftoppm" nil nil nil
			    "-jpeg"
			    "-rx" "250"
			    "-ry" "250"
			    pdf
			    "PDF")
	      (cl-loop
	       with i = 0
	       for file in (directory-files sub t "[.]jpg$")
	       for size = (progn
			    (when nil
			      (call-process "mogrify" nil nil nil
					    "-rotate" "270"
					    file))
			    (tcor-image-size file))
	       do (if (or tcor-inhibit-split
			  inhibit-split
			  (< (car size) (cdr size)))
		      ;; Vertical
		      (rename-file file
				   (expand-file-name (format "page-%03d.jpg"
							     (cl-incf i))
						     sub))
		    ;; Horizontal.
		    (call-process
		     "convert" nil nil nil
		     "-crop" (format "%sx%s+0+0" (/ (car size) 2) (cdr size))
		     (file-truename file)
		     (expand-file-name (format "page-%03d.jpg" (cl-incf i))
				       sub))
		    (call-process
		     "convert" nil nil nil
		     "-crop" (format "%sx%s+%s-0"
				     (/ (car size) 2)
				     (cdr size)
				     (/ (car size) 2))
		     ;; "1949x3064+1949-0"
		     (file-truename file)
		     (expand-file-name (format "page-%03d.jpg" (cl-incf i))
				       sub))
		    (delete-file file))))
	    (push sub subs)))))
    subs))

(defun tcor-unpack-directories (dir mag &optional prefix)
  (dolist (dir (if (consp dir)
		   dir
		 (directory-files dir t "[0-9]")))
    (when (string-match (tcor-issue-match mag) dir)
      (let* ((issue (match-string 0 dir))
	     (sub (expand-file-name (concat (or prefix "") issue)
				    (format "~/src/kwakk/magscan/%s/" mag)))
	     (i 0))
	(message "%s" dir)
	(unless (file-exists-p sub)
	  (make-directory sub t)
	  (dolist (file (directory-files dir t "webp\\|jpeg\\|png\\'"))
	    (call-process
	     "convert" nil nil nil
	     (file-truename file)
	     (expand-file-name (format "page-%03d.jpg" (cl-incf i)) sub))))))))

(defun tcor-magazines ()
  (with-temp-buffer
    (insert-file-contents "~/src/kwakk/magazines.json")
    (json-read)))

(defun tcor-list-missing-issues (mag &optional browse first archive)
  (let* ((data (tcor-magazines))
	 (elem (cdr (assq (intern mag) data))))
    (unless elem
      (user-error "No such mag: %s" mag))
    (let ((name (cdr (assq 'name elem))))
      (pop-to-buffer "*missing*")
      (erase-buffer)
      (let ((issues (directory-files (format "~/src/kwakk/magscan/%s/" mag) nil "[0-9]$")))
	(when-let ((last (cdr (assq 'last elem))))
	  (setq issues (nconc issues (list (format "%03d" (1+ last))))))
	(let ((groups (seq-group-by (lambda (issue)
				      (car (tcor-segment-issue issue mag)))
				    issues)))
	  (cl-loop for (group . issues) in groups
		   do (cl-loop with start = 1
			       for num in (mapcar (lambda (issue)
						    (cdr (tcor-segment-issue issue mag)))
						  issues)
			       do (cl-loop for i from start upto (1- num)
					   for string = (string-clean-whitespace
							 (format "%s %s %03d" name group i))
					   when (or (not first)
						    (> i first))
					   do
					   (insert string "\n")
					   (when browse
					     (when archive
					       (tcor-browse-url-firefox (format "https://archive.org/search?query=%s"
										string))
					       (when (string-match " 0" string)
						 (tcor-browse-url-firefox (format "https://archive.org/search?query=%s"
										  (replace-regexp-in-string " 0+" " " string)))))
					     (tcor-browse-url-firefox (format "https://annas-archive.org/search?q=%s"
									      string))
					     (when (string-match " 0" string)
					       (tcor-browse-url-firefox (format "https://annas-archive.org/search?q=%s"
										(replace-regexp-in-string " 0+" " " string))))
					     (sleep-for 5)))
			       (setq start (1+ num)))))))))

(defun tcor-browse-url-firefox (url &optional new-window)
  "Ask the Firefox WWW browser to load URL.
Defaults to the URL around or before point.  Passes the strings
in the variable `browse-url-firefox-arguments' to Firefox.

Interactively, if the variable `browse-url-new-window-flag' is non-nil,
loads the document in a new Firefox window.  A non-nil prefix argument
reverses the effect of `browse-url-new-window-flag'.

If `browse-url-firefox-new-window-is-tab' is non-nil, then
whenever a document would otherwise be loaded in a new window, it
is loaded in a new tab in an existing window instead.

Non-interactively, this uses the optional second argument NEW-WINDOW
instead of `browse-url-new-window-flag'."
  (interactive (browse-url-interactive-arg "URL: "))
  (setq url (browse-url-encode-url url))
  (let* ((process-environment (browse-url-process-environment)))
    (apply #'start-process
           (concat "firefox " url) nil
           "/usr/local/bin/firefox/firefox"
           (append
            browse-url-firefox-arguments
            (if (browse-url-maybe-new-window new-window)
		(if browse-url-firefox-new-window-is-tab
		    '("-new-tab")
		  '("-new-window")))
            (list url)))))

(defun tcor-segment-issue (issue mag)
  (let* ((len (if (member mag '(CBG CSN)) 4 3))
	 (num (substring issue (- (length issue) len))))
    (if (string-match "^[0-9]+$" num)
	(cons (substring issue 0 (- (length issue) len))
	      (string-to-number num))
      (and (string-match "[A-Z]+" issue)
	   (cons (match-string 0 issue)
		 (string-to-number (substring issue (match-end 0))))))))

(defun tcor-delete-extra-pages (mag)
  (switch-to-buffer "*extra*")
  (dolist (dir (directory-files (format "~/src/kwakk/magscan/%s/" mag) t "[0-9]$"))
    (dolist (file (seq-take (nreverse (directory-files dir t "page.*jpg")) 2))
      (erase-buffer)
      (insert-image (create-image file nil nil :max-height (frame-pixel-height)))
      (when (y-or-n-p (format "Delete %s? " file))
	(delete-file file)
	(when (file-exists-p (file-name-with-extension file "txt"))
	  (delete-file (file-name-with-extension file "txt")))
	(when (file-exists-p (file-name-with-extension file "json"))
	  (delete-file (file-name-with-extension file "json")))))))

(defun tcor-split-into-issues (start pages)
  (let ((files (directory-files "." nil "page.*jpg")))
    (cl-loop for num from start
	     for dir = (format "../SPL-%03d/" num)
	     while files
	     do
	     (make-directory dir)
	     (cl-loop for i from 1 upto pages
		      do (rename-file (pop files)
				      (format "%spage-%03d.jpg" dir i))))))

(defun tcor-list-dirs-without-pages ()
  (dolist (mag (directory-files "~/src/kwakk/magscan/" t "[A-Z]"))
    (dolist (issue (directory-files mag t "[0-9]$"))
      (unless (directory-files issue nil "page-001.jpg")
	(message "Missing %s" issue)))))

(defun tcor-index (&optional mags force)
  (dolist (mag (or mags (directory-files "~/src/kwakk/magscan/" nil "[A-Z]")))
    (let* ((dir (concat "~/src/kwakk/magscan/" mag))
	   (newest (car
		    (sort (directory-files-recursively dir "[.]txt\\'\\|issues.json")
			  #'file-newer-than-file-p)))
	   (omega (expand-file-name (format "~/src/kwakk/omega.db/%s/docdata.glass" mag))))
      (when (or force
		(not (file-exists-p omega))
		(file-newer-than-file-p newest omega))
	(message "Indexing %s" mag)
	(let ((default-directory (expand-file-name "~/src/kwakk/magscan/")))
	  (call-process "rsync" nil nil nil
			"-a" "--delete"
			"--include=*/" "--include=*.txt" "--exclude=*"
			(expand-file-name dir)
			(concat "/tmp/" mag))
	  (call-process "rm" nil nil nil
			"-rf" (expand-file-name (concat "~/src/kwakk/omega.db/" mag)))
	  (call-process "omindex" nil nil nil
			"-S" "--db" (expand-file-name (concat "~/src/kwakk/omega.db/" mag))
			"--url" "/"
			"-E" "8000"
			(concat "/tmp/" mag)))))))

(defun tcor-count-pages ()
  (dolist (mag (directory-files "~/src/kwakk/magscan/" nil "[A-Z]"))
    (let* ((dir (concat "~/src/kwakk/magscan/" mag))
	   (newest (car
		    (sort (append (directory-files-recursively dir "[.]txt\\'")
				  (and (file-exists-p (expand-file-name "suppress-covers.txt" dir))
				       (list (expand-file-name "suppress-covers.txt" dir)))
				  (and (file-exists-p (expand-file-name "double-issues.txt" dir))
				       (list (expand-file-name "double-issues.txt" dir))))
			  #'file-newer-than-file-p))))
      (when (or (not (file-exists-p (expand-file-name "issues.json" dir)))
		(file-newer-than-file-p newest (expand-file-name "issues.json" dir)))
	(message "Counting %s" mag)
	(magscan-count-pages dir)))))

(defun tcor-gather-data ()
  (let ((table (make-hash-table :test #'equal)))
    (dolist (mag (directory-files "~/src/kwakk/magscan/" nil "[A-Z]"))
      (let* ((dir (concat "~/src/kwakk/magscan/" mag "/"))
	     (pages (length (directory-files-recursively dir "page.*jpg")))
	     (issues (length (directory-files dir nil "[0-9]$"))))
	(setf (gethash mag table)
	      (list (cons 'issues issues)
		    (cons 'pages pages)))))
    (with-temp-buffer
      (insert (json-encode table))
      (write-region (point-min) (point-max) "~/src/kwakk/data.json"))))  

(defun tcor-pre-ocr ()
  (tcor-resize))

(defun tcor-post-ocr ()
  (magscan-covers-and-count)
  (tcor-count-pages)
  (tcor-gather-data)
  (tcor-index))

(defun tcor-find-credits-pages (&optional hours mag start)
  (let ((files nil))
    (dolist (mag (if mag
		     (list (expand-file-name mag "~/src/kwakk/magscan/"))
		   (directory-files "~/src/kwakk/magscan/" t "[A-Z]")))
      (dolist (issue (directory-files mag t "[0-9]$"))
	(dolist (file (nreverse (seq-take (nreverse (directory-files issue t "page.*jpg")) 4)))
	  (when (and (< (file-attribute-size (file-attributes file)) (* 500 1024))
		     (or (not hours)
			 (> (float-time (file-attribute-modification-time (file-attributes issue)))
			    (- (float-time) (* hours 60 60)))))
	    (push file files)))))
    (setq files (nreverse files))
    (catch 'stop
      (let ((i (or start 0)))
	(while (< i (length files))
	  (setq file (elt files i))
	  (switch-to-buffer "*pages*")
	  (erase-buffer)
	  (insert-image (create-image file nil nil :max-height (truncate (* (frame-pixel-height) 0.9))))
	  (let ((answer (car (read-multiple-choice (format "Delete %s?" file)
						   '((?y "Yes")
						     (?n "No")
						     (?p "Prev")
						     (?s "Stop"))))))
	    (cond
	     ((= answer ?y)
	      (delete-file file)
	      (when (file-exists-p (file-name-with-extension file "txt"))
		(delete-file (file-name-with-extension file "txt")))
	      (when (file-exists-p (file-name-with-extension file "json"))
		(delete-file (file-name-with-extension file "json")))
	      (when (file-exists-p (expand-file-name "issues.json" mag))
		(delete-file (expand-file-name "issues.json" mag)))
	      (cl-incf i))
	     ((= answer ?n)
	      (cl-incf i))
	     ((= answer ?s)
	      (throw 'stop i))
	     (t
	      (cl-decf i)))))))))

(defun tcor-list-all-mags (&optional category)
  (switch-to-buffer "*list*")
  (erase-buffer)
  (dolist (mag (with-temp-buffer
		 (insert-file-contents "~/src/kwakk/magazines.json")
		 (json-read)))
    (when (or (not category)
	      (equal category (cdr (assq 'cat (cdr mag)))))
      (if (memq (car mag) '(MIF MIM MII MIP))
	  (dolist (name (cdr (assq 'prefix (cdr mag))))
	    (insert (cdr name) ", "))
	(insert (cdr (assq 'name (cdr mag))) ", ")))))

(defun tcor-link-all-mags ()
  (switch-to-buffer "*list*")
  (erase-buffer)
  (dolist (mag (with-temp-buffer
		 (insert-file-contents "~/src/kwakk/magazines.json")
		 (json-read)))
    (unless (cdr (assq 'hidden (cdr mag)))
      (if (memq (car mag) '(MIF MIM MII MIP))
	  (dolist (name (cdr (assq 'prefix (cdr mag))))
	    (insert (format "<a href='https://kwakk.info/%s/'>%s</a>, "
			    (downcase (symbol-name (car mag)))
			    (cdr name))))
	(insert (format "<a href='https://kwakk.info/%s/'>%s</a>, "
			(downcase (symbol-name (car mag)))
			(cdr (assq 'name (cdr mag)))))))))


(defun tcor-do ()
  (tcor-pre-ocr)
  (call-process "~/src/tcor.el/orc-shard")
  ;; Loop while there's errors.
  (cl-loop for i from 0 upto 10
	   while (tcor-all)
	   do (sleep-for 1))
  (tcor-post-ocr))

;; https://www.comicsfanzines.co.uk/s-z/speakeasy-81-120
(defun tcor-fetch-views (url)
  (let ((dom
	 (with-current-buffer (url-retrieve-synchronously url)
	   (goto-char (point-min))
	   (search-forward "\n\n")
	   (prog1
	       (libxml-parse-html-region (point) (point-max))
	     (kill-buffer (current-buffer))))))
    dom))

(defun tcor-parse-views (dom)
  (cl-loop for link in (dom-by-tag dom 'a)
	   for url = (dom-attr link 'href)
	   for issue = (string-trim (dom-texts (dom-parent dom (dom-parent dom link))))
	   when (and url
		     (string-match "drive.google.*/view" url)
		     (not (zerop (length issue))))
	   collect (list url (replace-regexp-in-string
			      "[^a-zA-Z0-9]" "-"
			      (replace-regexp-in-string
			       "\\([0-9]+\\) +\\([()/0-9]+\\)\\'"
			       "\\1\\2" issue)))))

(defun tcor-view-urls (url)
  (let ((list (tcor-parse-views (tcor-fetch-views url)))
	(table (make-hash-table :test #'equal)))
    (with-temp-buffer
      (dolist (elem list)
	(unless (gethash (car elem) table)
	  (insert (cadr elem) " " (car elem) "\n")
	  (setf (gethash (car elem) table) t)))
      (write-region (point-min) (point-max) "/tmp/urls.txt" t))))

(defun tcor-find-empty-directories ()
  (interactive)
  (dolist (mag (directory-files "~/src/kwakk/magscan/" t "[A-Z]"))
    (dolist (issue (directory-files mag t "[0-9]$"))
      (unless (file-exists-p (expand-file-name "page-001.jpg" issue))
	(find-file issue)))))

(defun tcor-view (file)
  (interactive (list (car (dired-get-marked-files nil current-prefix-arg))))
  (cond
   ((string-match "[.]pdf\\'" file)
    (call-process "xxpdf" nil nil nil file))
   ((string-match "z$\\|zip$" file)
    (with-temp-buffer
      (call-process "unzip" nil t nil "-l" file)
      (goto-char (point-min))
      (forward-line 6)
      (message "%s" (buffer-substring (point) (pos-eol 5)))))
   ((string-match "r\\'" file)
    (with-temp-buffer
      (call-process "unrar" nil t nil "l" file)
      (goto-char (point-min))
      (forward-line 8)
      (message "%s" (buffer-substring (point) (pos-eol 6)))))))

(keymap-global-set "H-i" #'tcor-view)

(defun tcor-find-discontinuities ()
  (dolist (mag (directory-files "~/src/kwakk/magscan/" t "[A-Z]"))
    (dolist (issue (directory-files mag t "[0-9]$"))
      (let* ((pages (sort (directory-files issue nil "page.*jpg\\'")
			  #'string-version-lessp))
	     (last (car (last pages))))
	(unless (= (length pages)
		   (and (string-match "[0-9]+" last)
			(string-to-number (match-string 0 last))))
	  (find-file issue)
	  (error "Num %s; last %s" (length pages) last))))))

(provide 'tcor)

;; List 20 Italian-language magazines and fanzines about comic books (but exclude comic book magazines as well as online magazines).

;;; tcor.el ends here
