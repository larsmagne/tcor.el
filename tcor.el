;;; tcor.el --- The Comics Journal OCR -*- lexical-binding: t -*-
;; Copyright (C) 2016 Lars Magne Ingebrigtsen

;; Author: Lars Magne Ingebrigtsen <larsi@gnus.org>
;; Keywords: fun, comics

;; tcj.el is free software; you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; tcj.el is distributed in the hope that it will be useful, but
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

(defun tcor-upload (file)
  (let* ((url-request-method "POST")
	 (boundary "--14--")
	 (url-request-extra-headers
	  `(("Content-Type" . ,(format "multipart/form-data; boundary=%s"
				       boundary))))
	 (url-request-data
	  (mm-url-encode-multipart-form-data
	   `(("apikey" . ,tcor-api-key)
	     ("file" . (("filedata" . ,(with-temp-buffer
					 (set-buffer-multibyte nil)
					 (insert-file-contents file)
					 (buffer-string)))
			("name" . "file")
			("filename" . ,(file-name-nondirectory file)))))
	   boundary))
	 (result (url-retrieve-synchronously
		  "https://apipro2.ocr.space/parse/image" t))
	 json)
    (unless result
      (error "No response from OCR"))
    (with-current-buffer result
      (goto-char (point-min))
      (when (re-search-forward "\n\n" nil t)
	(setq json (ignore-errors (json-read))))
      (kill-buffer (current-buffer)))
    json))

(defun tcor-ocr (file &optional real-file)
  (message "Scanning %s" file)
  (let* ((json (tcor-upload file))
	 (ocr (cdr (assq 'ParsedResults json)))
	 (coding-system-for-write 'iso-8859-1))
    (when ocr
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
			(tcor-text-file (or real-file file))))))))

(defun tcor-text-file (file)
  (expand-file-name
   (replace-regexp-in-string
    "\\.jpg$" ".txt" (file-name-nondirectory file))
   "~/tcj/text"))
	  
(defun tcor-update ()
  (dolist (file (directory-files "~/tcj/" t "\\.jpg\\'"))
    (let ((new nil))
      (unless (file-exists-p (tcor-text-file file))
	(let ((size 90))
	  (while (> (file-attribute-size (file-attributes (or new file)))
		    (* 1024 1024))
	    (message "Reducing...")
	    (setq new "/tmp/reduced.jpg")
	    (call-process "convert" nil nil nil file "-resize"
			  (format "%d%%" size)
			  new)
	    (decf size 10)))
	(tcor-ocr (or new file) file)))))

(defun tcor-create-title-cdb ()
  (with-temp-buffer
    (let* ((regexp "tcj-\\([0-9]+\\)-\\([0-9]+\\).txt")
	   (files (directory-files "~/tcj/text/" nil regexp))
	   (table (make-hash-table)))
      (dolist (file files)
	(when (string-match regexp file)
	  (let ((issue (string-to-number (match-string 1 file)))
		(page (string-to-number (match-string 2 file))))
	    (setf (gethash issue table)
		  (max (gethash issue table 0) page)))))
      (dolist (file files)
	(when (string-match regexp file)
	  (let ((issue (string-to-number (match-string 1 file)))
		(page (string-to-number (match-string 2 file))))
	    (insert
	     (format
	      "/%s The&nbsp;Comics&nbsp;Journal&nbsp;issue&nbsp;%d,&nbsp;%s\n"
	      file issue
	      (cond
	       ((= page 1)
		"front&nbsp;cover")
	       ((= page 2)
		"inside&nbsp;front&nbsp;cover")
	       ((= page (gethash issue table))
		"back&nbsp;cover")
	       ((= page (1- (gethash issue table)))
		"inside&nbsp;back&nbsp;cover")
	       (t
		(format "page&nbsp;%d" (- page 2))))))))))
    (write-region (point-min) (point-max) "/tmp/titles.input")))

;;; tcor.el ends here
