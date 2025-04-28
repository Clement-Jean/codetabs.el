(require 'org)

(defun codetabs-parse-list-from-string (string-representation)
  "Parses a list from its string representation.
  STRING-REPRESENTATION should be a valid Elisp list string."
  (with-temp-buffer
    (insert string-representation)
    (goto-char (point-min))
    (read (current-buffer))))

(defun codetabs-extract-elements-from-string-list (string-list)
  "Parses a list of lists from a string and extracts elements into separate strings."
  (let ((parsed-list (codetabs-parse-list-from-string string-list))
        (first-elements "")
        (second-elements "")
        (third-elements ""))
    (dolist (sublist parsed-list)
      (when (>= (length sublist) 1)
        (setq first-elements (concat first-elements (format "%s," (car sublist)))))
      (when (>= (length sublist) 2)
        (setq second-elements (concat second-elements (format "%s," (nth 1 sublist)))))
      (when (>= (length sublist) 3)
        (setq third-elements (concat third-elements (format "%s," (nth 2 sublist))))))
    (list (string-trim-right first-elements ",")
          (string-trim-right second-elements ",")
          (string-trim-right third-elements ","))))

(defun codetabs-src-block-advice (oldfun src-block contents info)
  (let ((old-ret (funcall oldfun src-block contents info))
	(lang (org-element-property :language src-block))
	(name (org-element-property :name src-block))
	(skip (org-export-read-attribute :attr_codetabs src-block :skip))
	(emphasize (org-export-read-attribute :attr_codetabs src-block :emphasize)))
    (if (not skip)
	(with-temp-buffer
	  (insert old-ret)
	  (let* ((html (libxml-parse-html-region (point-min) (point)))
		 (div (elt (elt html 2) 2)))
	    (push `(lang . ,lang) (elt div 1))
	    (when name
	      (push `(name . ,name) (elt div 1)))
	    (when emphasize
	      (let* ((res (codetabs-extract-elements-from-string-list emphasize))
		     (starts (nth 0 res))
		     (ends (nth 1 res))
		     (classes (nth 2 res)))
		(push `("emphasize-start" . ,starts) (elt div 1))
		(push `("emphasize-end" . ,ends) (elt div 1))
		(push `("emphasize-class" . ,classes) (elt div 1))))
	    (erase-buffer)
	    (shr-dom-print div)
	    (replace-regexp-in-string (rx (seq " " "<span")) "<span" (buffer-string))))
      old-ret)))

(provide 'codetabs-src-block-advice)
