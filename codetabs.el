(require 'org)

(defvar codetabs-rx
  (rx (seq (group (seq "<div" (?? anything)
	   "class=\"" (?? (not "\"")) "org-src-container" (?? (not "\"")) "\"" (?? anything)
	   ">"))
      (?? anything)
      (seq "<pre" (?? anything)
	   "class=\""
	   (zero-or-more
	    (or
	     (seq "src-" (group (+ (not (or space "\"")))))
	     (seq (+? (not (or space "\"")))))
	    (?? space))
	   "\">")
      (group (+? anything))
      "</pre>"
      (+? anything)
      (group "</div>"))))

(defun codetabs-sibling-regions (string start end)
  "Checks if the substring of STRING between START and END (exclusive) contains only spaces or newlines (iterative)."
  (let ((length (length string))
        (only-whitespace-or-newline t))
    (when (or (< start 0) (> end length) (> start end))
      (error "Invalid indices: START=%d, END=%d, LENGTH=%d" start end length))
    (while (and (< start end) only-whitespace-or-newline)
      (let ((char (aref string start)))
        (unless (or (eq char ?\s) (eq char ?\n))
          (setq only-whitespace-or-newline nil)))
      (cl-incf start))
    only-whitespace-or-newline))

(defun codetabs-html-post-process (output backend info)
  "Identifies consecutive org-src-container divs in HTML output using regex."
  (when (eq backend 'html)
    (let ((search-position 0)
          (blocks '())
	  (grouped-blocks '()))
      (while (string-match codetabs-rx output search-position)
        (let ((start (match-beginning 0))
	      (end (match-end 4))
              (lang (match-string 2 output))
	      (code (match-string 3 output)))
	  (add-to-list 'blocks (list :lang lang :code code :start start :end end) t)
          (setq search-position end)))

      (let ((current-group '())
            (current-end 0))
        (dolist (block blocks)
          (let ((block-start (plist-get block :start)))
            (if (or (not current-group)
                    (codetabs-sibling-regions output current-end block-start))
		(add-to-list 'current-group block t)
		(when current-group
		  (add-to-list 'grouped-blocks current-group t)
                  (setq current-group (list block))))
            (setq current-end (plist-get block :end))))
        (when current-group
	  (add-to-list 'grouped-blocks current-group t)))

      (let ((new-output output)
	    (offset 0))
	(dolist (group grouped-blocks)
	  (if (> (length group) 1)
	      (let ((tab-headers "")
		    (tab-contents "")
		    (first-start (plist-get (car group) :start))
		    (last-end (plist-get (car (last group)) :end)))
		(dolist (block group)
		  (let ((lang (plist-get block :lang))
			(code (plist-get block :code)))
		    (setq tab-headers
			  (concat tab-headers
				  (format "<button class=\"tab-button\" data-lang=\"%s\">%s</button>"
					  lang lang)))
		    (setq tab-contents
			  (concat tab-contents
				  (format "<div class=\"tab-content\" id=\"%s\"><pre class=\"src src-%s\"><code>%s</code></pre></div>"
					  lang lang (string-trim code))))))
		(let ((tab-html (format "<div class=\"code-tabs\"><div class=\"tab-controls\">%s</div>%s</div>" tab-headers tab-contents))
		      (replace-start (+ offset first-start))
		      (replace-end (+ offset last-end)))

		  (setq replaced-length (- replace-end replace-start))
		  (setq replacement-length (length tab-html))

		  (setq new-output
			(concat (substring new-output 0 replace-start)
				tab-html
				(substring new-output replace-end)))

		  (setq offset (+ offset replacement-length (- replaced-length)))))))
	  new-output))))

(provide 'codetabs-html-post-process)
