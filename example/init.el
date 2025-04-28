(load-file "../codetabs.el")
(advice-add 'org-html-src-block :around #'codetabs-src-block-advice)
