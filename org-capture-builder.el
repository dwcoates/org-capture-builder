;;; org-capture-builder --- a small utility for templates of org capture templates
;;
;; Author: Dodge W. Coates
;;
;;; Commentary:
;;
;; useful for easily building large amounts of templates that follow a default pattern
;;
;;;;;;;; Todo
;; nugget
;; notable stuff
;; quote
;;;;;;;;
;;
;;; Code:

(defconst org-template/meta-data "%a: on %T by %(system-name)")

(defun org-template/header (task &optional priority prompt more-tags)
  "Build a capture template header where TASK is an org todo marker.

PRIORITY, PROMPT, and MORE-TAGS can be strings, nil or otherwise non-nil.
If otherwise non-nil, they will use default settings."
  (setq task (downcase task))
  (let ((priority (if priority " %^{Priority|C|A|B|C|D|E}" ""))
        (prompt (if prompt (concat
                            " " (if (stringp prompt)
                                    prompt
                                  (concat "%^{" (capitalize task) "}")))
                  " %?"))
        (more-tags (if more-tags " %G" "")))
    (concat "* " priority " " (upcase task) prompt more-tags)))

(defun org-project-template-builder (header &optional tags scheduling body watermark properties)
  "Build a capture template with HEADER, TAGS, SCHEDULING, BODY, WATERMARK.

PROPERTIES and MORE-TAGS are additional optional capture components."
  (concat
   header " "
   (when tags
     (concat
      ":"
      (if (listp tags)
          (mapconcat 'identity tags ":")
        tags)
      ":"))
   "\n"
   (when properties
     (concat ":PROPERTIES:\n"
             (mapconcat (lambda (p) (concat ":" (car p) ": " (cadr p))) properties "\n")
             "\n:END:\n"))
   (when scheduling (if (stringp scheduling) (concat scheduling "\n") "%^{Schedule|SCHEDULE|DEADLINE|}: %T"))
   "\n"
   (when body (concat (if (stringp body) body "\t%?") "\n"))
   (if watermark watermark org-template/meta-data)))

(defun w-wrapper (SEC KEYWORD NULL)
  "This is used by `org-make-project-templates' where SEC KEYWORD \
NULL use for plist extraction."
  (if (plist-member SEC KEYWORD)
      (plist-get SEC KEYWORD)
    NULL))

(defun t-wrapper
    (SEC GLOBAL-TAGS PREFIX DESC LOC
         HD-TASK HD-PRIO HD-PROMPT
         HD-MORE-TAGS SCHEDULING BODY WATERMARK
         PROPERTIES MORE-TAGS)
  "Wrapper used by `org-make-project-templates' for org-project-template-builder."
  (list
   PREFIX
   (concat DESC " " (or HD-PROMPT (capitalize HD-TASK)))
   LOC
   (org-project-template-builder
    (w-wrapper SEC :headers (org-template/header
                             HD-TASK
                             HD-PRIO
                             HD-PROMPT
                             HD-MORE-TAGS))
    (append GLOBAL-TAGS (w-wrapper SEC :scheduling nil))
    (w-wrapper SEC :scheduling SCHEDULING)
    (w-wrapper SEC :body BODY)
    (w-wrapper SEC :watermark WATERMARK)
    (w-wrapper SEC :properties PROPERTIES)
    )
   (plist-get SEC :keywords)))

(defun org-make-project-templates (prefix global-tags desc loc &rest args)
  "A function that takes PREFIX, GLOBAL-TAGS, DESC, and LOC, ARGS, returning a list of captures."
  (let ((basic (plist-get args :basic))
        (study (plist-get args :study))
        (project (plist-get args :project)))
    (append
     (list
      (t-wrapper (plist-get basic :todo) global-tags (concat prefix "t") desc loc "todo" t nil nil t nil nil nil nil) ;; tasks
      (t-wrapper (plist-get basic :idea) global-tags (concat prefix "i") desc loc "idea" nil nil nil nil nil nil nil nil) ;; idea
      (t-wrapper (plist-get basic :note) global-tags (concat prefix "n") desc loc "note" nil nil nil nil nil nil nil nil)) ;; note
     (when study
       (list
        (t-wrapper (plist-get study :question) global-tags (concat prefix "U") desc loc "question" nil "Question" nil nil nil nil nil nil) ;; question
        (t-wrapper (plist-get study :quick-question) global-tags (concat prefix "u") desc loc "question" nil "Quick Question" nil nil nil nil nil nil) ;; quick question
        (t-wrapper (plist-get study :review) global-tags (concat prefix "r") desc loc "review" t nil nil nil nil nil nil nil) ;; refresh)
        (t-wrapper (plist-get study :learn) global-tags (concat prefix "l") desc loc "learn" t nil nil nil nil nil nil nil))) ;; learn)
     (when project
       (list
        (t-wrapper (plist-get project :issue) global-tags (concat prefix "s") desc loc "issue" t t nil t nil nil nil nil) ;; issue
        (t-wrapper (plist-get project :bug) global-tags (concat prefix "b") desc loc "bug" t t nil t nil nil nil nil) ;; bug
        (t-wrapper (plist-get project :feature) global-tags (concat prefix "f") desc loc "feature" t t nil t nil nil nil nil nil)))  ;; feature
     )))

(provide 'org-capture-builder)

;;; org-capture-builder.el ends here
