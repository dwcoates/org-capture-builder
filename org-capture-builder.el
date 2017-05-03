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

(add-hook 'org-capture-prepare-finalize-hook 'counsel-org-tag)

(defconst org-template/meta-data "%a: on %T by %(system-name)")

(defvar org-make-additional-project-templates nil
  "Function accepting the same arguments as `org-make-project-templates'.
It returns, like `org-make-project-templates', a list of capture descriptions.
It will be called within `org-make-project-templates' as a way to extend the
function.  This function should look much like `org-make-project-templates'")

(defun org-template/header (task &optional priority prompt more-tags)
  "Build a capture template header where TASK is an org todo marker.

PRIORITY, PROMPT, and MORE-TAGS can be strings, nil or otherwise non-nil.
If otherwise non-nil, they will use default settings."
  (setq task (downcase task))
  (let ((priority (if priority " [#%^{Priority|C|A|B|C|D|E}]" ""))
        (prompt (if prompt (concat
                            " " (if (stringp prompt)
                                    prompt
                                  (concat "%^{" (capitalize task) "}")))
                  " %? "))
        (more-tags (if more-tags " %G" "")))
    (concat "* " (upcase task) priority prompt more-tags)))

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
   (when scheduling
     (if (stringp scheduling)
         (concat scheduling "\n")
       "%^{Schedule|SCHEDULED|DEADLINE|}: %t\n"))
   (when body (concat "\n" (if (stringp body) body "\t%?") "\n"))
   "\n"
   (if watermark watermark org-template/meta-data)))

(defun w-wrapper (SEC KEYWORD NULL)
  "This is used by `org-make-project-templates' where SEC KEYWORD \
NULL use for plist extraction."
  (if (plist-member SEC KEYWORD)
      (plist-get SEC KEYWORD)
    NULL))

(defun t-wrapper
    (SEC GLOBAL-TAGS PREFIX DESC LOC BASE-TAGS
         HD-TASK HD-PRIO HD-PROMPT
         HD-MORE-TAGS SCHEDULING BODY WATERMARK
         PROPERTIES MORE-TAGS &optional KEYWORDS)
  "Wrapper used by `org-make-project-templates' for org-project-template-builder."
  (append
   (list
    ;; prefix
    PREFIX
    ;; description
    (concat DESC (when DESC " ") (if (stringp HD-PROMPT) HD-PROMPT (capitalize HD-TASK)))
    ;; type
    'entry
    ;; location
    LOC
    ;; template
    (org-project-template-builder
     (w-wrapper SEC :headers (org-template/header
                              HD-TASK
                              HD-PRIO
                              (if (stringp HD-PROMPT) (concat "%^{" HD-PROMPT "}") HD-PROMPT)))
     (append GLOBAL-TAGS BASE-TAGS (w-wrapper SEC :scheduling nil))
     (w-wrapper SEC :scheduling SCHEDULING)
     (w-wrapper SEC :body BODY)
     (w-wrapper SEC :watermark WATERMARK)
     (w-wrapper SEC :properties PROPERTIES)
     HD-MORE-TAGS))
  (append (plist-get SEC :keywords) KEYWORDS)))

(defun org-make-project-templates (prefix global-tags desc loc &rest args)
  "A function that takes PREFIX, GLOBAL-TAGS, DESC, and LOC, ARGS, returning a list of captures."
  (let ((basic (plist-get args :basic))
        (study (plist-get args :study))
        (project (plist-get args :project))
        (custom-captures (plist-get args :custom-captures)))
    (cons
     (list prefix desc)
     (append
      (list
       ;; tasks
       (t-wrapper (plist-get basic :todo) global-tags (concat prefix "t") desc loc nil
                  "todo" t t nil t t nil nil nil)

       ;; idea
       (t-wrapper (plist-get basic :idea) global-tags (concat prefix "i") desc loc '("idea")
                  "idea" nil t   nil nil t nil nil nil)

       ;; note
       (t-wrapper (plist-get basic :note) global-tags (concat prefix "n") desc loc '("notes")
                  "note" nil t t nil t nil nil nil)

       ;; quote
       (t-wrapper (plist-get basic :quote) global-tags (concat prefix "\"") desc loc '("quote")
                  "" nil "Quote"  nil nil t nil nil nil)

       ;; nugget
       (t-wrapper (plist-get basic :nugget) global-tags (concat prefix ".") desc loc '("nugget")
                  "" nil  nil  nil nil nil nil nil nil '(:immediate-finish t)))
      (when study
        (list
         ;; question
         (t-wrapper (plist-get study :question) global-tags (concat prefix "U") desc loc '("question")
                    "next" nil "Question" t "SCHEDULED: %t" t nil nil nil)

         ;; quick question
         (t-wrapper (plist-get study :quick-question) global-tags (concat prefix "u") desc loc '("question")
                    "next" nil "Quick Question" nil "SCHEDULED: %t" nil nil nil nil '(:immediate-finish t))

         ;; refresh
         (t-wrapper (plist-get study :review) global-tags (concat prefix "z") desc loc '("review" "drill")
                    "" nil "Quiz" t "SCHEDULED: %t" t nil nil  nil)

         ;; review
         (t-wrapper (plist-get study :review) global-tags (concat prefix "r") desc loc '("review")
                    "review" t    "Review" t "SCHEDULED: %t" t nil nil  nil)

         ;; learn
         (t-wrapper (plist-get study :learn) global-tags (concat prefix "l") desc loc '("study")
                    "learn" t "Learn" t "SCHEDULED: %t" t nil nil nil)))
      (when project
        (list
         ;; issue
         (t-wrapper (plist-get project :issue)   global-tags (concat prefix "s") desc loc '("issue")
                    "issue"   t t t "SCHEDULED: %t" t nil nil nil)

         ;; bug
         (t-wrapper (plist-get project :bug)     global-tags (concat prefix "b") desc loc '("bug")
                    "bug"     t t t "SCHEDULED: %t" t nil nil nil)

         ;; feature
         (t-wrapper (plist-get project :feature) global-tags (concat prefix "f") desc loc '("feature")
                    "feature" t t t "SCHEDULED: %t" t nil nil nil)))

      custom-captures

      (when (functionp 'org-make-additional-project-templates)
        (funcall 'org-make-additional-project-templates prefix global-tags desc loc args))))
    ))



(provide 'org-capture-builder)

;;; org-capture-builder.el ends here
