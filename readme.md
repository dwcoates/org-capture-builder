# org-capture-builder.el

use `org-make-project-templates` to build a list of capture templates. 

The prototype is

    (org-make-project-templates prefix global-tags desc loc &args)
    
Sample: 

    (org-make-project-templates "ch" 
                                '("emacs") 
                                "Emacs" 
                                '(id "f19d9f24-eff6-4ecb-aedb-7ec4d1e1f97e") 
                                :basic t
                                :study '(:question '(:scheduling nil :watermark "This is my capture. The End.")
                                :project nil))
                                
Sample output:

    (("cht" 
      "Emacs Todo" 
      (id "f19d9f24-eff6-4ecb-aedb-7ec4d1e1f97e") 
      "*  %^{Priority|C|A|B|C|D|E} TODO %? :emacs:
          %^{Schedule|SCHEDULE|DEADLINE|}: %T

       Captured on %T by %(system-name) by: %a" 
       nil) 
       ("chi" 
       "Emacs Idea" 
       (id "f19d9f24-eff6-4ecb-aedb-7ec4d1e1f97e") 
       "*  IDEA %? :emacs:

        Captured on %T by %(system-name) by: %a" 
        nil) 
        ("chn" "Emacs Note" (id "f19d9f24-eff6-4ecb-aedb-7ec4d1e1f97e") "*  NOTE %? :emacs:


        Captured on %T by %(system-name) by: %a" 
        nil) 
        
        ("chU" 
        "Emacs Question" 
        (id "f19d9f24-eff6-4ecb-aedb-7ec4d1e1f97e")
        "*  QUESTION Question :emacs:

        Captured on %T by %(system-name) by: %a" 
        nil)
         
        ("chu"
        "Emacs Quick Question"
        (id "f19d9f24-eff6-4ecb-aedb-7ec4d1e1f97e")
        "*  QUESTION Quick Question :emacs:

        Captured on %T by %(system-name) by: %a" 
        nil)
          
        ("chr"
        "Emacs Review"
        (id "f19d9f24-eff6-4ecb-aedb-7ec4d1e1f97e")
        "*  %^{Priority|C|A|B|C|D|E} REVIEW %? :emacs:

        Captured on %T by %(system-name) by: %a" 
        nil)
           
        ("chl"
        "Emacs Learn"
        (id "f19d9f24-eff6-4ecb-aedb-7ec4d1e1f97e")
        "*  %^{Priority|C|A|B|C|D|E} LEARN %? :emacs:
        
        Captured on %T by %(system-name) by: %a" 
        nil))
