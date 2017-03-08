# org-capture-builder.el

use `org-make-project-templates` to build a list of capture templates. 

The prototype is

    (org-make-project-templates prefix global-tags desc loc &args)
    
Sample input: 

    (org-make-project-templates "ab" 
                                '("myCoolTag") 
                                "Test" 
                                '(id "abcd-efg-123") 
                                :basic '(:note (:watermark "")) 
                                :study '(:quick-question (:keywords (:immediate-finish t)))
                                :project t)
                                
Sample output:

    (("abt" "Test Todo"
      (id "abcd-efg-123")
      "*  %^{Priority|C|A|B|C|D|E} TODO %? :myCoolTag:\n%^{Schedule|SCHEDULE|DEADLINE|}: %T\n%a: on %T by %(system-name)")
     ("abi" "Test Idea"
      (id "abcd-efg-123")
      "*  IDEA %? :myCoolTag:\n\n%a: on %T by %(system-name)")
     ("abn" "Test Note"
      (id "abcd-efg-123")
      "*  NOTE %? :myCoolTag:\n\n")
     ("abU" "Test Question"
      (id "abcd-efg-123")
      "*  NEXT Question :myCoolTag:\n\n%a: on %T by %(system-name)")
     ("abu" "Test Quick Question"
      (id "abcd-efg-123")
      "*  NEXT Quick Question :myCoolTag:\n\n%a: on %T by %(system-name)" :immediate-finish t)
     ("abr" "Test Review"
      (id "abcd-efg-123")
      "*   Review :myCoolTag:\n\n%a: on %T by %(system-name)")
     ("abl" "Test Learn"
      (id "abcd-efg-123")
      "*  %^{Priority|C|A|B|C|D|E} LEARN %? :myCoolTag:\n\n%a: on %T by %(system-name)")
      ("abs" "Test Issue"
     (id "abcd-efg-123")
      "*  %^{Priority|C|A|B|C|D|E} ISSUE %^{Issue} :myCoolTag:\n%^{Schedule|SCHEDULE|DEADLINE|}: %T\n%a: on %T by %(system-name)")
     ("abb" "Test Bug"
      (id "abcd-efg-123")
      "*  %^{Priority|C|A|B|C|D|E} BUG %^{Bug} :myCoolTag:\n%^{Schedule|SCHEDULE|DEADLINE|}: %T\n%a: on %T by %(system-name)")
     ("abf" "Test Feature"
      (id "abcd-efg-123")
      "*  %^{Priority|C|A|B|C|D|E} FEATURE %^{Feature} :myCoolTag:\n%^{Schedule|SCHEDULE|DEADLINE|}: %T\n%a: on %T by %(system-name)"))
    
