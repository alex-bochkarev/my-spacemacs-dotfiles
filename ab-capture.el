;;; ab-capture.el --- my personal org-capture set up

;; Copyright (c) 2023
;; Author: Alexey Bochkarev

;;; Keybindings

  (global-set-key (kbd "H-c") 'org-capture)

;;; capture templates
  (setq org-capture-templates
        (quote (

;;;; Repo-specific templates
              ("r" "=== Repo-specific templates === ")
              ("rt" "New TODO (repo-specific)" entry (file+headline (lambda () (concat (projectile-project-root)  org-projectile-per-project-filepath)) "Current project TODOs")
               "* TODO %?\n%U\n%a\n" :prepend t)
              ("rc" "Changelog entry (repo-specific)" entry(file+headline (lambda () (concat (projectile-project-root) "CHANGELOG.org")) "Running changelog")
               "* %u: %? \n%a\n" :prepend t)
              ("rn" "General note / assumptions / etc (repo-specific)" entry (file+headline (lambda () (concat (projectile-project-root)  org-projectile-per-project-filepath)) "Notes")
               "* %?\n%U\n%a\n" :prepend t)
              ("rf" "Further note / TODOs" entry (file+headline (lambda () (concat (projectile-project-root)  org-projectile-per-project-filepath)) "Further")
               "* %?\n%U\n%a\n" :prepend t)

;;;; (PKB) Project-specific templates
              ("p" "=== Project-specific templates (PKB/notes) === ")
              ("pt" "New TODO (project-specific)" entry (file+headline (lambda () (concat pkb-project-notes-dir (projectile-project-name) "/" pkb-project-note-file)) "Current project TODOs")
               "* TODO %?\n%U\n%a\n" :prepend t)
              ("pl" "Project Log entry" entry(file+headline (lambda () (concat  pkb-project-notes-dir (projectile-project-name) "/" pkb-project-log-file)) "Running results")
               "* %U: %? \n%a\n" :prepend t)
              ("pN" "Notation note (project-specific)" item (file+headline (lambda () (concat  pkb-project-notes-dir (projectile-project-name) "/" pkb-project-notation-file)) "Table of symbols (notation)")
               "- %?\n")
              ("pn" "General note / assumptions / etc (project-specific)" entry (file+headline (lambda () (concat pkb-project-notes-dir (projectile-project-name) "/" pkb-project-note-file)) "Notes")
               "* %?\n%U\n%a\n" :prepend t)
              ("pf" "Further research note (project-specific)" entry (file+headline (lambda () (concat pkb-project-notes-dir (projectile-project-name) "/" pkb-project-note-file)) "Further research and TODOs")
               "* %?\n%U\n%a\n" :prepend t)

;;;; email captures
              ("e" "Email backlog (email.org)" entry (file+headline org-email-file "Backlog")
               "* TODO %a\n%T" :prepend t :empty-lines 1 :immediate-finish t)
              ("E" "Email backlog (email.org, edit first)" entry (file+headline org-email-file "Backlog")
               "* TODO %a\n%T" :prepend t :empty-lines 1)

;;;; current TODOs and SOMEDAYs
              ("t" "Current TODO (current.org)" entry (file+headline org-current-file "Daily inbox")
               "* TODO %? \n%a\n" :prepend t)
              ("s" "=== Someday TODOs (someday.org) ============== ")
              ("sw" "Weekend TODO" entry (file+headline org-someday-file "Weekend")
               "* TODO %? \n" :prepend t)
              ("ss" "Someday TODO" entry (file+headline org-someday-file "Someday")
               "* TODO %? \n" :prepend t)
              ("n" "Current/fleeting note (current.org)" entry (file+headline org-current-file "Daily inbox")
               "* %? \n%a\n" :prepend t)

;;;; bookmarks / readme-s
              ("b" "========== [b] Bookmarks / readme notes====================")
              ("br" "Research-related entry" entry (file+headline org-readme-file "Research-related notes")
               "* %a \n Captured: %U\n %?\n")
              ("bg" "General note (link)" entry (file+headline org-readme-file "General notes")
               "* %a\n Captured: %U\n %?\n\n")

;;;; other top-level templates
              ("w" "Web note idea (blog)" entry (file+headline org-blog-file "Ideas for notes")
               "* %?\n%U\n")
              ("d" "A distraction!" entry (file org-distractions-file)
               "* %?\n Link: %a\n Captured: %U\n")
              ("R" "Daily result" entry (file+olp org-current-file "Daily inbox" "Results")
               "* %? \n%a\n%U\n" :prepend t)
              ("a" "A question TBD w/Anita" entry (file+headline org-current-file "TBD w/Anita")
               "* %?\n Captured: %U\n\n")
              ("k" "Things to do with kids" entry (file+olp org-current-file "Kids" "Activity")
               "* %? \n%a\n%U\n" :prepend t))))

;; refiling config
(setq org-refile-targets '((org-agenda-files :maxlevel . 3)))
