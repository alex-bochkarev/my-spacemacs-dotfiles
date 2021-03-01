;; -*- mode: emacs-lisp -*-
;; This file is loaded by Spacemacs at startup.
;; It must be stored in your home directory.

(defun dotspacemacs/layers ()
  "Configuration Layers declaration.
You should not put any user code in this function besides modifying the variable
values."
  (setq-default
   ;; Base distribution to use. This is a layer contained in the directory
   ;; `+distribution'. For now available distributions are `spacemacs-base'
   ;; or `spacemacs'. (default 'spacemacs)
   dotspacemacs-distribution 'spacemacs
   ;; Lazy installation of layers (i.e. layers are installed only when a file
   ;; with a supported type is opened). Possible values are `all', `unused'
   ;; and `nil'. `unused' will lazy install only unused layers (i.e. layers
   ;; not listed in variable `dotspacemacs-configuration-layers'), `all' will
   ;; lazy install any layer that support lazy installation even the layers
   ;; listed in `dotspacemacs-configuration-layers'. `nil' disable the lazy
   ;; installation feature and you have to explicitly list a layer in the
   ;; variable `dotspacemacs-configuration-layers' to install it.
   ;; (default 'unused)
   dotspacemacs-enable-lazy-installation 'unused
   ;; If non-nil then Spacemacs will ask for confirmation before installing
   ;; a layer lazily. (default t)
   dotspacemacs-ask-for-lazy-installation t
   ;; If non-nil layers with lazy install support are lazy installed.
   ;; List of additional paths where to look for configuration layers.
   ;; Paths must have a trailing slash (i.e. `~/.mycontribs/')
   dotspacemacs-configuration-layer-path '("~/.spacemacs.d/layers/")
   ;; List of configuration layers to load.
   dotspacemacs-configuration-layers
   '(javascript
     rust ;; needed for Hugo (editing TOMLs)
     ;; ----------------------------------------------------------------
     ;; Example of useful layers you may want to use right away.
     ;; Uncomment some layer names and press <SPC f e R> (Vim style) or
     ;; <M-m f e R> (Emacs style) to install them.
     ;; ----------------------------------------------------------------
     helm
     auto-completion
     ;; better-defaults
     emacs-lisp
     git
     markdown
     (plantuml :variables
               plantuml-default-exec-mode 'jar
               plantuml-jar-path (expand-file-name "~/distrib/plantuml.1.2021.0.jar" )
               org-plantuml-jar-path (expand-file-name "~/distrib/plantuml.1.2021.0.jar" ))

     (org :variables
          org-enable-roam-support t
          org-enable-org-journal-support t
          org-enable-github-support t
          org-projectile-file "TODOs.org"
          org-enable-hugo-support t)
     (mu4e :variables
       mu4e-installation-path "/usr/share/emacs/site-lisp/mu4e")
     ;; (shell :variables
     ;;        shell-default-height 30
     ;;        shell-default-position 'bottom)
     syntax-checking
     version-control
     graphviz
     yaml
     ;; octave
     html
     c-c++
     spell-checking
     ;; python from here: https://www.reddit.com/r/spacemacs/comments/aer220/help_setting_up_python_integration/
     lsp
     dap ;; new debugger for python layer
     (python :variables
             python-fill-column 99
             python-formatter 'yapf
             python-backend 'lsp
             python-lsp-server 'pyright)
     (conda :variables conda-anaconda-home "/home/bochkarev/distrib/anaconda3")
     (ess :variables ess-r-backend 'lsp)
     (latex :variables latex-enable-folding t)
     bibtex
     pdf
     finance ;; ledger layer
     csv
     evil-commentary
     (evil-snipe :variables evil-snipe-enable-alternate-f-and-t-behaviors t)
     themes-megapack
     ;; deft ;; don't use this, actually
     unicode-fonts
     (vinegar :variables
              vinegar-reuse-dired-buffer t)
     ;; elfeed setup
     (elfeed :variables rmh-elfeed-org-files (list "~/.spacemacs.d/elfeed.org"))
     )
   ;; List of additional packages that will be installed without being
   ;; wrapped in a layer. If you need some configuration for these
   ;; packages, then consider creating a layer. You can also put the
   ;; configuration in `dotspacemacs/user-config'.
   dotspacemacs-additional-packages '(
                                      doom-themes
                                      org-roam-bibtex
                                      org-roam-server
                                      all-the-icons-dired
                                      ;; org-re-reveal-ref
                                      parchment-theme
                                      sunny-day-theme
                                      poet-theme
                                      org-msg ;; email-related
                                      )
   ;; A list of packages that cannot be updated.
   dotspacemacs-frozen-packages '()
   ;; A list of packages that will not be installed and loaded.
   dotspacemacs-excluded-packages '()
   ;; Defines the behaviour of Spacemacs when installing packages.
   ;; Possible values are `used-only', `used-but-keep-unused' and `all'.
   ;; `used-only' installs only explicitly used packages and uninstall any
   ;; unused packages as well as their unused dependencies.
   ;; `used-but-keep-unused' installs only the used packages but won't uninstall
   ;; them if they become unused. `all' installs *all* packages supported by
   ;; Spacemacs and never uninstall them. (default is `used-only')
   dotspacemacs-install-packages 'used-only))

(defun dotspacemacs/init ()
  "Initialization function.
This function is called at the very startup of Spacemacs initialization
before layers configuration.
You should not put any user code in there besides modifying the variable
values."
  ;; This setq-default sexp is an exhaustive list of all the supported
  ;; spacemacs settings.
  (setq-default
   ;; If non nil ELPA repositories are contacted via HTTPS whenever it's
   ;; possible. Set it to nil if you have no way to use HTTPS in your
   ;; environment, otherwise it is strongly recommended to let it set to t.
   ;; This variable has no effect if Emacs is launched with the parameter
   ;; `--insecure' which forces the value of this variable to nil.
   ;; (default t)
   dotspacemacs-elpa-https t
   ;; Maximum allowed time in seconds to contact an ELPA repository.
   dotspacemacs-elpa-timeout 5
   ;; If non nil then spacemacs will check for updates at startup
   ;; when the current branch is not `develop'. Note that checking for
   ;; new versions works via git commands, thus it calls GitHub services
   ;; whenever you start Emacs. (default nil)
   dotspacemacs-check-for-update nil
   ;; If non-nil, a form that evaluates to a package directory. For example, to
   ;; use different package directories for different Emacs versions, set this
   ;; to `emacs-version'.
   dotspacemacs-elpa-subdirectory nil
   ;; One of `vim', `emacs' or `hybrid'.
   ;; `hybrid' is like `vim' except that `insert state' is replaced by the
   ;; `hybrid state' with `emacs' key bindings. The value can also be a list
   ;; with `:variables' keyword (similar to layers). Check the editing styles
   ;; section of the documentation for details on available variables.
   ;; (default 'vim)
   dotspacemacs-editing-style 'vim
   ;; If non nil output loading progress in `*Messages*' buffer. (default nil)
   dotspacemacs-verbose-loading nil
   ;; Specify the startup banner. Default value is `official', it displays
   ;; the official spacemacs logo. An integer value is the index of text
   ;; banner, `random' chooses a random text banner in `core/banners'
   ;; directory. A string value must be a path to an image format supported
   ;; by your Emacs build.
   ;; If the value is nil then no banner is displayed. (default 'official)
   dotspacemacs-startup-banner 'official
   ;; List of items to show in startup buffer or an association list of
   ;; the form `(list-type . list-size)`. If nil then it is disabled.
   ;; Possible values for list-type are:
   ;; `recents' `bookmarks' `projects' `agenda' `todos'."
   ;; List sizes may be nil, in which case
   ;; `spacemacs-buffer-startup-lists-length' takes effect.
   dotspacemacs-startup-lists '((recents . 5)
                                (projects . 7))
   ;; True if the home buffer should respond to resize events.
   dotspacemacs-startup-buffer-responsive t
   ;; Default major mode of the scratch buffer (default `text-mode')
   dotspacemacs-scratch-mode 'emacs-lisp-mode
   ;; List of themes, the first of the list is loaded when spacemacs starts.
   ;; Press <SPC> T n to cycle to the next theme in the list (works great
   ;; with 2 themes variants, one dark and one light)
   dotspacemacs-themes '(moe-light
                         doom-gruvbox)
   ;; If non nil the cursor color matches the state color in GUI Emacs.
   dotspacemacs-colorize-cursor-according-to-state t
   ;; Default font, or prioritized list of fonts. `powerline-scale' allows to
   ;; quickly tweak the mode-line size to make separators look not too crappy.
   dotspacemacs-default-font '("Iosevka Term"
                               :size 15
                               :weight normal
                               :width normal
                               :powerline-scale 1.1)
   ;; The leader key
   dotspacemacs-leader-key "SPC"
   ;; The key used for Emacs commands (M-x) (after pressing on the leader key).
   ;; (default "SPC")
   dotspacemacs-emacs-command-key "SPC"
   ;; The key used for Vim Ex commands (default ":")
   dotspacemacs-ex-command-key ":"
   ;; The leader key accessible in `emacs state' and `insert state'
   ;; (default "M-m")
   dotspacemacs-emacs-leader-key "M-m"
   ;; Major mode leader key is a shortcut key which is the equivalent of
   ;; pressing `<leader> m`. Set it to `nil` to disable it. (default ",")
   dotspacemacs-major-mode-leader-key ","
   ;; Major mode leader key accessible in `emacs state' and `insert state'.
   ;; (default "C-M-m")
   dotspacemacs-major-mode-emacs-leader-key "C-M-m"
   ;; These variables control whether separate commands are bound in the GUI to
   ;; the key pairs C-i, TAB and C-m, RET.
   ;; Setting it to a non-nil value, allows for separate commands under <C-i>
   ;; and TAB or <C-m> and RET.
   ;; In the terminal, these pairs are generally indistinguishable, so this only
   ;; works in the GUI. (default nil)
   dotspacemacs-distinguish-gui-tab nil
   ;; If non nil `Y' is remapped to `y$' in Evil states. (default nil)
   dotspacemacs-remap-Y-to-y$ nil
   ;; If non-nil, the shift mappings `<' and `>' retain visual state if used
   ;; there. (default t)
   dotspacemacs-retain-visual-state-on-shift t
   ;; If non-nil, J and K move lines up and down when in visual mode.
   ;; (default nil)
   dotspacemacs-visual-line-move-text t
   ;; If non nil, inverse the meaning of `g' in `:substitute' Evil ex-command.
   ;; (default nil)
   dotspacemacs-ex-substitute-global nil
   ;; Name of the default layout (default "Default")
   dotspacemacs-default-layout-name "Default"
   ;; If non nil the default layout name is displayed in the mode-line.
   ;; (default nil)
   dotspacemacs-display-default-layout nil
   ;; If non nil then the last auto saved layouts are resume automatically upon
   ;; start. (default nil)
   dotspacemacs-auto-resume-layouts nil
   ;; Size (in MB) above which spacemacs will prompt to open the large file
   ;; literally to avoid performance issues. Opening a file literally means that
   ;; no major mode or minor modes are active. (default is 1)
   dotspacemacs-large-file-size 1
   ;; Location where to auto-save files. Possible values are `original' to
   ;; auto-save the file in-place, `cache' to auto-save the file to another
   ;; file stored in the cache directory and `nil' to disable auto-saving.
   ;; (default 'cache)
   dotspacemacs-auto-save-file-location 'cache
   ;; Maximum number of rollback slots to keep in the cache. (default 5)
   dotspacemacs-max-rollback-slots 5
   ;; If non nil, `helm' will try to minimize the space it uses. (default nil)
   dotspacemacs-helm-resize nil
   ;; if non nil, the helm header is hidden when there is only one source.
   ;; (default nil)
   dotspacemacs-helm-no-header nil
   ;; define the position to display `helm', options are `bottom', `top',
   ;; `left', or `right'. (default 'bottom)
   dotspacemacs-helm-position 'bottom
   ;; Controls fuzzy matching in helm. If set to `always', force fuzzy matching
   ;; in all non-asynchronous sources. If set to `source', preserve individual
   ;; source settings. Else, disable fuzzy matching in all sources.
   ;; (default 'always)
   dotspacemacs-helm-use-fuzzy 'always
   ;; If non nil the paste micro-state is enabled. When enabled pressing `p`
   ;; several times cycle between the kill ring content. (default nil)
   dotspacemacs-enable-paste-transient-state nil
   ;; Which-key delay in seconds. The which-key buffer is the popup listing
   ;; the commands bound to the current keystroke sequence. (default 0.4)
   dotspacemacs-which-key-delay 0.4
   ;; Which-key frame position. Possible values are `right', `bottom' and
   ;; `right-then-bottom'. right-then-bottom tries to display the frame to the
   ;; right; if there is insufficient space it displays it at the bottom.
   ;; (default 'bottom)
   dotspacemacs-which-key-position 'bottom
   ;; If non nil a progress bar is displayed when spacemacs is loading. This
   ;; may increase the boot time on some systems and emacs builds, set it to
   ;; nil to boost the loading time. (default t)
   dotspacemacs-loading-progress-bar t
   ;; If non nil the frame is fullscreen when Emacs starts up. (default nil)
   ;; (Emacs 24.4+ only)
   dotspacemacs-fullscreen-at-startup nil
   ;; If non nil `spacemacs/toggle-fullscreen' will not use native fullscreen.
   ;; Use to disable fullscreen animations in OSX. (default nil)
   dotspacemacs-fullscreen-use-non-native nil
   ;; If non nil the frame is maximized when Emacs starts up.
   ;; Takes effect only if `dotspacemacs-fullscreen-at-startup' is nil.
   ;; (default nil) (Emacs 24.4+ only)
   dotspacemacs-maximized-at-startup nil
   ;; A value from the range (0..100), in increasing opacity, which describes
   ;; the transparency level of a frame when it's active or selected.
   ;; Transparency can be toggled through `toggle-transparency'. (default 90)
   dotspacemacs-active-transparency 90
   ;; A value from the range (0..100), in increasing opacity, which describes
   ;; the transparency level of a frame when it's inactive or deselected.
   ;; Transparency can be toggled through `toggle-transparency'. (default 90)
   dotspacemacs-inactive-transparency 90
   ;; If non nil show the titles of transient states. (default t)
   dotspacemacs-show-transient-state-title t
   ;; If non nil show the color guide hint for transient state keys. (default t)
   dotspacemacs-show-transient-state-color-guide t
   ;; If non nil unicode symbols are displayed in the mode line. (default t)
   dotspacemacs-mode-line-unicode-symbols t
   ;; If non nil smooth scrolling (native-scrolling) is enabled. Smooth
   ;; scrolling overrides the default behavior of Emacs which recenters point
   ;; when it reaches the top or bottom of the screen. (default t)
   dotspacemacs-smooth-scrolling t
   ;; Control line numbers activation.
   ;; If set to `t' or `relative' line numbers are turned on in all `prog-mode' and
   ;; `text-mode' derivatives. If set to `relative', line numbers are relative.
   ;; This variable can also be set to a property list for finer control:
   ;; '(:relative nil
   ;;   :disabled-for-modes dired-mode
   ;;                       doc-view-mode
   ;;                       markdown-mode
   ;;                       org-mode
   ;;                       pdf-view-mode
   ;;                       text-mode
   ;;   :size-limit-kb 1000)
   ;; (default nil)
   dotspacemacs-line-numbers 'relative
   ;; Code folding method. Possible values are `evil' and `origami'.
   ;; (default 'evil)
   dotspacemacs-folding-method 'evil
   ;; If non-nil smartparens-strict-mode will be enabled in programming modes.
   ;; (default nil)
   dotspacemacs-smartparens-strict-mode nil
   ;; If non-nil pressing the closing parenthesis `)' key in insert mode passes
   ;; over any automatically added closing parenthesis, bracket, quote, etc…
   ;; This can be temporary disabled by pressing `C-q' before `)'. (default nil)
   dotspacemacs-smart-closing-parenthesis nil
   ;; Select a scope to highlight delimiters. Possible values are `any',
   ;; `current', `all' or `nil'. Default is `all' (highlight any scope and
   ;; emphasis the current one). (default 'all)
   dotspacemacs-highlight-delimiters 'all
   ;; If non nil, advise quit functions to keep server open when quitting.
   ;; (default nil)
   dotspacemacs-persistent-server nil
   ;; List of search tool executable names. Spacemacs uses the first installed
   ;; tool of the list. Supported tools are `ag', `pt', `ack' and `grep'.
   ;; (default '("ag" "pt" "ack" "grep"))
   dotspacemacs-search-tools '("ag" "pt" "ack" "grep")
   ;; The default package repository used if no explicit repository has been
   ;; specified with an installed package.
   ;; Not used for now. (default nil)
   dotspacemacs-default-package-repository nil
   ;; Delete whitespace while saving buffer. Possible values are `all'
   ;; to aggressively delete empty line and long sequences of whitespace,
   ;; `trailing' to delete only the whitespace at end of lines, `changed'to
   ;; delete only whitespace for changed lines or `nil' to disable cleanup.
   ;; (default nil)
   dotspacemacs-whitespace-cleanup nil
   ))

(defun dotspacemacs/user-init ()
  "Initialization function for user code.
It is called immediately after `dotspacemacs/init', before layer configuration
executes.
 This function is mostly useful for variables that need to be set
before packages are loaded. If you are unsure, you should try in setting them in
`dotspacemacs/user-config' first."
  (setenv "WORKON_HOME" "~/distrib/anaconda3/envs")
  )

(defun dotspacemacs/user-config ()
  "Configuration function for user code.
This function is called at the very end of Spacemacs initialization after
layers configuration.
This is the place where most of your configurations should be done. Unless it is
explicitly specified that a variable should be set before a package is loaded,
you should place your code here."
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  (with-eval-after-load 'org
    ;; my orgmode customizations go here
    (require 'org-tempo) ;; for some reason otherwise src-blocks expansion does not work
    (require 'ox-extra) ;; :ignore: feature
    (ox-extras-activate '(ignore-headlines))
    ;; set ellipsis
    (setq org-ellipsis "↴") ;; might want to consider: ▼, ⤵, ↴, ⬎, ⤷, ⋱

    (setq org-journal-carryover-items "+carryover|+TODO=\"TODO\"")

    ;; clock into drawer
    (setq org-clock-into-drawer t)

    ;; for resolving idle time - ask me if I seem to be out
    (setq org-clock-idle-time 15)

    ;; log done
    (setq org-log-done t)

    ;; set up a capture mode
    (setq org-refile-file "~/orgmode/refile.org")
    (setq org-lj-file "~/orgmode/labjournal.org")
    (setq org-workflow-file "~/orgmode/master.org")
    (setq org-sys-log "~/pkb/SysRebuild.org")

    ;; some Zettelkasten targets
    (setq org-zk-readme-file "~/zettelkasten/20200401114428_readme.org")

    (setq org-clock-history-length 24)

    (setq org-refile-targets
          '(("~/orgmode/meta-org.org" :maxlevel . 9)
            ("~/pkb/Bookshelf.org" :maxlevel . 9)
            (org-workflow-file :maxlevel . 9)
            (org-lj-file :maxlevel . 9)
            ;; project-specific files go below
;;            ("~/Skoltech/Research/OPF.org" :maxlevel . 9)
;;            ("~/consulting/2017-12 FGC BM/BM-project.org" :maxlevel . 9)
            ))

    ;; journal-specific setup
    (defun org-journal-find-location ()
      ;; Open today's journal, but specify a non-nil prefix argument in order to
      ;; inhibit inserting the heading; org-capture will insert the heading.
      (org-journal-new-entry t)
      ;; Position point on the journal's top-level heading so that org-capture
      ;; will add the new entry as a child entry.
      (goto-char (point-min)))

    ;; ================================== CAPTURE TEMPLATES CONFIG GO HERE ===================================
    (setq projectile-changelog-file "CHANGELOG.org")

    (setq org-capture-templates
          (quote (
                  ("t" "=========== Tasks / TODO-s ===================")
                  ;;;;; Tasks templates ;;;;;
                  ("tT" "New TODO and CLOCK-IN" entry (file+headline org-refile-file "Tasks")
                   "* TODO %?\n%U\n%a\n" :clock-in t :clock-resume t)
                  ("tt" "New TODO" entry (file+headline org-refile-file "Tasks")
                   "* TODO %?\n%U\n%a\n")
                  ("tp" "New project-specific TODO" entry (file+headline (lambda () (concat (projectile-project-root)  org-projectile-per-project-filepath)) "Current project TODOs")
                   "* TODO %?\n%U\n%a\n")
                  ("c" "Changelog entry (specific project)" entry(file+headline (lambda () (concat (projectile-project-root) "CHANGELOG.org")) "Current version")
                   "* %U: %? \n%a\n" :prepend t)

                  ;;;;; Journal templates ;;;;;
                  ("j" "=========== [J]ournal entries ============")
                  ("jj" "Journal entry" entry (function org-journal-find-location)
                   "* %(format-time-string org-journal-time-format)%^{Title}\n%i%?")
                  ;; ("jj" "Journal entry (today)" entry (file+datetree org-lj-file)
                  ;;  "* %U -- %?\n")

                  ("jd" "Journal entry (specific date)" entry(file+datetree+prompt org-lj-file)
                   "* %? \n %T")
                  ("ji" "A research [i]dea" entry (file+headline org-lj-file "Ideas / pipeline")
                   "* %U %?\n")
                  ("jn" "A research [n]ote" entry (file+headline org-lj-file "Notes")
                   "* %U -- %?\n")
                  ("ja" "Journal [a]rticle -- reading log" entry(file+datetree org-lj-file)
                   "* Reading: %?  :article:\n Started: %U")

                  ;;;;; General-purpose templates ;;;;;
                  ("" "============ General-purpose templates ========")
                  ;; ("d" "A [d]istraction / phone call / etc." entry(file+headline org-workflow-file "Distractions")
                  ;;  "* %? :REFILE: \n%U\n%a\n" :clock-in t :clock-resume t)

                  ("s" "A [s]ystem-related info / configs" entry(file+headline org-sys-log "Ecosystem")
                   "* %? \n%U\n")

                  ("n" "A [n]ote" entry (file+headline org-workflow-file "Notes")
                   "* %?\n%U\n%a\n")
                  ("i" "An [i]dea" entry (file+headline org-workflow-file "Ideas")
                   "* %?\n%U\n%a\n" :clock-in t :clock-resume t)

                  ("b" "Note: [b]uy something / shopping list" entry(file+headline org-workflow-file "Shopping list")
                   "* %?")

                  ;;;;; readme-notes for org-protocol

                  ("m" "========== Book[m]arks / readme notes====================")
                  ("mr" "Research-related entry" entry (file+headline org-zk-readme-file "Research-related notes")
                   "* %a\n Captured: %U\n %?\n")
                  ("mg" "General note (link)" entry (file+headline org-zk-readme-file "General notes")
                   "* %a\n Captured: %U\n %?\n")

                  ;;;;; Questions ;;;;;
                  ;; ("Q" "========== Questions tbd later / feedback ===============")
                  ;; ("Qs" "[S]tochastic opt (Dr. Song)" entry (file+olp org-workflow-file "Study" "IE 8930 Song" "Questions tbd") "**** %? \n")
                  ;; ("Qh" "[H]f (Dr. Neyens)" entry (file+olp org-workflow-file "Study" "IE 8000 HF" "Questions tbd") "**** %? \n")
                  ("Q" "Question TBD w / Dr. Smith)" entry (file+olp org-workflow-file "Research" "Questions tbd") "**** %? \n")

                  ;; ("l" "========== Course log entries ================")
                  ;; ("lp" "Advanced [p]robability (Dr. Burak)" entry (file+olp org-workflow-file "Study" "IE 8880 Prob" "Course log") "**** %U %?")
                  ;; ("lo" "Advanced [o]R (Dr. Khademi)" entry (file+olp org-workflow-file "Study" "IE 8800 OR" "Course log") "**** %U %?")
                  ;; ("la" "[a]lgoritms (Dr. Dean)" entry (file+olp org-workflow-file "Study" "Algorithms" "Course log") "**** %U %?")

                  ;;;;; Personal templates ;;;;;
                  ("p" "========== Personal issues and habits ========")
                  ("pp" "[p]ush-ups" plain (file+olp org-workflow-file "Habits" "Push-ups")
                   "%U >>> %?")
                  ;; ("B" "Break from focus" plain (file+olp org-workflow-file "Habits" "Breaks")
                  ;;  "*** %U" :immediate-finish t)
                  )))

    (use-package org-protocol
      :after org)

    ;; disable it for now
    ;; (use-package org-re-reveal :after org)
    ;; (use-package org-re-reveal-ref :after org)
    ;; set-up agenda
    (setq org-agenda-files (list "~/orgmode"))

    ;; Punch-in / punch-out concept realization
    (setq ab/continuous-clocking nil)

    (defun ab/punch-in (arg)
      "Start continuous clocking and set the default task to the
selected task.  If no task is selected set the Organization task
as the default task."
      (interactive "p")
      (setq ab/continuous-clocking t)
      (if (equal major-mode 'org-agenda-mode)
          ;;
          ;; We're in the agenda
          ;;
          (let* ((marker (org-get-at-bol 'org-hd-marker))
                 (tags (org-with-point-at marker (org-get-tags-at))))
            (if (and (eq arg 4) tags)
                (org-agenda-clock-in '(16))
              (ab/clock-in-organization-task-as-default)))
        ;;
        ;; We are not in the agenda
        ;;
        (save-restriction
          (widen)
                                        ; Find the tags on the current task
          (if (and (equal major-mode 'org-mode) (not (org-before-first-heading-p)) (eq arg 4))
              (org-clock-in '(16))
            (ab/clock-in-organization-task-as-default)))))

    (defun ab/punch-out ()
      "Stops continuous clocking and clocks-out if necessary"
      (interactive)
      (setq ab/continuous-clocking nil)
      (when (org-clock-is-active)
        (org-clock-out))
      (org-agenda-remove-restriction-lock))

    (defun ab/clock-in-default-task ()
      (save-excursion
        (org-with-point-at org-clock-default-task
          (org-clock-in))))

    (defvar ab/organization-task-id "2946c481-3af4-4642-b886-ac7936858346")

    (defun ab/clock-in-organization-task-as-default ()
      (interactive)
      (org-with-point-at (org-id-find ab/organization-task-id 'marker)
        (org-clock-in '(16))))

    (defun ab/clock-out-to-def ()
      (when (and ab/continuous-clocking
                 (not org-clock-clocking-in)
                 (marker-buffer org-clock-default-task)
                 (not org-clock-resolving-clocks-due-to-idleness))
        (ab/clock-in-default-task)))

    (add-hook 'org-clock-out-hook 'ab/clock-out-to-def 'append)

    ;; Sometimes I change tasks I'm clocking quickly - this removes clocked tasks with 0:00 duration
    (setq org-clock-out-remove-zero-time-clocks t)

    (setq org-use-fast-todo-selection t)

    (setq org-todo-keywords
          (quote ((sequence "TODO(t)" "NEXT(n)" "WIP(p)" "WATCH(w)" "HOLD(h)" "|" "DONE(d)" "CANCELED(q)"))))

    (setq org-todo-keyword-faces
          (quote (("TODO" :foreground "red" :weight bold)
                  ("NEXT" :foreground "white" :background "red" :weight bold)
                  ("WIP" :foreground "white" :background "blue" :weight bold)
                  ("DONE" :foreground "forest green" :weight bold)
                  ("WATCH" :foreground "yellow" :weight bold)
                  ("CANCELED" :foreground "black" :background "gray")
                  ("HOLD" :foreground "orange" :weight bold))))

    ;; default tagslist
    (setq org-tag-alist '(("CS" . ?s) ("keydate" . ?k) ("frog" . ?f)))

    ;; tags that are not inherited
    (setq org-tags-exclude-from-inheritance '("keydate")) ;; that was the logic of keydate tag (needed for simple calendar generation)

    ;; here is the remainder of nice code to implement simple to-do lists generation for the team (to be sent by email further)
    ;; todo-lists for the team
    (defun ab/get-date-if-not-nil(datearg)
      "Returns date if not nil and nil otherwise"
      (interactive)
      (if datearg
          (format-time-string "%d-%m-%Y" datearg)
        "          ")
      )

    ;; ============================== The Core: custom agenda setup ==============================================
    (setq org-agenda-custom-commands
          '(
            ("d" "Daily agenda"
             (
              (agenda ""
                      ((org-agenda-prefix-format " %i %?-25(concat (org-format-outline-path (list (nth 0 (org-get-outline-path))))) : ")
                       (org-agenda-overriding-header "== Calendar: ===============================================")))
              (tags-todo "-PERSONAL-REFILE/WIP"
                         ((org-agenda-prefix-format " %i %?-25(concat (org-format-outline-path (list (nth 0 (org-get-outline-path))))) : ")
                          (org-agenda-overriding-header "== WORK IN PROGRESS: ===============================================")))
              (tags-todo "-PERSONAL-REFILE/WATCH"
                         ((org-agenda-prefix-format " %i %?-25(concat (org-format-outline-path (list (nth 0 (org-get-outline-path))))) : ")
                          (org-agenda-overriding-header "== WATCHING / CONTROL: ======================================")))
              (tags-todo "-PERSONAL-REFILE/NEXT"
                         ((org-agenda-prefix-format " %i %?-25(concat (org-format-outline-path (list (nth 0 (org-get-outline-path))))) : ")
                          (org-agenda-overriding-header "== NEXT STEPS POOL: ============================================")
                          (org-agenda-sorting-strategy '(priority-down))
                          ))
              (tags-todo "+PERSONAL-REFILE"
                         ((org-agenda-prefix-format " %i %?-25(concat (org-format-outline-path (list (nth 0 (org-get-outline-path))))) : ")
                          (org-agenda-overriding-header "== PERSONAL TODOs: ==========================================")
                          (org-agenda-sorting-strategy '(todo-state-down priority-down effort-up))))

              (tags "REFILE"
                    ((org-agenda-prefix-format " %i %?-25(concat (org-format-outline-path (list (nth 0 (org-get-outline-path))))) : ")
                     (org-agenda-overriding-header "== TODO's to REFILE: ========================================")
                     (org-agenda-sorting-strategy '(todo-state-down priority-down effort-up))))
              (tags-todo "-PERSONAL-REFILE-someday/TODO"
                         ((org-agenda-prefix-format " %i %?-25(concat (org-format-outline-path (list (nth 0 (org-get-outline-path))))) : ")
                          (org-agenda-overriding-header "== OTHER TODOs : ===============================================")
                          (org-agenda-sorting-strategy '(priority-down))
                          ))
              (tags "-REFILE+someday"
                    ((org-agenda-prefix-format " %i %?-25(concat (org-format-outline-path (list (nth 0 (org-get-outline-path))))) : ")
                     (org-agenda-overriding-header "== SOMEDAY items: ==========================================")))
              ))

            ("k" "Key milestones"
             ((tags "keydate"
                         ((org-agenda-prefix-format " %i %?-12(concat \"\" (ab/get-date-if-not-nil (org-get-deadline-time nil)) \" -- \" )")
                          (org-agenda-todo-keyword-format "")
                          (org-agenda-overriding-header "\n== KEY MILESTONES: =====================================\n")
                          (org-agenda-remove-tags t)
                          ))))

            ("p" "Calendar (chronological) Plan by milestones"
             ((tags "keydate"
                         ((org-agenda-prefix-format " %i %?-12(concat \"\" (ab/get-date-if-not-nil (org-get-deadline-time nil)) \" -- \" )")
                          (org-agenda-todo-keyword-format "")
                          (org-agenda-sorting-strategy '(deadline-up))
                          (org-agenda-overriding-header "\n== KEY MILESTONES - calendar plan =========================================\n")
                          (org-agenda-remove-tags t)
                          ))))
            )

          )

    ;; thunderlink setup -- opening emails direct;; with org-mac-link message:// links are handed over to the macOS system,
    ;; which has built-in handling. On Windows and Linux, we can use thunderlink!
    ;; (deactivated, since not supported by Thunderbird as of writing this. Now using mu4e)

   ;;  (when (not (string-equal system-type "darwin"))
   ;;    ;; modify this for your system
   ;;    (setq thunderbird-program "~/distrib/thunderbird/thunderbird")

   ;;    (defun org-message-thunderlink-open (slash-message-id)
   ;;      "Handler for org-link-set-parameters that converts a standard message:// link into
   ;; a thunderlink and then invokes thunderbird."
   ;;      ;; remove any / at the start of slash-message-id to create real message-id
   ;;      (let ((message-id
   ;;             (replace-regexp-in-string (rx bos (* "/"))
   ;;                                       ""
   ;;                                       slash-message-id)))
   ;;        (start-process
   ;;         (concat "thunderlink: " message-id)
   ;;         nil
   ;;         thunderbird-program
   ;;         "-thunderlink"
   ;;         (concat "thunderlink://messageid=" message-id)
   ;;         )))
   ;;    ;; on message://aoeu link, this will call handler with //aoeu
   ;;    (org-link-set-parameters "message" :follow #'org-message-thunderlink-open))

    (with-eval-after-load 'org-agenda
      (require 'org-projectile)
      (mapcar '(lambda (file)
                 (when (file-exists-p file)
                   (push file org-agenda-files)))
              (org-projectile-todo-files)))

    ;; ============================== End of the custom agenda setup ==============================================

    (server-start)

    (setq spaceline-org-clock-p t)

    (setq org-time-clocksum-format
          '(:hours "%d" :require-hours t :minutes ":%02d" :require-minutes t))

    ;; Use full outline paths for refile targets - we file directly with IDO
    (setq org-refile-use-outline-path t)

    ;; Targets complete directly with IDO
    (setq org-outline-path-complete-in-steps nil)
    (setq org-completion-use-ido t)

    ;; set format for clock-reports
    (setq org-duration-format (quote h:mm))

    ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ;; Setup default HTML-export format (css)

    (setq org-export-html-style-include-scripts nil
          org-export-html-style-include-default nil)

    (setq org-export-html-style
          "<link rel=\"stylesheet\" type=\"text/css\" href=\"~/css/workflow.css\" />")

    ;; set up column view for efficient estimates usage
    ;; from oylipt

    ;; Set default column view headings: Task Effort Clock_Summary
    (setq org-columns-default-format "%80ITEM(Task) %10Effort(Effort){:} %10CLOCKSUM")
    ;; global Effort estimate values
    ;; global STYLE property values for completion
    (setq org-global-properties (quote (("Effort_ALL" . "0:15 0:30 0:45 1:00 2:00 3:00 4:00 5:00 6:00 0:00")
                                        ("STYLE_ALL" . "habit"))))


    ;; babel setup
    (setq org-ditaa-jar-path "/usr/bin/ditaa")

    (org-babel-do-load-languages
     'org-babel-load-languages
     '((ditaa . t)
       (dot . t)
       (python . t)
       (shell . t)
       (R . t)
       (plantuml . t)
       (makefile . t)
       ;; (ipython . t)
       )) ; this line activates ditaa

    (setq org-src-fontify-natively t
          org-src-window-setup 'current-window ;; edit in current window
          org-src-strip-leading-and-trailing-blank-lines t
          org-src-preserve-indentation t ;; do not put two spaces on the left
          org-src-tab-acts-natively t)


    (setq org-edit-src-content-indentation 0)

    (setq org-list-allow-alphabetical t)

    ;; (setq python-indent-offset 4)
    ;; (setq pipenv-with-flycheck nil)

;;   ;; don’t prompt me to confirm everytime I want to evaluate a block
;;   (setq org-confirm-babel-evaluate nil)

;; ;;; display/update images in the buffer after I evaluate
;;   (add-hook ‘org-babel-after-execute-hook ‘org-display-inline-images ‘append)


    ;; orgmode specific: use minted for syntax highlighting
    ;; Use minted
    (add-to-list 'org-latex-packages-alist '("" "minted"))
    (setq org-latex-listings 'minted)

    ;; Add the shell-escape flag
    (setq org-latex-pdf-process '(
                                  "pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f"
                                  ;; "bibtex %b"
                                  "pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f"
                                  "pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f"
                                  ))

    ;; Sample minted options.
    (setq org-latex-minted-options '(
                                     ("frame" "lines")
                                     ("fontsize" "\\scriptsize")
                                     ("xleftmargin" "\\parindent")
                                     ("linenos" "")
                                     ))
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

    ;; org-roam-server setup
    (setq org-roam-server-host "127.0.0.1"
          org-roam-server-port 8080
          org-roam-server-authenticate nil
          org-roam-server-export-inline-images t
          org-roam-server-serve-files nil
          org-roam-server-served-file-extensions '("pdf" "mp4" "ogv")
          org-roam-server-network-poll t
          org-roam-server-network-arrows nil
          org-roam-server-network-label-truncate t
          org-roam-server-network-label-truncate-length 60
          org-roam-server-network-label-wrap-length 20)
  ;; =================================== CUSTOM BIBTEX SETUP (ORG-REF) ===========================================

  ;; this file is to set up my reference management
  ;; KEY TOOLS used:
  ;; - Zotero (standalone + browser plugin) - data collection
  ;; - Better BibTex zotero plugin to keep my bib up-to-date
  ;; - dropbox to keep my bib
  ;; - orgmode for authoring
  ;; - org-ref

  ;; Activate org-zotxt-mode in org-mode buffers
  ;; (add-hook 'org-mode-hook (lambda () (org-zotxt-mode 1)))

  ;; Bind something to replace the awkward C-u C-c " i
  ;;(define-key org-mode-map
  ;;  (kbd "H-<SPC>") (lambda () (interactive)
  ;;		    (org-zotxt-insert-reference-link)))

  ;;(define-key org-mode-map
  ;;  (kbd "H-\\") (lambda () (interactive)
  ;;		    (org-zotxt-insert-reference-link '(4))))


  ;; (define-key org-mode-map
  ;;  (kbd "H-<return>") (lambda () (interactive)
  ;;		    (org-zotxt-open-attachment))
  ;;  )

  ;; Change citation format to be less cumbersome in files.
  ;; You'll need to install mkbehr-short into your style manager first.
  ;; (eval-after-load "zotxt"
  ;; '(setq zotxt-default-bibliography-style "mkbehr-short"))

  (setq reftex-default-bibliography '"~/Dropbox/bibliography/references.bib")

  ;; see org-ref for use of these variables
  (setq org-ref-default-bibliography '"~/Dropbox/bibliography/references.bib")

  (setq org-ref-bibliography-notes '"~/Dropbox/bibliography/notes.org"
        org-ref-default-bibliography '"~/Dropbox/bibliography/references.bib"
        org-ref-pdf-directory '"~/Dropbox/bibliography/bibtex-pdfs/")

  (setq bibtex-completion-bibliography '"~/Dropbox/bibliography/references.bib"
        ;; bibtex-completion-library-path '"~/Dropbox/bibliography/bibtex-pdfs"
        ;; bibtex-completion-notes-path '"~/Dropbox/bibliography/helm-bibtex-notes"
        bibtex-completion-pdf-field '"file")

  ;; open pdf with system pdf viewer (works with okular)
  (setq bibtex-completion-pdf-open-function
        (lambda (fpath)
          (start-process "open" "*open*" "evince" fpath)))

  ;; alternative
  ;; (setq bibtex-completion-pdf-open-function 'org-open-file)

  ;; get org-ref to open Zotero-style saved PDFs (not in a single directory)
  (defun ab/org-ref-open-pdf-at-point ()
    "Opens the PDF for bibtex key under point if it exists."
    (interactive)
    (let* ((results (org-ref-get-bibtex-key-and-file))
           (key (car results))
           (pdf-file (car (bibtex-completion-find-pdf key))))
      (message "pdf file is %s" pdf-file)
      (if (file-exists-p pdf-file)
          (org-open-file pdf-file)
          ;; (let ((process-connection-type nil))
          ;;   (start-process "" nil "xdg-open" pdf-file))
          ;;(bibtex-completion-pdf-open-function pdf-file)
        (message "No PDF found for %s" key))))

  (setq org-ref-open-pdf-function 'ab/org-ref-open-pdf-at-point)

  ;; correct export of references
  (setq org-latex-prefer-user-labels t)

  ;; set up export to latex

  (setq org-latex-pdf-process
        '("pdflatex -shell-escape -interaction nonstopmode -output-directory %o %b"
          "bibtex %b"
          "makeindex %b"
          "pdflatex -shell-escape -interaction nonstopmode -output-directory %o %b"
          "pdflatex -shell-escape -interaction nonstopmode -output-directory %o %b"))

  (setq org-format-latex-options (plist-put org-format-latex-options :scale 2.0)) ; set up normal inline latex size

  ;; set up Julia environment
  ;;(setq inferior-julia-program-name "~/distrib/julia/bin/julia")


  ;; set-up projectile cash for fuzzy open-file search
  (setq projectile-enable-caching t)

  ;; org-protocol setup

  (add-to-list 'org-modules 'org-protocol)
  (require 'org-protocol)
  (require 'org-roam-protocol)
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; gnome-panel clocked-in task
  (add-hook
   'org-mode-hook
   (lambda ()
     ;; Org clock string to Gnome top bar. Needs :
     ;; https://extensions.gnome.org/extension/974/short-memo/
     (defun current-task-to-status ()
       (interactive)
       (if (fboundp 'org-clocking-p)
           (if (org-clocking-p)
               (call-process "dconf" nil nil nil "write"
                             "/org/gnome/shell/extensions/short-memo/message"
                             (concat "'" (org-clock-get-clock-string) "'"))
             (call-process "dconf" nil nil nil "write"
                           "/org/gnome/shell/extensions/short-memo/message"
                           "'No active clock'"))))
     ;; update clock message every minute
     (run-with-timer 0 60 'current-task-to-status)

     ;; update clock immediately on clock-in / clock-out
     (defun my-org-clock-message (old-function &rest arguments)
       (apply old-function arguments)
       (current-task-to-status))
     (advice-add #'org-clock-in :around #'my-org-clock-message)
     (advice-add #'org-clock-out :around #'my-org-clock-message)
     )
   )

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; org-roam setup

  (setq org-roam-directory "~/zettelkasten/")
  (setq org-roam-link-title-format "🕮:%s") ;; maybe? 🕮 § (doesn't look good in bold?)

  (setq orb-templates
        '(("r" "ref" plain (function org-roam-capture--get-point) ""
           :file-name "refs/${citekey}"
           :head "#+TITLE: ${citekey}: ${title}\n#+ROAM_KEY: ${ref}\n#+TAGS: ${keywords}\n\n"
           :unnarrowed t
           :immediate-finish t)))

  (defun ab-full-orf ()
    (interactive)
    (let ((helm-full-frame t))
      (org-roam-find-file)))

  ;; while we are at it: make help pop up a new frame always
  ;; (setq helm-display-function 'helm-display-buffer-in-own-frame
  ;;       helm-display-buffer-reuse-frame t
  ;;       helm-use-undecorated-frame-option t)

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; define some custom (global) keybindings
  (global-set-key (kbd "H-c") 'org-capture)
  (global-set-key (kbd "H-q") 'org-fill-paragraph)
  (global-set-key (kbd "H-/") 'org-roam-find-file)
  (global-set-key (kbd "H-a") 'org-agenda)
  (global-set-key (kbd "H-t") 'org-projectile/goto-todos)
  ;; special files menu >>>
  (spacemacs|define-transient-state ab|goto-special-file
    :title "Go to a 'special' file (project-specific)"
    :doc
    "\n [_t_] TODOs list [_c_] change log [_r_/_R_] README{.org/.md} [_i_] .gitignore [_q_] quit"
    :bindings
    ("t" org-projectile/goto-todos :exit t)
    ("c" (find-file (concat (projectile-project-root) "CHANGELOG.org")) :exit t)
    ("R" (find-file (concat (projectile-project-root) "README.md")) :exit t)
    ("r" (find-file (concat (projectile-project-root) "README.org")) :exit t)
    ("i" (find-file (concat (projectile-project-root) ".gitignore")) :exit t)
    ("q" nil :exit t))

  (define-key evil-normal-state-map (kbd "H-g")
    'spacemacs/ab|goto-special-file-transient-state/body)

  ;; <<<
  (global-set-key (kbd "H-1") (lambda () (interactive) (find-file "~/orgmode/master.org")))
  (global-set-key (kbd "H-m") 'spacemacs/toggle-maximize-buffer)
  (global-set-key (kbd "H-SPC") 'avy-goto-char)

  (global-set-key (kbd "H-s") 'org-save-all-org-buffers)

  ;; moving around
  (global-set-key (kbd "H-<up>") 'org-roam-find-file)
  (global-set-key (kbd "H-h") 'evil-window-left)
  (global-set-key (kbd "H-l") 'evil-window-right)
  (global-set-key (kbd "H-k") 'evil-window-up)
  (global-set-key (kbd "H-j") 'evil-window-down)

  (spacemacs/set-leader-keys (kbd "<DEL>") 'spacemacs/kill-this-buffer)

  ;; use ripgrep instead of grep (way faster!)
  ;; borrowed from https://gist.github.com/pesterhazy/fabd629fbb89a6cd3d3b92246ff29779
  (evil-leader/set-key "/" 'spacemacs/helm-project-do-ag)

  (defun ab/jump-master ()
    "Jump to master-file"
    (interactive)
    (bookmark-maybe-load-default-file)
    (bookmark-jump "master"))

  (defun ab/jump-lj ()
    "Jump to master-file"
    (interactive)
    (bookmark-maybe-load-default-file)
    (bookmark-jump "labjournal"))

  (progn
    ;; define a prefix keymap
    (define-prefix-command 'ab-keymap)
    (define-key ab-keymap (kbd "a") 'ab/org-ref-open-pdf-at-point) ;; open a PDF at point (for org-ref)
    (define-key ab-keymap (kbd "1") 'ab/jump-master)
    (define-key ab-keymap (kbd "2") 'ab/jump-lj)
    (define-key ab-keymap (kbd "t") 'org-timer-set-timer) ;; easy timer
    (define-key ab-keymap (kbd "i") 'org-clock-in)
    (define-key ab-keymap (kbd "o") 'org-clock-out)
    (define-key ab-keymap (kbd "w") 'org-wiki-helm)
    )

  (define-key org-mode-map (kbd "s-<return>") 'org-insert-item)
  (define-key org-mode-map (kbd "C-s-<return>") 'org-insert-todo-heading)
  (global-set-key (kbd "H-i") 'org-clock-in)
  (global-set-key (kbd "H-p") 'org-pomodoro)
  (global-set-key (kbd "H-o") 'org-clock-out)

  (global-set-key (kbd "<H-f12>") 'org-projectile/goto-todos)

  (global-set-key (kbd "C-`") ab-keymap)
  (global-set-key (kbd "<f8>") 'org-agenda)

  ;; C++ specific config
  (setq org-src-tab-acts-natively t)

  ;; dired-specific config
  (setq dired-dwim-target t)

  ;; for graphviz to work
  (setq default-tab-width 4)
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; Zettelkasten related config
  ;; deft -- no longer used.
  ;; (setq deft-recursive t)
  ;; (setq deft-use-filter-string-for-filename t)
  ;; (setq deft-use-filename-as-title nil)
  ;; (setq deft-default-extension "org")
  ;; (setq deft-directory "~/zettelkasten/")

  ;; org-journal
  (setq org-journal-date-prefix "#+TITLE: ")
  (setq org-journal-file-format "%Y-%m-%d.org")
  (setq org-journal-dir "~/zettelkasten/daily/")
  (setq org-journal-date-format "%A, %d %B %Y")
    )
  ;; end of orgmode-specific setup
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  ;; email setup (mu4e)
  (with-eval-after-load 'mu4e
    (setq mu4e-attachment-dir "~/Downloads/email")
    (setq mu4e-change-filenames-when-moving t)

    ;; config based on the Spacemacs docs
;;; Set up some common mu4e variables

    (setq mu4e-maildir "~/.mail"
          mu4e-get-mail-command "mbsync -a"
          mu4e-update-interval nil
          mu4e-compose-signature-auto-include nil
          mu4e-view-show-images t
          mu4e-view-show-addresses t)

;;; Mail directory shortcuts
    (setq mu4e-maildir-shortcuts
          '(("/CU/INBOX" . ?c)
            ("/personal/INBOX" . ?p)))

;;; Bookmarks
    (setq mu4e-bookmarks
          `(("flag:unread AND NOT flag:trashed" "Unread messages" ?u)
            ("date:today..now" "Today's messages" ?t)
            ("date:7d..now" "Last 7 days" ?w)
            (,(mapconcat 'identity
                         (mapcar
                          (lambda (maildir)
                            (concat "maildir:" (car maildir)))
                          mu4e-maildir-shortcuts) " OR ")
             "All inboxes" ?i)))

    ;; setting up contexts
    ;; source: https://www.djcbsoftware.nl/code/mu/mu4e/Contexts-example.html
    (setq mu4e-contexts
          `( ,(make-mu4e-context
	             :name "Pri"
	             :enter-func (lambda () (mu4e-message "Entering Private context"))
               :leave-func (lambda () (mu4e-message "Leaving Private context"))
	             ;; we match based on maildir
	             :match-func (lambda (msg)
			                       (when msg
			                         (string-match-p "^/personal" (mu4e-message-field msg :maildir))))
	             :vars '( ( user-mail-address	    . "a@bochkarev.io"  )
		                    ( user-full-name	    . "Alexey Bochkarev" )
		                    ( mu4e-compose-signature .
		                      (concat
		                       "Alexey Bochkarev\n"
                           "https://www.bochkarev.io\n"
		                       "telegram: @abochka\n"))
                        ;; set up maildir folders
                        (mu4e-sent-folder . "/personal/Sent")
	                      (mu4e-drafts-folder . "/personal/Drafts")
	                      (mu4e-trash-folder . "/personal/Trash")
                        (mu4e-refile-folder . "/personal/Archive")
                        ;; sending mail preferences
                        (mu4e-sent-messages-behavior . sent)
                        (smtpmail-queue-dir . "~/.mail/personal/queue/cur")
	                      (message-send-mail-function . smtpmail-send-it)
	                      (smtpmail-smtp-user . "a@bochkarev.io")
	                      (smtpmail-starttls-credentials . (("smtp.mailbox.org" 587 nil nil)))
	                      (smtpmail-auth-credentials . (expand-file-name "~/.authinfo.gpg"))
	                      (smtpmail-default-smtp-server . "smtp.mailbox.org")
	                      (smtpmail-smtp-server . "smtp.mailbox.org")
	                      (smtpmail-smtp-service . 587)
                        (stmpmail-stream-type 'ssl)
	                      (smtpmail-debug-info . t)
	                      (smtpmail-debug-verbose . t)
                        ))
             ,(make-mu4e-context
	             :name "CU"
	             :enter-func (lambda () (mu4e-message "Switch to the Clemson context"))
               :leave-func (lambda () (mu4e-message "Leaving Clemson context"))
	             ;; no leave-func
	             ;; we match based on the maildir of the message
	             ;; this matches maildir ... and its sub-directories
	             :match-func (lambda (msg)
			                       (when msg
			                         (string-match-p "^/CU" (mu4e-message-field msg :maildir))))
	             :vars '( ( user-mail-address	     . "abochka@g.clemson.edu" )
		                    ( user-full-name	     . "Alexey Bochkarev" )
		                    ( mu4e-compose-signature  .
		                      (concat
		                       "Alexey Bochkarev\n"
                           "https://www.bochkarev.io\n"
		                       "telegram: @abochka\n"))
                        ;; set up maildir folders
                        (mu4e-sent-folder . "/CU/Sent")
	                      (mu4e-drafts-folder . "/CU/Drafts")
	                      (mu4e-trash-folder . "/CU/Trash")
                        (mu4e-refile-folder . "/CU/Archive")
                        ;; sending mail preferences
                        (mu4e-sent-messages-behavior . delete)
                        (smtpmail-queue-dir . "~/.mail/CU/queue/cur")
	                      (message-send-mail-function . smtpmail-send-it)
	                      (smtpmail-smtp-user . "abochka@g.clemson.edu")
	                      (smtpmail-starttls-credentials . (("smtp.gmail.com" 587 nil nil)))
	                      (smtpmail-auth-credentials . (expand-file-name "~/.authinfo.gpg"))
	                      (smtpmail-default-smtp-server . "smtp.gmail.com")
	                      (smtpmail-smtp-server . "smtp.gmail.com")
	                      (smtpmail-smtp-service . 587)
	                      (smtpmail-debug-info . t)
	                      (smtpmail-debug-verbose . t)
                        ))
             )))
  (setq mu4e-context-policy 'pick-first)
  (setq mu4e-compose-context-policy 'ask-if-none) ;; that's default, I guess

  ;; set `mu4e-context-policy` and `mu4e-compose-policy` to tweak when mu4e should
  ;; guess or ask the correct context, e.g.

  ;; start with the first (default) context;
  ;; default is to ask-if-none (ask when there's no context yet, and none match)
  ;; (setq mu4e-context-policy 'pick-first)

  ;; compose with the current context is no context matches;
  ;; default is to ask
  ;; (setq mu4e-compose-context-policy nil)

  ;; the story with trashing
  ;; see https://github.com/djcb/mu/issues/1136
  (setf (alist-get 'trash mu4e-marks)
        (list :char '("d" . "▼")
              :prompt "dtrash"
              :dyn-target (lambda (target msg)
                            (mu4e-get-trash-folder msg))
              :action (lambda (docid msg target)
                        ;; Here's the main difference to the regular trash mark,
                        ;; no +T before -N so the message is not marked as
                        ;; IMAP-deleted:
                        (mu4e~proc-move docid (mu4e~mark-check-target target) "-N"))))
  (add-hook 'mu4e-headers-mode-hook
	          (defun my/mu4e-change-headers ()
	            (interactive)
	            (setq mu4e-headers-fields
		                `((:human-date . 12)
		                  (:flags . 4)
		                  (:from-or-to . 15)
		                  (:subject . ,(- (window-body-width) 47))
		                  (:size . 7)))))

  ;; sending mail config
  (setq mu4e-compose-in-new-frame t
        ;; mu4e-sent-messages-behavior 'delete ;; set up on the per-context basis
        mu4e-compose-signature-auto-include t
        mu4e-compose-format-flowed t
        org-mu4e-convert-to-html t)

  ;; spell check
  (add-hook 'mu4e-compose-mode-hook
            (defun my-do-compose-stuff ()
              "My settings for message composition."
              (visual-line-mode)
              (use-hard-newlines -1)
              (flyspell-mode)))
  (require 'smtpmail)
  (setq smtpmail-queue-mail nil)  ;; start in normal mode

  (setq org-mu4e-convert-to-html t)
  (setq mu4e-view-show-addresses 't)


  (setq message-kill-buffer-on-exit t)
  (setq mu4e-compose-dont-reply-to-self t)
  ;; don't ask when quitting
  (setq mu4e-confirm-quit nil)

  ;; set up org-msg
  (setq mail-user-agent 'mu4e-user-agent)

  (setq org-msg-options "tex:dvipng html-postamble:nil H:5 num:nil ^:{} toc:nil author:nil email:nil \\n:t"
	      org-msg-startup "hidestars indent inlineimages"
	      org-msg-default-alternatives '(text html))
  (org-msg-mode) ;; so it fires up by default

  ;;end of email config (mu4e) ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  ;; line numbers for orgmode fix (linum)
  ;; (setq relative-line-numbers-motion-function 'forward-visible-line)
  ;; (set-face-attribute 'linum nil :height 100)

  ;;; calendar customization
  (copy-face 'default 'calendar-iso-week-header-face)
  (set-face-attribute 'calendar-iso-week-header-face nil
                      :height 0.7)

  (copy-face 'default 'calendar-iso-week-face)
  (set-face-attribute 'calendar-iso-week-face nil
                      :height 1.0 :foreground "salmon")

  (setq calendar-week-start-day 1)

  (setq calendar-intermonth-header
        (propertize "Wk"                  ; or e.g. "KW" in Germany
                    'font-lock-face 'calendar-iso-week-header-face))

  (setq calendar-intermonth-text
        '(propertize
          (format "%2d"
                  (car
                   (calendar-iso-from-absolute
                    (calendar-absolute-from-gregorian (list month day year)))))
          'font-lock-face 'calendar-iso-week-face))
  ;;; end of calendar customization
  (setq my-email "abochka@clemson.edu")
  (setq my-bib "~/Dropbox/bibliography/references.bib")
  ;; yasnippet directory
  (setq yas-snippet-dirs (append yas-snippet-dirs
                                 '("~/.spacemacs.d/snippets"))) ;; append with a personal snippets collection

  ;; (use-package org-special-block-extras
  ;;   :ensure t
  ;;   :hook (org-mode . org-special-block-extras-mode))

  ;; encryption config, from https://github.com/nickanderson/Level-up-your-notes-with-Org/blob/master/dot-spacemacs
  ;; http://yenliangl.blogspot.com/2009/12/encrypt-your-important-data-in-emacs.html
  ;; http://emacs-fu.blogspot.com/2011/02/keeping-your-secrets-secret.html
  ;; This allows me to encrypt subtrees that are tagged with crypt automatically.
  ;; by default I want to encrypt it to myself. but with properties entries I cna encrypt to other people. which is useful in a shared file situation
  (require 'org-crypt)

  ;; Automatically re-encrypt entries on save to avoid leaking decrypted
  ;; information.
  (org-crypt-use-before-save-magic)
  (setq org-crypt-disable-auto-save (quote encrypt))

  ;; GPG key to use for encryption
  ;; Either the Key ID or set to nil to use symmetric encryption.
  (setq org-crypt-key "623059B30431300C1268C89A95BC4F563F9F14E6")

  ;; This prevents the crypt tag from being included in inheritance.
  (setq org-tags-exclude-from-inheritance (quote ("crypt")))

  ;; set up ESS
  ;; (add-hook 'ess-r-post-run-hook (lambda () (ess-load-file "~/.spacemacs.d/ESS-autorun.R")))
  ;; ab/config end
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  )

;; Do not write anything past this comment. This is where Emacs will
;; auto-generate custom variable definitions.
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   (quote
    (ws-butler winum which-key volatile-highlights vi-tilde-fringe uuidgen use-package toc-org spaceline powerline restart-emacs request rainbow-delimiters popwin persp-mode pcre2el paradox spinner org-plus-contrib org-bullets open-junk-file neotree move-text macrostep lorem-ipsum linum-relative link-hint indent-guide hydra lv hungry-delete hl-todo highlight-parentheses highlight-numbers parent-mode highlight-indentation helm-themes helm-swoop helm-projectile projectile pkg-info epl helm-mode-manager helm-make helm-flx helm-descbinds helm-ag google-translate golden-ratio flx-ido flx fill-column-indicator fancy-battery eyebrowse expand-region exec-path-from-shell evil-visualstar evil-visual-mark-mode evil-unimpaired evil-tutor evil-surround evil-search-highlight-persist highlight evil-numbers evil-nerd-commenter evil-mc evil-matchit evil-lisp-state smartparens evil-indent-plus evil-iedit-state iedit evil-exchange evil-escape evil-ediff evil-args evil-anzu anzu evil goto-chg undo-tree eval-sexp-fu elisp-slime-nav dumb-jump f dash s diminish define-word column-enforce-mode clean-aindent-mode bind-map bind-key auto-highlight-symbol auto-compile packed aggressive-indent adaptive-wrap ace-window ace-link ace-jump-helm-line helm avy helm-core popup async))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
(defun dotspacemacs/emacs-custom-settings ()
  "Emacs custom settings.
This is an auto-generated function, do not modify its content directly, use
Emacs customize menu instead.
This function is called at the very end of Spacemacs initialization."
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(elfeed-feeds
   '("http://pubsonline.informs.org/action/showFeed?type=etoc&feed=rss&jc=moor"))
 '(mml-secure-openpgp-sign-with-sender t)
 '(org-agenda-files
   '("~/DSPI/TODOs.org" "~/projects/tgs-curl/TODOs.org" "~/research-wip/BDD-appl/TODOs.org" "~/BDD/TODOs.org" "~/projects/bochkarev.io/TODOs.org" "/home/bochkarev/orgmode/BM_story.org" "/home/bochkarev/orgmode/german.org" "/home/bochkarev/orgmode/labjournal.org" "/home/bochkarev/orgmode/master.org" "/home/bochkarev/orgmode/refile.org" "/home/bochkarev/orgmode/results.org"))
 '(package-selected-packages
   '(tide typescript-mode tern nodejs-repl livid-mode skewer-mode js2-refactor multiple-cursors js2-mode js-doc import-js grizzl helm-gtags ggtags counsel-gtags counsel swiper add-node-modules-path ws-butler winum which-key volatile-highlights vi-tilde-fringe uuidgen use-package toc-org spaceline powerline restart-emacs request rainbow-delimiters popwin persp-mode pcre2el paradox spinner org-plus-contrib org-bullets open-junk-file neotree move-text macrostep lorem-ipsum linum-relative link-hint indent-guide hydra lv hungry-delete hl-todo highlight-parentheses highlight-numbers parent-mode highlight-indentation helm-themes helm-swoop helm-projectile projectile pkg-info epl helm-mode-manager helm-make helm-flx helm-descbinds helm-ag google-translate golden-ratio flx-ido flx fill-column-indicator fancy-battery eyebrowse expand-region exec-path-from-shell evil-visualstar evil-visual-mark-mode evil-unimpaired evil-tutor evil-surround evil-search-highlight-persist highlight evil-numbers evil-nerd-commenter evil-mc evil-matchit evil-lisp-state smartparens evil-indent-plus evil-iedit-state iedit evil-exchange evil-escape evil-ediff evil-args evil-anzu anzu evil goto-chg undo-tree eval-sexp-fu elisp-slime-nav dumb-jump f dash s diminish define-word column-enforce-mode clean-aindent-mode bind-map bind-key auto-highlight-symbol auto-compile packed aggressive-indent adaptive-wrap ace-window ace-link ace-jump-helm-line helm avy helm-core popup async))
 '(safe-local-variable-values
   '((eval progn
           (org-babel-goto-named-src-block "setup")
           (org-babel-execute-src-block)
           (outline-hide-sublevels 1))
     (javascript-backend . tide)
     (javascript-backend . tern)
     (javascript-backend . lsp))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(font-lock-variable-name-face ((t (:foreground "#000000" :weight bold)))))
)
