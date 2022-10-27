;; -*- mode: emacs-lisp; lexical-binding: t -*-
;; This file is loaded by Spacemacs at startup.
;; It must be stored in your home directory.

(defun dotspacemacs/layers ()
  "Layer configuration:
This function should only modify configuration layer settings."
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

   ;; List of additional paths where to look for configuration layers.
   ;; Paths must have a trailing slash (i.e. `~/.mycontribs/')
   dotspacemacs-configuration-layer-path '()

   ;; List of configuration layers to load.
   dotspacemacs-configuration-layers
   '(haskell
     octave
     rust ;; note: needed for Hugo / editing TOML files
     julia
     csv
     graphviz
     (python :variables python-test-runner 'pytest)
     ipython-notebook
     restructuredtext git javascript
     html
     search-engine
     ;; ----------------------------------------------------------------
     ;; Example of useful layers you may want to use right away.
     ;; Uncomment some layer names and press `SPC f e R' (Vim style) or
     ;; `M-m f e R' (Emacs style) to install them.
     ;; ----------------------------------------------------------------
     (auto-completion :variables
                      auto-completion-tab-key-behavior nil)
     ;; better-defaults
     emacs-lisp
     ;;helm
     (ivy :variables ivy-enable-icons t)
     ;; lsp
     ;; markdown
     (markdown :variables markdown-live-preview-engine 'vmd)
     ;; multiple-cursors
     ;; (shell :variables
     ;;        shell-default-height 30
     ;;        shell-default-position 'bottom)
     spell-checking
     syntax-checking
     version-control

     themes-megapack

     helpful

     ranger

     ;; org-related setup
     (org :variables
          org-enable-github-support t
          org-projectile-file "TODOs.org"
          org-enable-roam-support t
          org-enable-roam-server t
          org-enable-roam-protocol t
          org-enable-hugo-support t)
     bibtex

     ;; mail setup
     (mu4e :variables
           mu4e-installation-path "/usr/share/emacs/site-lisp/elpa/mu4e-1.8.10")

     ess
     ;; latex setup
     latex

     ;; literature notes
     deft
     treemacs)


   ;; List of additional packages that will be installed without being wrapped
   ;; in a layer (generally the packages are installed only and should still be
   ;; loaded using load/require/use-package in the user-config section below in
   ;; this file). If you need some configuration for these packages, then
   ;; consider creating a layer. You can also put the configuration in
   ;; `dotspacemacs/user-config'. To use a local version of a package, use the
   ;; `:location' property: '(your-package :location "~/path/to/your-package/")
   ;; Also include the dependencies as they will not be resolved automatically.
   dotspacemacs-additional-packages '(org-msg keyfreq modus-themes deadgrep counsel-etags)

   ;; A list of packages that cannot be updated.
   dotspacemacs-frozen-packages '()

   ;; A list of packages that will not be installed and loaded.
   dotspacemacs-excluded-packages '()

   ;; Defines the behaviour of Spacemacs when installing packages.
   ;; Possible values are `used-only', `used-but-keep-unused' and `all'.
   ;; `used-only' installs only explicitly used packages and deletes any unused
   ;; packages as well as their unused dependencies. `used-but-keep-unused'
   ;; installs only the used packages but won't delete unused ones. `all'
   ;; installs *all* packages supported by Spacemacs and never uninstalls them.
   ;; (default is `used-only')
   dotspacemacs-install-packages 'used-only))

(defun dotspacemacs/init ()
  "Initialization:
This function is called at the very beginning of Spacemacs startup,
before layer configuration.
It should only modify the values of Spacemacs settings."
  ;; This setq-default sexp is an exhaustive list of all the supported
  ;; spacemacs settings.
  (setq-default
   ;; If non-nil then enable support for the portable dumper. You'll need
   ;; to compile Emacs 27 from source following the instructions in file
   ;; EXPERIMENTAL.org at to root of the git repository.
   ;; (default nil)
   dotspacemacs-enable-emacs-pdumper nil

   ;; Name of executable file pointing to emacs 27+. This executable must be
   ;; in your PATH.
   ;; (default "emacs")
   dotspacemacs-emacs-pdumper-executable-file "emacs"

   ;; Name of the Spacemacs dump file. This is the file will be created by the
   ;; portable dumper in the cache directory under dumps sub-directory.
   ;; To load it when starting Emacs add the parameter `--dump-file'
   ;; when invoking Emacs 27.1 executable on the command line, for instance:
   ;;   ./emacs --dump-file=$HOME/.emacs.d/.cache/dumps/spacemacs-27.1.pdmp
   ;; (default (format "spacemacs-%s.pdmp" emacs-version))
   dotspacemacs-emacs-dumper-dump-file (format "spacemacs-%s.pdmp" emacs-version)

   ;; If non-nil ELPA repositories are contacted via HTTPS whenever it's
   ;; possible. Set it to nil if you have no way to use HTTPS in your
   ;; environment, otherwise it is strongly recommended to let it set to t.
   ;; This variable has no effect if Emacs is launched with the parameter
   ;; `--insecure' which forces the value of this variable to nil.
   ;; (default t)
   dotspacemacs-elpa-https t

   ;; Maximum allowed time in seconds to contact an ELPA repository.
   ;; (default 5)
   dotspacemacs-elpa-timeout 5

   ;; Set `gc-cons-threshold' and `gc-cons-percentage' when startup finishes.
   ;; This is an advanced option and should not be changed unless you suspect
   ;; performance issues due to garbage collection operations.
   ;; (default '(100000000 0.1))
   dotspacemacs-gc-cons '(100000000 0.1)

   ;; Set `read-process-output-max' when startup finishes.
   ;; This defines how much data is read from a foreign process.
   ;; Setting this >= 1 MB should increase performance for lsp servers
   ;; in emacs 27.
   ;; (default (* 1024 1024))
   dotspacemacs-read-process-output-max (* 1024 1024)

   ;; If non-nil then Spacelpa repository is the primary source to install
   ;; a locked version of packages. If nil then Spacemacs will install the
   ;; latest version of packages from MELPA. Spacelpa is currently in
   ;; experimental state please use only for testing purposes.
   ;; (default nil)
   dotspacemacs-use-spacelpa nil

   ;; If non-nil then verify the signature for downloaded Spacelpa archives.
   ;; (default t)
   dotspacemacs-verify-spacelpa-archives t

   ;; If non-nil then spacemacs will check for updates at startup
   ;; when the current branch is not `develop'. Note that checking for
   ;; new versions works via git commands, thus it calls GitHub services
   ;; whenever you start Emacs. (default nil)
   dotspacemacs-check-for-update nil

   ;; If non-nil, a form that evaluates to a package directory. For example, to
   ;; use different package directories for different Emacs versions, set this
   ;; to `emacs-version'. (default 'emacs-version)
   dotspacemacs-elpa-subdirectory 'emacs-version

   ;; One of `vim', `emacs' or `hybrid'.
   ;; `hybrid' is like `vim' except that `insert state' is replaced by the
   ;; `hybrid state' with `emacs' key bindings. The value can also be a list
   ;; with `:variables' keyword (similar to layers). Check the editing styles
   ;; section of the documentation for details on available variables.
   ;; (default 'vim)
   dotspacemacs-editing-style 'vim

   ;; If non-nil show the version string in the Spacemacs buffer. It will
   ;; appear as (spacemacs version)@(emacs version)
   ;; (default t)
   dotspacemacs-startup-buffer-show-version t

   ;; Specify the startup banner. Default value is `official', it displays
   ;; the official spacemacs logo. An integer value is the index of text
   ;; banner, `random' chooses a random text banner in `core/banners'
   ;; directory. A string value must be a path to an image format supported
   ;; by your Emacs build.
   ;; If the value is nil then no banner is displayed. (default 'official)
   dotspacemacs-startup-banner 'random

   ;; Scale factor controls the scaling (size) of the startup banner. Default
   ;; value is `auto' for scaling the logo automatically to fit all buffer
   ;; contents, to a maximum of the full image height and a minimum of 3 line
   ;; heights. If set to a number (int or float) it is used as a constant
   ;; scaling factor for the default logo size.
   dotspacemacs-startup-banner-scale 'auto

   ;; List of items to show in startup buffer or an association list of
   ;; the form `(list-type . list-size)`. If nil then it is disabled.
   ;; Possible values for list-type are:
   ;; `recents' `recents-by-project' `bookmarks' `projects' `agenda' `todos'.
   ;; List sizes may be nil, in which case
   ;; `spacemacs-buffer-startup-lists-length' takes effect.
   ;; The exceptional case is `recents-by-project', where list-type must be a
   ;; pair of numbers, e.g. `(recents-by-project . (7 .  5))', where the first
   ;; number is the project limit and the second the limit on the recent files
   ;; within a project.
   dotspacemacs-startup-lists '((recents . 5)
                                (projects . 7))

   ;; True if the home buffer should respond to resize events. (default t)
   dotspacemacs-startup-buffer-responsive t

   ;; Show numbers before the startup list lines. (default t)
   dotspacemacs-show-startup-list-numbers t

   ;; The minimum delay in seconds between number key presses. (default 0.4)
   dotspacemacs-startup-buffer-multi-digit-delay 0.4

   ;; If non-nil, show file icons for entries and headings on Spacemacs home buffer.
   ;; This has no effect in terminal or if "all-the-icons" package or the font
   ;; is not installed. (default nil)
   dotspacemacs-startup-buffer-show-icons t

   ;; Default major mode for a new empty buffer. Possible values are mode
   ;; names such as `text-mode'; and `nil' to use Fundamental mode.
   ;; (default `text-mode')
   dotspacemacs-new-empty-buffer-major-mode 'text-mode

   ;; Default major mode of the scratch buffer (default `text-mode')
   dotspacemacs-scratch-mode 'text-mode

   ;; If non-nil, *scratch* buffer will be persistent. Things you write down in
   ;; *scratch* buffer will be saved and restored automatically.
   dotspacemacs-scratch-buffer-persistent nil

   ;; If non-nil, `kill-buffer' on *scratch* buffer
   ;; will bury it instead of killing.
   dotspacemacs-scratch-buffer-unkillable nil

   ;; Initial message in the scratch buffer, such as "Welcome to Spacemacs!"
   ;; (default nil)
   dotspacemacs-initial-scratch-message nil

   ;; List of themes, the first of the list is loaded when spacemacs starts.
   ;; Press `SPC T n' to cycle to the next theme in the list (works great
   ;; with 2 themes variants, one dark and one light)
   dotspacemacs-themes '(doom-nord modus-operandi modus-vivendi)

   ;; Set the theme for the Spaceline. Supported themes are `spacemacs',
   ;; `all-the-icons', `custom', `doom', `vim-powerline' and `vanilla'. The
   ;; first three are spaceline themes. `doom' is the doom-emacs mode-line.
   ;; `vanilla' is default Emacs mode-line. `custom' is a user defined themes,
   ;; refer to the DOCUMENTATION.org for more info on how to create your own
   ;; spaceline theme. Value can be a symbol or list with additional properties.
   ;; (default '(spacemacs :separator wave :separator-scale 1.5))
   ;; default: dotspacemacs-mode-line-theme '(spacemacs :separator wave :separator-scale 1.5)
   dotspacemacs-mode-line-theme 'doom

   ;; If non-nil the cursor color matches the state color in GUI Emacs.
   ;; (default t)
   dotspacemacs-colorize-cursor-according-to-state t

   ;; Default font or prioritized list of fonts. The `:size' can be specified as
   ;; a non-negative integer (pixel size), or a floating-point (point size).
   ;; Point size is recommended, because it's device independent. (default 10.0)
   dotspacemacs-default-font '("Iosevka"
                               :size 12.0
                               :weight normal
                               :width normal)

   ;; The leader key (default "SPC")
   dotspacemacs-leader-key "SPC"

   ;; The key used for Emacs commands `M-x' (after pressing on the leader key).
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
   ;; (default "C-M-m" for terminal mode, "<M-return>" for GUI mode).
   ;; Thus M-RET should work as leader key in both GUI and terminal modes.
   ;; C-M-m also should work in terminal mode, but not in GUI mode.
   dotspacemacs-major-mode-emacs-leader-key (if window-system "<M-return>" "C-M-m")

   ;; These variables control whether separate commands are bound in the GUI to
   ;; the key pairs `C-i', `TAB' and `C-m', `RET'.
   ;; Setting it to a non-nil value, allows for separate commands under `C-i'
   ;; and TAB or `C-m' and `RET'.
   ;; In the terminal, these pairs are generally indistinguishable, so this only
   ;; works in the GUI. (default nil)
   dotspacemacs-distinguish-gui-tab nil

   ;; Name of the default layout (default "Default")
   dotspacemacs-default-layout-name "Default"

   ;; If non-nil the default layout name is displayed in the mode-line.
   ;; (default nil)
   dotspacemacs-display-default-layout nil

   ;; If non-nil then the last auto saved layouts are resumed automatically upon
   ;; start. (default nil)
   dotspacemacs-auto-resume-layouts nil

   ;; If non-nil, auto-generate layout name when creating new layouts. Only has
   ;; effect when using the "jump to layout by number" commands. (default nil)
   dotspacemacs-auto-generate-layout-names nil

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

   ;; If non-nil, the paste transient-state is enabled. While enabled, after you
   ;; paste something, pressing `C-j' and `C-k' several times cycles through the
   ;; elements in the `kill-ring'. (default nil)
   dotspacemacs-enable-paste-transient-state nil

   ;; Which-key delay in seconds. The which-key buffer is the popup listing
   ;; the commands bound to the current keystroke sequence. (default 0.4)
   dotspacemacs-which-key-delay 0.4

   ;; Which-key frame position. Possible values are `right', `bottom' and
   ;; `right-then-bottom'. right-then-bottom tries to display the frame to the
   ;; right; if there is insufficient space it displays it at the bottom.
   ;; (default 'bottom)
   dotspacemacs-which-key-position 'bottom

   ;; Control where `switch-to-buffer' displays the buffer. If nil,
   ;; `switch-to-buffer' displays the buffer in the current window even if
   ;; another same-purpose window is available. If non-nil, `switch-to-buffer'
   ;; displays the buffer in a same-purpose window even if the buffer can be
   ;; displayed in the current window. (default nil)
   dotspacemacs-switch-to-buffer-prefers-purpose nil

   ;; If non-nil a progress bar is displayed when spacemacs is loading. This
   ;; may increase the boot time on some systems and emacs builds, set it to
   ;; nil to boost the loading time. (default t)
   dotspacemacs-loading-progress-bar t

   ;; If non-nil the frame is fullscreen when Emacs starts up. (default nil)
   ;; (Emacs 24.4+ only)
   dotspacemacs-fullscreen-at-startup nil

   ;; If non-nil `spacemacs/toggle-fullscreen' will not use native fullscreen.
   ;; Use to disable fullscreen animations in OSX. (default nil)
   dotspacemacs-fullscreen-use-non-native nil

   ;; If non-nil the frame is maximized when Emacs starts up.
   ;; Takes effect only if `dotspacemacs-fullscreen-at-startup' is nil.
   ;; (default nil) (Emacs 24.4+ only)
   dotspacemacs-maximized-at-startup nil

   ;; If non-nil the frame is undecorated when Emacs starts up. Combine this
   ;; variable with `dotspacemacs-maximized-at-startup' in OSX to obtain
   ;; borderless fullscreen. (default nil)
   dotspacemacs-undecorated-at-startup t

   ;; A value from the range (0..100), in increasing opacity, which describes
   ;; the transparency level of a frame when it's active or selected.
   ;; Transparency can be toggled through `toggle-transparency'. (default 90)
   dotspacemacs-active-transparency 90

   ;; A value from the range (0..100), in increasing opacity, which describes
   ;; the transparency level of a frame when it's inactive or deselected.
   ;; Transparency can be toggled through `toggle-transparency'. (default 90)
   dotspacemacs-inactive-transparency 90

   ;; If non-nil show the titles of transient states. (default t)
   dotspacemacs-show-transient-state-title t

   ;; If non-nil show the color guide hint for transient state keys. (default t)
   dotspacemacs-show-transient-state-color-guide t

   ;; If non-nil unicode symbols are displayed in the mode line.
   ;; If you use Emacs as a daemon and wants unicode characters only in GUI set
   ;; the value to quoted `display-graphic-p'. (default t)
   dotspacemacs-mode-line-unicode-symbols t

   ;; If non-nil smooth scrolling (native-scrolling) is enabled. Smooth
   ;; scrolling overrides the default behavior of Emacs which recenters point
   ;; when it reaches the top or bottom of the screen. (default t)
   dotspacemacs-smooth-scrolling t

   ;; Show the scroll bar while scrolling. The auto hide time can be configured
   ;; by setting this variable to a number. (default t)
   dotspacemacs-scroll-bar-while-scrolling t

   ;; Control line numbers activation.
   ;; If set to `t', `relative' or `visual' then line numbers are enabled in all
   ;; `prog-mode' and `text-mode' derivatives. If set to `relative', line
   ;; numbers are relative. If set to `visual', line numbers are also relative,
   ;; but only visual lines are counted. For example, folded lines will not be
   ;; counted and wrapped lines are counted as multiple lines.
   ;; This variable can also be set to a property list for finer control:
   ;; '(:relative nil
   ;;   :visual nil
   ;;   :disabled-for-modes dired-mode
   ;;                       doc-view-mode
   ;;                       markdown-mode
   ;;                       org-mode
   ;;                       pdf-view-mode
   ;;                       text-mode
   ;;   :size-limit-kb 1000)
   ;; When used in a plist, `visual' takes precedence over `relative'.
   ;; (default nil)
   dotspacemacs-line-numbers 'visual

   ;; Code folding method. Possible values are `evil', `origami' and `vimish'.
   ;; (default 'evil)
   dotspacemacs-folding-method 'vimish

   ;; If non-nil and `dotspacemacs-activate-smartparens-mode' is also non-nil,
   ;; `smartparens-strict-mode' will be enabled in programming modes.
   ;; (default nil)
   dotspacemacs-smartparens-strict-mode nil

   ;; If non-nil smartparens-mode will be enabled in programming modes.
   ;; (default t)
   dotspacemacs-activate-smartparens-mode t

   ;; If non-nil pressing the closing parenthesis `)' key in insert mode passes
   ;; over any automatically added closing parenthesis, bracket, quote, etc...
   ;; This can be temporary disabled by pressing `C-q' before `)'. (default nil)
   dotspacemacs-smart-closing-parenthesis nil

   ;; Select a scope to highlight delimiters. Possible values are `any',
   ;; `current', `all' or `nil'. Default is `all' (highlight any scope and
   ;; emphasis the current one). (default 'all)
   dotspacemacs-highlight-delimiters 'all

   ;; If non-nil, start an Emacs server if one is not already running.
   ;; (default nil)
   dotspacemacs-enable-server t

   ;; Set the emacs server socket location.
   ;; If nil, uses whatever the Emacs default is, otherwise a directory path
   ;; like \"~/.emacs.d/server\". It has no effect if
   ;; `dotspacemacs-enable-server' is nil.
   ;; (default nil)
   dotspacemacs-server-socket-dir nil

   ;; If non-nil, advise quit functions to keep server open when quitting.
   ;; (default nil)
   dotspacemacs-persistent-server nil

   ;; List of search tool executable names. Spacemacs uses the first installed
   ;; tool of the list. Supported tools are `rg', `ag', `pt', `ack' and `grep'.
   ;; (default '("rg" "ag" "pt" "ack" "grep"))
   dotspacemacs-search-tools '("rg" "ag" "pt" "ack" "grep")

   ;; Format specification for setting the frame title.
   ;; %a - the `abbreviated-file-name', or `buffer-name'
   ;; %t - `projectile-project-name'
   ;; %I - `invocation-name'
   ;; %S - `system-name'
   ;; %U - contents of $USER
   ;; %b - buffer name
   ;; %f - visited file name
   ;; %F - frame name
   ;; %s - process status
   ;; %p - percent of buffer above top of window, or Top, Bot or All
   ;; %P - percent of buffer above bottom of window, perhaps plus Top, or Bot or All
   ;; %m - mode name
   ;; %n - Narrow if appropriate
   ;; %z - mnemonics of buffer, terminal, and keyboard coding systems
   ;; %Z - like %z, but including the end-of-line format
   ;; If nil then Spacemacs uses default `frame-title-format' to avoid
   ;; performance issues, instead of calculating the frame title by
   ;; `spacemacs/title-prepare' all the time.
   ;; (default "%I@%S")
   dotspacemacs-frame-title-format "%I@%S"

   ;; Format specification for setting the icon title format
   ;; (default nil - same as frame-title-format)
   dotspacemacs-icon-title-format nil

   ;; Show trailing whitespace (default t)
   dotspacemacs-show-trailing-whitespace t

   ;; Delete whitespace while saving buffer. Possible values are `all'
   ;; to aggressively delete empty line and long sequences of whitespace,
   ;; `trailing' to delete only the whitespace at end of lines, `changed' to
   ;; delete only whitespace for changed lines or `nil' to disable cleanup.
   ;; (default nil)
   dotspacemacs-whitespace-cleanup nil

   ;; If non-nil activate `clean-aindent-mode' which tries to correct
   ;; virtual indentation of simple modes. This can interfere with mode specific
   ;; indent handling like has been reported for `go-mode'.
   ;; If it does deactivate it here.
   ;; (default t)
   dotspacemacs-use-clean-aindent-mode t

   ;; Accept SPC as y for prompts if non-nil. (default nil)
   dotspacemacs-use-SPC-as-y nil

   ;; If non-nil shift your number row to match the entered keyboard layout
   ;; (only in insert state). Currently supported keyboard layouts are:
   ;; `qwerty-us', `qwertz-de' and `querty-ca-fr'.
   ;; New layouts can be added in `spacemacs-editing' layer.
   ;; (default nil)
   dotspacemacs-swap-number-row nil

   ;; Either nil or a number of seconds. If non-nil zone out after the specified
   ;; number of seconds. (default nil)
   dotspacemacs-zone-out-when-idle nil

   ;; Run `spacemacs/prettify-org-buffer' when
   ;; visiting README.org files of Spacemacs.
   ;; (default nil)
   dotspacemacs-pretty-docs t

   ;; If nil the home buffer shows the full path of agenda items
   ;; and todos. If non-nil only the file name is shown.
   dotspacemacs-home-shorten-agenda-source nil

   ;; If non-nil then byte-compile some of Spacemacs files.
   dotspacemacs-byte-compile t))

(defun dotspacemacs/user-env ()
  "Environment variables setup.
This function defines the environment variables for your Emacs session. By
default it calls `spacemacs/load-spacemacs-env' which loads the environment
variables declared in `~/.spacemacs.env' or `~/.spacemacs.d/.spacemacs.env'.
See the header of this file for more information."
  (spacemacs/load-spacemacs-env))

(defun dotspacemacs/user-init ()
  "Initialization for user code:
This function is called immediately after `dotspacemacs/init', before layer
configuration.
It is mostly for variables that should be set before packages are loaded.
If you are unsure, try setting them in `dotspacemacs/user-config' first.")


(defun dotspacemacs/user-load ()
  "Library to load while dumping.
This function is called only while dumping Spacemacs configuration. You can
`require' or `load' the libraries of your choice that will be included in the
dump.")


(defun dotspacemacs/user-config ()
  "Configuration for user code:
This function is called at the very end of Spacemacs startup, after layer
configuration.
Put your configuration code here, except for variables that should be set
before packages are loaded."

  ;; general options
  ;; do not create lockfiles, see https://develop.spacemacs.org/doc/FAQ.html#why-do-i-get-files-starting-with-
  (setq create-lockfiles nil)

  ;; search engines integration
  (setq browse-url-browser-function 'browse-url-generic
        engine/browser-function 'browse-url-generic
        browse-url-generic-program "qutebrowser")
  (setq-default search-engine-amazon-tld "de")

  ;; theme and general visuals config
  (setq modus-themes-syntax '(green-strings yellow-comments alt-syntax)) ;; for modus-themes
  (setq modus-themes-bold-constructs t)

  (defun my-modus-themes-custom-faces ()
    (modus-themes-with-colors
      (custom-set-faces
       `(fill-column-indicator ((,class :height 2.0 :background ,bg-inactive :foreground ,bg-inactive))))))
  (add-hook 'modus-themes-after-load-theme-hook #'my-modus-themes-custom-faces)

  (setq org-ellipsis "↴") ;; might want to consider: ▼, ⤵, ↴, ⬎, ⤷, ⋱
  ;; end of theme config

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; bibtex config
  (setq bibtex-completion-bibliography '("~/Dropbox/bibliography/references.bib"))
  (setq my-bib "~/Dropbox/bibliography/references.bib")  ;; for 'bib' template
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; org-roam config
  (setq org-roam-directory (file-truename "~/PKB/notes"))
  (org-roam-db-autosync-mode)

  (global-set-key (kbd "H-/") 'org-roam-node-find)
  ;; end of org-roam config

  ;; references and literature notes
  (setq bibtex-completion-pdf-field "file")
  (setq bibtex-completion-pdf-open-function 'spacemacs//open-in-external-app)

  (setq ab/lit-notes-dir "~/PKB/notes/refs")

  (defun ab/pre-populate-notes (KEY)
    "Pre-populates a literature note with some bibtex data."
    (let ((entry (bibtex-completion-get-entry KEY)))
      (insert ":PROPERTIES:\n")
      (insert ":ID: " KEY "-refnote\n")
      (insert ":END:\n\n")
      (insert "#+TITLE: (notes) " KEY ": " (bibtex-completion-get-value "title" entry) "\n")
      (insert "#+SUBTITLE: by " (bibtex-completion-get-value "author" entry) "\n\n")
      (insert "Source: cite:" KEY)))

  (defun ab/populate-refnote ()
    (interactive)
    (ab/pre-populate-notes (read-string "Bibtex key:")))

  (defun ab/edit-lit-notes (KEYS)
    "Opens the notes associated with (bibtex) KEYS,
   basically just files in the form of `ab/lit-notes-directory/key.org`"
    (dolist (KEY KEYS)
      (let ((file (concat ab/lit-notes-dir "/" KEY ".org"))
            (entry (bibtex-completion-get-entry KEY)))
        (if (file-exists-p file)
            (find-file file)
          (if (yes-or-no-p
               (concat file " -- the file does not exist! Create?"))
              (progn
                (find-file file)
                ;; pre-populate the file as possible
                (ab/pre-populate-notes KEY)))))))

  (setq bibtex-completion-edit-notes-function 'ab/edit-lit-notes)

  ;; deft set up
  (setq deft-directory ab/lit-notes-dir)

  ;; see https://github.com/jrblevin/deft/issues/75
  ;; (the last comment)
  (setq deft-strip-summary-regexp
        (concat "\\("
	              "^:.+:.*\n" ; any line with a :SOMETHING:
	              "\\|^#\\+.*\n" ; anyline starting with a #+
	              "\\|^\\*.+.*\n" ; anyline where an asterisk starts the line
	              "\\)"))

  (setq deft-use-filename-as-title nil)

  (advice-add 'deft-parse-title :override
              (lambda (file contents)
                (if deft-use-filename-as-title
	                  (deft-base-filename file)
	                (let* ((case-fold-search 't)
	                       (begin (string-match "^#\\+TITLE: \\(?:(notes)\s\\)*" contents))
	                       (end-of-begin (match-end 0))
	                       (end (string-match "\n" contents begin)))
	                  (if begin
	                      (substring contents end-of-begin end)
	                    (format "%s" file))))))

  (global-set-key (kbd "H-n") 'spacemacs/deft)

  ;; dired customizations
  (setq dired-listing-switches "-laXGh --group-directories-first")

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; email setup (with mu4e)
  (with-eval-after-load 'mu4e
    (setq mu4e-attachment-dir "~/Downloads/email")
    (setq mu4e-change-filenames-when-moving t)

    ;; config based on the Spacemacs docs
    ;; Set up some common mu4e variables

    (setq mu4e-maildir "~/.mail"
          mu4e-get-mail-command "mbsync -a"
          mu4e-update-interval nil
          mu4e-compose-signature-auto-include t
          ;; mu4e-compose-format-flowed t
          mu4e-view-show-images t
          mu4e-view-show-addresses t)

    ;; Mail directory shortcuts
    (setq mu4e-maildir-shortcuts
          '(("/CU/INBOX" . ?c)
            ("/personal/INBOX" . ?p)
            ("/personal/lists" . ?l)
            ("/legacy/INBOX" . ?g)))

    ;; Bookmarks
    (setq mu4e-bookmarks
          `(("flag:unread AND NOT flag:trashed AND NOT maildir:/\/.+\/Trash/" "Unread messages" ?u)
            ("date:today..now AND NOT maildir:/\/.+\/Trash/" "Today's messages" ?t)
            ("date:7d..now AND NOT flag:trashed AND NOT maildir:/\/.+\/Trash/" "Last 7 days" ?w)
            ("flag:flagged" "Flagged/starred" ?f)
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
,(make-mu4e-context
	             :name "legacy"
	             :enter-func (lambda () (mu4e-message "Switch to the legacy gmail context"))
               :leave-func (lambda () (mu4e-message "Leaving legacy gmail context"))
	             ;; no leave-func
	             ;; we match based on the maildir of the message
	             ;; this matches maildir ... and its sub-directories
	             :match-func (lambda (msg)
			                       (when msg
			                         (string-match-p "^/legacy" (mu4e-message-field msg :maildir))))
	             :vars '( ( user-mail-address	     . "aabochkaryov@gmail.com" )
		                    ( user-full-name	     . "Alexey Bochkarev" )
		                    ( mu4e-compose-signature  .
		                      (concat
		                       "Alexey Bochkarev\n"
                           "https://www.bochkarev.io\n"
		                       "telegram: @abochka\n"))
                        ;; set up maildir folders
                        (mu4e-sent-folder . "/legacy/Sent")
	                      (mu4e-drafts-folder . "/legacy/Drafts")
	                      (mu4e-trash-folder . "/legacy/Trash")
                        (mu4e-refile-folder . "/legacy/Archive")
                        ;; sending mail preferences
                        (mu4e-sent-messages-behavior . delete)
                        (smtpmail-queue-dir . "~/.mail/legacy/queue/cur")
	                      (message-send-mail-function . smtpmail-send-it)
	                      (smtpmail-smtp-user . "aabochkaryov@gmail.com")
	                      (smtpmail-starttls-credentials . (("smtp.gmail.com" 587 nil nil)))
	                      (smtpmail-auth-credentials . (expand-file-name "~/.authinfo.gpg"))
	                      (smtpmail-default-smtp-server . "smtp.gmail.com")
	                      (smtpmail-smtp-server . "smtp.gmail.com")
	                      (smtpmail-smtp-service . 587)
	                      (smtpmail-debug-info . t)
	                      (smtpmail-debug-verbose . t)
                        ))
             )))

  (setq mu4e-compose-context-policy 'ask-if-none) ;; that's default, I guess

  ;; the story with trashing
  ;; see https://github.com/djcb/mu/issues/1136
  (with-eval-after-load 'mu4e
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
		                    (:size . 7))))))

  ;; sending mail config
  (setq mu4e-compose-in-new-frame t
        ;; mu4e-sent-messages-behavior 'delete ;; set up on the per-context basis
        mu4e-compose-signature-auto-include t
        ;; mu4e-compose-format-flowed t ;; <-- this caused a weird behavior when forwarding emails
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

  (setq my-email "a@bochkarev.io")

  ;; set up attaching emails from within dired
  ;; (see the FAQ in the manual for details)
  ;;
  (require 'gnus-dired)
  ;; make the `gnus-dired-mail-buffers' function also work on
  ;; message-mode derived modes, such as mu4e-compose-mode
  (defun gnus-dired-mail-buffers ()
    "Return a list of active message buffers."
    (let (buffers)
      (save-current-buffer
        (dolist (buffer (buffer-list t))
     	    (set-buffer buffer)
     	    (when (and (derived-mode-p 'message-mode)
     		             (null message-sent-message-via))
     	      (push (buffer-name buffer) buffers))))
      (nreverse buffers)))

  (setq gnus-dired-mail-mode 'mu4e-user-agent)
  (add-hook 'dired-mode-hook 'turn-on-gnus-dired-mode)

  ;; after that, mark the file(s) in dired and ~C-c~ ~RET~ ~C-a~
  ;; (will ask whether to attach to an existing, or a new message

  ;;end of email config (mu4e) ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  ;; special places config >>>
  (spacemacs|define-transient-state ab|goto-special-file
    :title "Go to a 'special' project-specific file."
    :doc
    "\n [_t_] TODOs list [_c_] change log [_r_/_R_] README{.org/.md} \n [_i_] .gitignore [_T_] .ctagsignore [_m_] Makefile [_s_] setup.el\n [_q_] quit"
    :bindings
    ("t" org-projectile/goto-todos :exit t)
    ("c" (find-file (concat (projectile-project-root) "CHANGELOG.org")) :exit t)
    ("R" (find-file (concat (projectile-project-root) "README.md")) :exit t)
    ("r" (find-file (concat (projectile-project-root) "README.org")) :exit t)
    ("i" (find-file (concat (projectile-project-root) ".gitignore")) :exit t)
    ("T" (find-file (concat (projectile-project-root) ".ctagsignore")) :exit t)
    ("m" (find-file (concat (projectile-project-root) "Makefile")) :exit t)
    ("s" (find-file (concat (projectile-project-root) "setup.el")) :exit t)
    ("q" nil :exit t))

  (define-key evil-normal-state-map (kbd "H-p")
    'spacemacs/ab|goto-special-file-transient-state/body)

  (spacemacs|define-transient-state ab|goto-file
    :title "Special 'locations': Goto-menu."
    :doc
    "\n [_g_] Current org-file [_r_] Reading list [_d_] Distracted [_m_] mobile inbox [_j_] Job search [_S_] Shopping list\n [_w_] website notes [_s_] startpage [_p_] projects folder [_l_] ledger file [_q_] quit"
    :bindings
    ("g" (find-file org-current-file) :exit t)
    ("r" (find-file org-readme-file) :exit t)
    ("d" (find-file org-distractions-file) :exit t)
    ("m" (find-file org-mobile-file) :exit t)
    ("j" (find-file "~/org/js2021.org") :exit t)
    ("w" (find-file "~/PKB/notes/website.org") :exit t)
    ("S" (find-file "~/org/shopping.org") :exit t)
    ("s" (find-file "~/projects/startpage/start.html") :exit t)
    ("l" (find-file "~/finance/ledger.beancount") :exit t)
    ("p" (find-file "~/projects/") :exit t)
    ("q" nil :exit t))

  (global-set-key (kbd "H-g") 'spacemacs/ab|goto-file-transient-state/body)

  (global-set-key (kbd "H-h") 'evil-window-left)
  (global-set-key (kbd "H-l") 'evil-window-right)
  (global-set-key (kbd "H-j") 'evil-window-down)
  (global-set-key (kbd "H-k") 'evil-window-up)

  (global-set-key (kbd "H-s") 'spacemacs/search-engine-select)
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; general user keybindings
  (spacemacs|define-transient-state ab|goto-config-file
    :title "dotfiles menu"
    :doc
    "\n [_e_] emacs [_z_] z-shell [_a_] aliases [_3_] i3wm [_m_] xmonad [_s_] statusbar [_c_] config folder \n [_j_] my emoJis [_q_] quit"
    :bindings
    ("e" (find-file "~/.spacemacs.d/init.el") :exit t)
    ("j" (find-file "~/.config/rofimoji/data/favorites.csv") :exit t)
    ("z" (find-file "~/.zshrc") :exit t)
    ("a" (find-file "~/.config/zsh_aliases") :exit t)
    ("3" (find-file "~/.config/i3/config") :exit t)
    ("m" (find-file "~/dotfiles/xmonad/.xmonad/xmonad.hs") :exit t)
    ("s" (find-file "~/.config/i3status-rust/config.toml") :exit t)
    ("c" (find-file "~/.config/") :exit t)
    ("q" nil :exit t))

  (spacemacs/set-leader-keys "fd" 'spacemacs/ab|goto-config-file-transient-state/body)
  ;; <<< end of special places

  ;; orgmode ecosystem setup

  (setq org-list-allow-alphabetical t)
  (setq org-clock-out-remove-zero-time-clocks t)
  ;; key files for the ecosystem

  (setq org-readme-file "~/org/readme.org")
  (setq org-quotes-file "~/org/quotes.org")
  (setq org-distractions-file "~/org/fun.org")
  (setq org-current-file "~/org/current.org")
  (setq org-blog-file "~/PKB/notes/blog.org")
  (setq org-daily-summary-file "~/org/summaries.org.gpg")
  (setq org-mobile-file "~/Dropbox/orgzly/mobile-refile.org")

  ;; agenda set up
  (setq org-agenda-files
        '("~/org"
          "~/projects/DSPI"
          "~/projects/BDDs"
          "~/projects/DSPI-MCTS-paper"
          "~/projects/align-BDD"
          "~/projects/br-sorting"
          "~/PKB/notes/website.org"
          "~/PKB/notes/res-pipeline.org"
          "~/dotfiles"))

  (setq org-use-fast-todo-selection t)

  (setq org-todo-keywords
        (quote ((sequence "TODO(t)" "WIP(w)" "KTL(k@)" "LATER(l)" "|" "DONE(d!)" "CANCELED(q@!)"))))


  (setq org-tags-exclude-from-inheritance '("keydate")) ;; that was the logic of keydate tag (needed for simple calendar generation)

  ;; a few helper functions
  (defun ab/jump-to-parent-and-fold ()
    "Jumps to the parent heading in the orgmode and folds it."
    (interactive)
    (if (org-at-heading-p)
        (outline-up-heading 1)
      (outline-previous-heading))
    (org-cycle))

  (evil-define-key '(normal insert) org-mode-map (kbd "H-q") 'ab/jump-to-parent-and-fold)
  ;; ============================== The Core: custom agenda setup ==============================================
  (defun ab/get-date-if-not-nil(datearg)
    "Returns date if not nil and nil otherwise"
    (interactive)
    (if datearg
        (format-time-string "%d-%m-%Y" datearg)
      "          "))
  (setq org-agenda-custom-commands
        '(
          ("d" "Daily agenda"
           (
            (agenda "" ((org-agenda-overriding-header "== Calendar: ========================================================================================================="))
                    (org-agenda-sorting-strategy '(priority-down))
                    (org-agenda-skip-function '(org-agenda-skip-entry-if 'done)))
            (todo "WIP"
                       ((org-agenda-overriding-header "== Started / WIP: ====================================================================================================")))
            (todo "KTL"
                       ((org-agenda-overriding-header "== Control / watch / awaiting: ======================================================================================="))
                       (org-agenda-sorting-strategy '(priority-down)))
            (todo "LATER"
                       ((org-agenda-overriding-header "== LATER pool: =======================================================================================================")
                        ))
            (todo "TODO"
                  ((org-agenda-overriding-header "== Not scheduled: ====================================================================================================")
                   (org-agenda-skip-function '(org-agenda-skip-entry-if 'scheduled 'deadline 'timestamp))))))

          ("c" "The calendar plan: key dates."
           ((tags "keydate"
                  ((org-agenda-prefix-format "%c: %i %?-12(concat \"\" (ab/get-date-if-not-nil (org-get-deadline-time nil)) \" -- \" )")
                   (org-agenda-todo-keyword-format "")
                   (org-agenda-sorting-strategy '(deadline-up))
                   (org-agenda-overriding-header "== KEY MILESTONES - calendar plan =========================================\n")
                   (org-agenda-remove-tags t)))))))

  (setq org-agenda-skip-scheduled-if-done t)
  (setq org-agenda-skip-deadline-if-done t)
  (global-set-key (kbd "H-a") 'org-agenda)
  ;; ============================== End of the custom agenda setup ==============================================

  ;; capture setup >>
  (global-set-key (kbd "H-c") 'org-capture)

  (global-set-key (kbd "H-i") 'org-clock-in)
  (global-set-key (kbd "H-o") 'org-clock-out)


  ;; for resolving idle time - ask me if I seem to be out
  (setq org-clock-idle-time 15)
  (setq spaceline-org-clock-p t)

  ;; org-protocol related config
  (use-package org-protocol
    :after org)

  ;; capture templates
  (setq org-capture-templates
        (quote (
                ("p" "=== Project-specific templates === ")
                ("pt" "New TODO (project-specific)" entry (file+headline (lambda () (concat (projectile-project-root)  org-projectile-per-project-filepath)) "Current project TODOs")
                 "* TODO %?\n%U\n%a\n" :prepend t)
                ("pc" "Changelog entry (project-specific)" entry(file+headline (lambda () (concat (projectile-project-root) "CHANGELOG.org")) "Running changelog")
                 "* %U: %? \n%a\n" :prepend t)
                ;; ("n" "Notation note (project-specific)" item (file+headline (lambda () (concat (projectile-project-root)  "notation.org")) "Table of symbols (notation)")
                 ;; "- %?\n")
                ("pn" "General note / assumptions / etc (project-specific)" entry (file+headline (lambda () (concat (projectile-project-root)  org-projectile-per-project-filepath)) "Notes")
                 "* %?\n%U\n%a\n" :prepend t)
                ("pf" "Further work note (project-specific)" entry (file+headline (lambda () (concat (projectile-project-root)  "further.org")) "Notes")
                 "* %?\n%U\n%a\n" :prepend t)
                ("t" "Current TODO (current.org)" entry (file+headline org-current-file "Daily inbox")
                 "* TODO %? \n%a\n" :prepend t)
                ("n" "Current/fleeting note (current.org)" entry (file+headline org-current-file "Daily inbox")
                 "* %? \n%a\n" :prepend t)
                ("b" "========== [b] Bookmarks / readme notes====================")
                ("br" "Research-related entry" entry (file+headline org-readme-file "Research-related notes")
                 "* %a \n Captured: %U\n %?\n")
                ("bg" "General note (link)" entry (file+headline org-readme-file "General notes")
                 "* %a\n Captured: %U\n %?\n\n")
                ("w" "Web note idea (blog)" entry (file+headline org-blog-file "Ideas for notes")
                 "* %?\n%U\n")
                ("d" "A distraction!" entry (file org-distractions-file)
                 "* %?\n Link: %a\n Captured: %U\n")
                ("r" "Daily result" entry (file+olp org-current-file "Daily inbox" "Results")
                 "* %? \n%a\n%U\n" :prepend t)
                ("k" "Things to do with kids" entry (file+olp org-current-file "Kids" "Activity")
                 "* %? \n%a\n%U\n" :prepend t))))

  ;; refiling config
  (setq org-refile-targets '((org-agenda-files :maxlevel . 3)))
  ;; << end of capture setup

  ;; julia-specific customizations >>>
  (defun ab|julia-close-this ()
    "Adds `end' on the next line, with the same indent."
    (interactive)
    (save-excursion (let (cur-indent (current-indentation))
                      (move-end-of-line nil)
                      (newline)
                      (insert "end")
                      (julia-indent-line))))

  (evil-define-key '(normal insert) julia-mode-map (kbd "H-<return>") 'ab|julia-close-this)
  (spacemacs|define-transient-state ab|julia-insert-symbol
    :title "Insert symbol"
    :doc
    "\n [_a_] α [_b_] β [_p_] π [_D_] Δ [_d_] δ\n [_'_] prime [_i_] in [_n_] not in [_q_] quit"
    :bindings
    ("a"  (insert "α") :exit t)
    ("b"  (insert "β") :exit t)
    ("p"  (insert "π") :exit t)
    ("D"  (insert "Δ") :exit t)
    ("d"  (insert "δ") :exit t)
    ("'"  (insert "′") :exit t)
    ("i"  (insert "∈") :exit t)
    ("n"  (insert "∉") :exit t)
    ("q" nil :exit t))

    (evil-define-key '(insert) julia-mode-map (kbd "H-'") 'spacemacs/ab|julia-insert-symbol-transient-state/body)
  ;; <<< end of julia-specific customizations

  ;; using a special ripgrep interface
  (evil-leader/set-key "/" 'deadgrep)

  ;; (setq helm-ag-base-command "rg --vimgrep --no-heading --smart-case")

  (setq yas-snippet-dirs (append yas-snippet-dirs
                                 '("~/.spacemacs.d/snippets"))) ;; append with a personal snippets collection

  ;; ctags config
  (setq projectile-tags-command "ctags -Re --tag-relative=yes --exclude=@.ctagsignore -f \"%s\" %s .")
  ;; (global-set-key (kbd "H-SPC") 'helm-etags-select)
  (global-set-key (kbd "H-SPC") 'counsel-etags-list-tag)

  ;; minor tweaks for orgmode
  (require 'ox-extra) ;; :ignore: feature
  (ox-extras-activate '(ignore-headlines))

  (pixel-scroll-mode)

  ;; prevent ~undo-tree~ files from appearing everywhere
  (setq undo-tree-history-directory-alist '(("." . "~/.spacemacs.d/undo")))

  ;; beancount for money tracking
  (add-to-list 'load-path "~/.spacemacs.d/layers/beancount-mode/")
  (require 'beancount)
  (add-to-list 'auto-mode-alist '("\\.beancount\\'" . beancount-mode))

  (add-hook 'beancount-mode-hook #'outline-minor-mode)  ;; turn on outlining/folding by default
  (define-key beancount-mode-map (kbd "C-c C-n") #'outline-next-visible-heading)
  (define-key beancount-mode-map (kbd "C-c C-p") #'outline-previous-visible-heading)

  ;; keyfreq setup
  (setq keyfreq-file (concat spacemacs-cache-directory "emacs.keyfreq"))
  (keyfreq-mode 1)
  (keyfreq-autosave-mode 1))

;; Do not write anything past this comment. This is where Emacs will
;; auto-generate custom variable definitions.
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
 '(package-selected-packages
   '(toml-mode ron-mode racer rust-mode flycheck-rust cargo csv-mode company-reftex company-math math-symbol-lists company-auctex auctex-latexmk auctex graphviz-dot-mode yapfify stickyfunc-enhance sphinx-doc pytest pyenv-mode pydoc py-isort poetry transient pippel pipenv pyvenv pip-requirements nose lsp-python-ms lsp-pyright live-py-mode importmagic epc ctable concurrent deferred helm-pydoc helm-cscope xcscope cython-mode company-anaconda blacken anaconda-mode pythonic tern npm-mode nodejs-repl livid-mode skewer-mode js2-refactor multiple-cursors js2-mode js-doc import-js grizzl helm-gtags ggtags dap-mode lsp-treemacs bui lsp-mode markdown-mode counsel-gtags yasnippet web-mode web-beautify tagedit slim-mode scss-mode sass-mode pug-mode prettier-js impatient-mode simple-httpd helm-css-scss haml-mode emmet-mode counsel-css counsel swiper ivy company-web web-completion-data company add-node-modules-path ws-butler writeroom-mode winum which-key volatile-highlights vi-tilde-fringe uuidgen use-package undo-tree treemacs-projectile treemacs-persp treemacs-icons-dired treemacs-evil toc-org symon symbol-overlay string-inflection string-edit spaceline-all-the-icons restart-emacs request rainbow-delimiters quickrun popwin pcre2el password-generator paradox ox-hugo ox-gfm overseer org-superstar org-rich-yank org-projectile org-present org-pomodoro org-msg org-mime org-download org-contrib org-cliplink open-junk-file nameless multi-line mu4e-maildirs-extension mu4e-alert macrostep lorem-ipsum link-hint keyfreq inspector info+ indent-guide hybrid-mode hungry-delete hl-todo highlight-parentheses highlight-numbers highlight-indentation helm-xref helm-themes helm-swoop helm-purpose helm-projectile helm-org-rifle helm-org helm-mu helm-mode-manager helm-make helm-ls-git helm-flx helm-descbinds helm-ag google-translate golden-ratio gnuplot font-lock+ flyspell-correct-helm flycheck-pos-tip flycheck-package flycheck-elsa flx-ido fancy-battery eyebrowse expand-region evil-visualstar evil-visual-mark-mode evil-unimpaired evil-tutor evil-textobj-line evil-surround evil-org evil-numbers evil-nerd-commenter evil-mc evil-matchit evil-lisp-state evil-lion evil-indent-plus evil-iedit-state evil-goggles evil-exchange evil-escape evil-ediff evil-easymotion evil-collection evil-cleverparens evil-args evil-anzu eval-sexp-fu emr elisp-slime-nav editorconfig dumb-jump drag-stuff dotenv-mode dired-quick-sort diminish define-word column-enforce-mode clean-aindent-mode centered-cursor-mode auto-highlight-symbol auto-dictionary auto-compile aggressive-indent ace-link ace-jump-helm-line))
 '(warning-suppress-types '((comp))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
)
