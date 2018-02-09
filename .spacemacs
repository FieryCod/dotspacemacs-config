;; -*- mode: emacs-lisp -*-
;; This file is loaded by Spacemacs at startup.
;; It must be stored in your home directory.
(defun dotspacemacs/layers ()
  "Layer configuration: This function should only modify configuration layer settings."
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
   dotspacemacs-configuration-layer-path '()
   ;; List of configuration layers to load.
   dotspacemacs-configuration-layers '((auto-completion :variables
                                                        auto-completion-enable-snippets-in-popup t
                                                        auto-completion-return-key-behavior nil
                                                        auto-completion-enable-help-tooltip t
                                                        auto-completion-enable-sort-by-usage t
                                                        auto-completion-tab-key-behavior 'complete
                                                        spacemacs-default-company-backends '(company-files company-capf company-anaconda company-keywords company-ispell))
                                       javascript
                                       csv
                                       better-defaults
                                       clojure
                                       html
                                       github
                                       emacs-lisp
                                       git
                                       syntax-checking
                                       (spell-checking :variables spell-checking-enable-auto-dictionary t)
                                       ivy
                                       version-control
                                       neotree
                                       yaml
                                       parinfer
                                       sql
                                       python ;; Note that you can use YAPF to auto-format the code
                                       colors
                                       markdown
                                       org
                                       (semantic :disabled-for emacs-lisp) ;; disables hangs on emacs-lisp
                                       (shell :variables
                                              shell-default-height 10
                                              shell-default-position 'bottom
                                              shell-default-term-shell "/bin/zsh"))

   ;; List of additional packages that will be installed without being
   ;; wrapped in a layer. If you need some configuration for these
   ;; packages, then consider creating a layer. You can also put the
   ;; configuration in `dotspacemacs/user-config'.
   dotspacemacs-additional-packages '(highlight-indent-guides
                                      dracula-theme
                                      tide
                                      vue-mode
                                      focus
                                      4clojure
                                      rainbow-delimiters
                                      editorconfig
                                      edit-indirect
                                      git
                                      edit-server
                                      spaceline-all-the-icons
                                      all-the-icons-dired
                                      all-the-icons-gnus
                                      all-the-icons-ivy
                                      anaconda-mode
                                      company-anaconda
                                      indium
                                      sphinx-doc)

   ;; A list of packages that cannot be updated.
   dotspacemacs-frozen-packages '()
   ;; A list of packages that will not be installed and loaded.
   dotspacemacs-excluded-packages '(tern company-tern)

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
   ;; If non-nil then spacemacs will check for updates at startup
   ;; when the current branch is not `develop'. Note that checking for
   ;; new versions works via git commands, thus it calls GitHub services
   ;; whenever you start Emacs. (default nil)
   dotspacemacs-check-for-update nil
   ;; Set the default modeline theme to all-the-icons
   ;; If non-nil, a form that evaluates to a package directory. For example, to
   ;; use different package directories for different Emacs versions, set this
   ;; to `emacs-version'. (default nil)
   dotspacemacs-elpa-subdirectory nil
   dotspacemacs-mode-line-theme 'all-the-icons
   ;; One of `vim', `emacs' or `hybrid'.
   ;; `hybrid' is like `vim' except that `insert state' is replaced by the
   ;; `hybrid state' with `emacs' key bindings. The value can also be a list
   ;; with `:variables' keyword (similar to layers). Check the editing styles
   ;; section of the documentation for details on available variables.
   ;; (default 'vim)
   dotspacemacs-editing-style 'vim
   ;; If non-nil output loading progress in `*Messages*' buffer. (default nil)
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
   ;; `recents' `bookmarks' `projects' `agenda' `todos'.
   ;; List sizes may be nil, in which case
   ;; `spacemacs-buffer-startup-lists-length' takes effect.
   dotspacemacs-startup-lists '((recents . 5)
                                (projects . 7))
   ;; True if the home buffer should respond to resize events. (default t)
   dotspacemacs-startup-buffer-responsive t
   ;; Default major mode of the scratch buffer (default `text-mode')
   dotspacemacs-scratch-mode 'text-mode
   ;; List of themes, the first of the list is loaded when spacemacs starts.
   ;; Press `SPC T n' to cycle to the next theme in the list (works great
   ;; with 2 themes variants, one dark and one light)
   dotspacemacs-themes '(dracula
                         doom-one
                         spacemacs-dark
                         spacemacs-light)
   ;; If non-nil the cursor color matches the state color in GUI Emacs.
   ;; (default t)
   dotspacemacs-colorize-cursor-according-to-state t
   ;; Default font, or prioritized list of fonts. `powerline-scale' allows to
   ;; quickly tweak the mode-line size to make separators look not too crappy.
   dotspacemacs-default-font '("Fantasque Sans Mono"
                               :size 18
                               :weight normal
                               :width normal
                               :powerline-scale 1.0)

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
   ;; (default "C-M-m")
   dotspacemacs-major-mode-emacs-leader-key "C-M-m"
   ;; These variables control whether separate commands are bound in the GUI to
   ;; the key pairs `C-i', `TAB' and `C-m', `RET'.
   ;; Setting it to a non-nil value, allows for separate commands under `C-i'
   ;; and TAB or `C-m' and `RET'.
   ;; In the terminal, these pairs are generally indistinguishable, so this only
   ;; works in the GUI. (default nil)
   dotspacemacs-distinguish-gui-tab nil
   ;; If non-nil `Y' is remapped to `y$' in Evil states. (default nil)
   dotspacemacs-remap-Y-to-y$ nil
   ;; If non-nil, the shift mappings `<' and `>' retain visual state if used
   ;; there. (default t)
   dotspacemacs-retain-visual-state-on-shift nil
   ;; If non-nil, `J' and `K' move lines up and down when in visual mode.
   ;; (default nil)
   dotspacemacs-visual-line-move-text t
   ;; If non-nil, inverse the meaning of `g' in `:substitute' Evil ex-command.
   ;; (default nil)
   dotspacemacs-ex-substitute-global nil
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
   ;; If non-nil, the paste transient-state is enabled. While enabled, pressing
   ;; `p' several times cycles through the elements in the `kill-ring'.
   ;; (default nil)
   dotspacemacs-enable-paste-transient-state nil
   ;; Which-key delay in seconds. The which-key buffer is the popup listing
   ;; the commands bound to the current keystroke sequence. (default 0.4)
   dotspacemacs-which-key-delay 0.1
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
   dotspacemacs-loading-progress-bar nil
   ;; If non-nil the frame is fullscreen when Emacs starts up. (default nil)
   ;; (Emacs 24.4+ only)
   dotspacemacs-fullscreen-at-startup nil
   ;; If non-nil `spacemacs/toggle-fullscreen' will not use native fullscreen.
   ;; Use to disable fullscreen animations in OSX. (default nil)
   dotspacemacs-fullscreen-use-non-native nil
   ;; If non-nil the frame is maximized when Emacs starts up.
   ;; Takes effect only if `dotspacemacs-fullscreen-at-startup' is nil.
   ;; (default nil) (Emacs 24.4+ only)
   dotspacemacs-maximized-at-startup t
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
   ;; If non-nil unicode symbols are displayed in the mode line. (default t)
   dotspacemacs-mode-line-unicode-symbols t
   ;; If non-nil smooth scrolling (native-scrolling) is enabled. Smooth
   ;; scrolling overrides the default behavior of Emacs which recenters point
   ;; when it reaches the top or bottom of the screen. (default t)
   dotspacemacs-smooth-scrolling nil
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
   dotspacemacs-line-numbers t
   ;; Code folding method. Possible values are `evil' and `origami'.
   ;; (default 'evil)
   dotspacemacs-folding-method 'evil
   ;; If non-nil `smartparens-strict-mode' will be enabled in programming modes.
   ;; (default nil)
   dotspacemacs-smartparens-strict-mode nil
   ;; If non-nil pressing the closing parenthesis `)' key in insert mode passes
   ;; over any automatically added closing parenthesis, bracket, quote, etc…
   ;; This can be temporary disabled by pressing `C-q' before `)'. (default nil)
   dotspacemacs-smart-closing-parenthesis t
   ;; Select a scope to highlight delimiters. Possible values are `any',
   ;; `current', `all' or `nil'. Default is `all' (highlight any scope and
   ;; emphasis the current one). (default 'all)
   dotspacemacs-highlight-delimiters 'all
   ;; If non-nil, advise quit functions to keep server open when quitting.
   ;; (default nil)
   dotspacemacs-persistent-server nil
   ;; List of search tool executable names. Spacemacs uses the first installed
   ;; tool of the list. Supported tools are `rg', `ag', `pt', `ack' and `grep'.
   ;; (default '("rg" "ag" "pt" "ack" "grep"))
   dotspacemacs-search-tools '("rg" "ag" "pt" "ack" "grep")
   ;; The default package repository used if no explicit repository has been
   ;; specified with an installed package.
   ;; Not used for now. (default nil)
   dotspacemacs-default-package-repository nil
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
   ;; (default "%I@%S")
   dotspacemacs-frame-title-format "%I@%S"
   ;; Format specification for setting the icon title format
   ;; (default nil - same as frame-title-format)
   dotspacemacs-icon-title-format t
   ;; Delete whitespace while saving buffer. Possible values are `all'
   ;; to aggressively delete empty line and long sequences of whitespace,
   ;; `trailing' to delete only the whitespace at end of lines, `changed' to
   ;; delete only whitespace for changed lines or `nil' to disable cleanup.
   ;; (default nil)
   dotspacemacs-whitespace-cleanup 'trailing
   ;; Either nil or a number of seconds. If non-nil zone out after the specified
   ;; number of seconds. (default nil)
   dotspacemacs-zone-out-when-idle nil
   ;; Run `spacemacs/prettify-org-buffer' when
   ;; visiting README.org files of Spacemacs.
   ;; (default nil)
   dotspacemacs-pretty-docs t))

(defun dotspacemacs/user-init ()
  "Initialization for user code:
This function is called immediately after `dotspacemacs/init', before layer
configuration.
It is mostly for variables that should be set before packages are loaded.
If you are unsure, try setting them in `dotspacemacs/user-config' first."

  ;; add some defaults
  (setq-default
   ;; disable wordwrap
   truncate-lines t

   ;; js2-mode
   js2-basic-offset 2
   js-indent-level 2

   ;; web-mode
   css-indent-offset 2
   web-mode-markup-indent-offset 2
   web-mode-css-indent-offset 2
   web-mode-code-indent-offset 2
   web-mode-attr-indent-offset 2
   electric-indent-inhibit t))

(defun edit-indirect-scss (begin end)
  "Enables to quickly edit scss mode in the indirect buffer"
  (interactive "r")
  (switch-to-buffer (edit-indirect-region begin end))
  (scss-mode))

(defun setup-tide-mode ()
  "Setup tide mode for javascript files"
  (interactive)
  (tide-setup)
  (flycheck-mode +1)
  (eldoc-mode +1)
  (tide-hl-identifier-mode +1)
  (company-mode +1)
  (flycheck-add-next-checker 'javascript-eslint 'javascript-tide 'append))

(defun read-current-buffer ()
  "Reads the current buffer"
  (buffer-substring-no-properties 1 (buffer-size)))

(defun get-area-start-pos (regex string)
  "Get start pos of template, script, style area in .vue file"
  (let ((regex-start-pos (string-match regex string 0))
        (start-string (match-string 0 string)))
    (+ regex-start-pos (length start-string) 2)))

;; This can be done more smart using setq etc.. ask @FieryCod for more information if needed
;; BUG: If template has script, or style then edit-vuejs-js and edit-vuejs-scss don't work
;; Either use proper regexp or refactor in smarter way
(defun edit-vuejs-template ()
  "Enables to quickly edit vuejs-template in the indirect buffer"
  (interactive)
  (let* ((start-pos (get-area-start-pos "\\(<template.*\\)" (read-current-buffer)))
         (curr-overlay (mmm-overlay-at start-pos)))
    (goto-char start-pos)
    (switch-to-buffer (edit-indirect-region (overlay-start curr-overlay) (overlay-end curr-overlay)))
    (vue-html-mode)
    (goto-char 0)))

(defun edit-vuejs-js ()
  "Enables to quickly edit vuejs-js in the indirect buffer"
  (interactive)
  (let* ((start-pos (get-area-start-pos "\\(<script.*\\)" (read-current-buffer)))
         (curr-overlay (mmm-overlay-at start-pos)))
    (goto-char start-pos)
    (switch-to-buffer (edit-indirect-region (overlay-start curr-overlay) (overlay-end curr-overlay)))
    (js2-mode)
    (goto-char 0)))

(defun edit-vuejs-scss ()
  "Enables to quickly edit vuejs-style in the indirect buffer"
  (interactive)
  (let* ((start-pos (get-area-start-pos "\\(<style.*\\)" (read-current-buffer)))
         (curr-overlay (mmm-overlay-at start-pos)))
    (goto-char start-pos)
    (switch-to-buffer (edit-indirect-region (overlay-start curr-overlay) (overlay-end curr-overlay)))
    (scss-mode)
    (goto-char 0)))

(defun dotspacemacs/user-config ()
  "Configuration for user code:
This function is called at the very end of Spacemacs startup, after layer
configuration.
Put your configuration code here, except for variables that should be set
before packages are loaded."

  ;; Always save buffer
  (defadvice save-buffer (before save-buffer-always activate)
    (set-buffer-modified-p t))

  ;; Save the buffer while navigating to the other window, frame, buffer
  (defadvice switch-to-buffer (before save-buffer-now activate)
    (when buffer-file-name (save-buffer)))
  (defadvice other-window (before other-window-now activate)
    (when buffer-file-name (save-buffer)))
  (defadvice other-frame (before other-frame-now activate)
    (when buffer-file-name (save-buffer)))

  ;; Start Emacs server (works with google-chrome extension)
  (edit-server-start)

  ;; Disables auto-compile which hangs the Emacs while editing .spacemacs file
  (remove-hook 'emacs-lisp-mode-hook 'auto-compile-mode)
  (add-hook 'emacs-lisp-mode-hook (lambda () (rainbow-delimiters-mode 1 )) 'append)

  ;; Adds some great coloring for the modes
  (add-hook 'after-init-hook 'global-color-identifiers-mode)
  (set-face-attribute 'font-lock-comment-delimiter-face nil :slant 'italic)
  (set-face-attribute 'font-lock-comment-face nil :slant 'italic)
  (set-face-attribute 'font-lock-doc-face nil :slant 'italic)

  ;; Add support for CSV
  (setq csv-separators '(";" "\t"))
  (setq csv-field-quotes '("\"" "'"))
  (setq csv-align-padding 2)
  (setq csv-header-lines 1)

  ;; Fix for the wrong output in commands from other shells used
  (setq explicit-shell-file-name "/bin/bash")
  (setq shell-file-name "bash")

  ;; Aligns annotation to the right hand side
  (setq company-tooltip-align-annotations t)

  ;; Run linter on save
  (setq flycheck-check-syntax-automatically '(save mode-enabled))

  ;; Enable great paradox menu
  (setq paradox-github-token "INSERT YOUR TOKEN HERE") ;; Look for paradox package

  ;; Customize neo-tree theme
  (setq neo-theme (if (display-graphic-p) 'icons 'cup))

  ;; Disable the startup message and compacting font caches
  (setq inhibit-startup-message t)
  (setq inhibit-compacting-font-caches t)

  ;; Setup projectile caching
  (setq projectile-enable-caching nil)

  ;; Setup google translate
  (setq google-translate-default-target-language "pl")

  ;; Enable spaceline-all-the-icons and customize it
  (use-package spaceline-all-the-icons
    :after spaceline
    :config
    (setq spaceline-all-the-icons-highlight-file-name t)
    (setq spaceline-all-the-icons-separator-type 'none)
    (spaceline-all-the-icons--setup-anzu)             ;; Enable anzu searching
    (spaceline-all-the-icons--setup-neotree))         ;; Enable Neotree mode line

  ;; customize indent-guide
  (require 'highlight-indent-guides)
  (use-package highlight-indent-guides
    :init
    (add-hook 'prog-mode-hook 'highlight-indent-guides-mode)
    (add-hook 'mmm-mode-hook (lambda () (highlight-indent-guides-mode 1)))
    :config (setq highlight-indent-guides-method 'character
                  highlight-indent-guides-character ?|
                  highlight-indent-guides-auto-character-face-perc 15)) ;; customize margin-mode

  ;; Setup for git-gutter-fringe+
  (use-package git-gutter-fringe+ :ensure t :defer 3
    :if window-system
    :config
    (setq git-gutter-fr+-side 'right-fringe)
    (defun theme-git-gutter (&rest args) "Set the background colour of the git-gutter faces"
           (set-face-attribute 'git-gutter-fr+-added nil :foreground (face-foreground 'success) :background (face-foreground 'success))
           (set-face-attribute 'git-gutter-fr+-deleted nil :foreground (face-foreground 'error) :background (face-foreground 'error))
           (set-face-attribute 'git-gutter-fr+-modified nil :foreground (face-foreground 'warning) :background (face-foreground 'warning)))
    (theme-git-gutter)
    (advice-add 'load-theme :after 'theme-git-gutter)
    (global-git-gutter+-mode))

  ;; Setup all the icons for ivy
  (all-the-icons-ivy-setup)

  ;; Customize dired mode with all-the-icons-dired-mode
  (add-hook 'dired-mode-hook 'all-the-icons-dired-mode)

  ;; Add hook for text-mode
  (add-hook 'text-mode-hook 'smartparens-mode)

  ;; Speedup web-mode validation
  (setq web-mode-enable-block-partial-invalidation t)

  ;; Disable wrong highlights of smartparens (Do not delete in any case)
  (with-eval-after-load 'smartparens
    (show-smartparens-global-mode -1))

  ;; Config for python mode
  (add-hook 'python-mode-hook (lambda ()
                                (require 'sphinx-doc)
                                (sphinx-doc-mode t)
                                (flycheck-mode 1)
                                (setq flycheck-checker 'python-pylint
                                      flycheck-checker-error-threshold 900
                                      flycheck-pylintrc ".pylintrc")
                                (setq python-indent-offset 2)
                                (setq python-indent-guess-indent-offset nil)))

  ;; Config for vue.js, nuxt.js
  (add-hook 'vue-mode-hook (lambda () (semantic-mode -1)) 'append)
  (setq mmm-submode-decoration-level 0)

  ;; Add config for scss files
  (add-to-list 'auto-mode-alist '("\\.scss\\'" . scss-mode))
  (add-hook 'scss-mode-hook (lambda () (rainbow-mode 1)) 'append)

  ;; Add support for .jqtpl
  (add-to-list 'auto-mode-alist '("\\.jqtpl\\'" . web-mode))

  ;; Config for .cdt
  (add-to-list 'auto-mode-alist '("\\.cdt\\'" . js2-mode))

  ;; Turn off js2 mode errors & warnings (we lean on eslint/standard)
  (setq js2-mode-show-parse-errors nil)
  (setq js2-mode-show-strict-warnings nil)

  ;; Add support for intellisense as in vscode
  (setq tide-format-options '(:indentSize 2 :tabSize 2 :insertSpaceAfterFunctionKeywordForAnonymousFunctions t :placeOpenBraceOnNewLineForFunctions nil))
  (add-hook 'js2-mode-hook 'setup-tide-mode)

  ;; Special mode for clojure development
  (add-hook 'cider-mode-hook (lambda () (rainbow-delimiters-mode 1 )) 'append)
  (setq clojure-enable-fancify-symbols t)

  ;; Adds some basic indirect keybindings
  (global-set-key (kbd "C-c C-c r") 'edit-indirect-region)
  (global-set-key (kbd "C-c C-c t") 'edit-vuejs-template)
  (global-set-key (kbd "C-c C-c j") 'edit-vuejs-js)
  (global-set-key (kbd "C-c C-c s") 'edit-vuejs-scss)

  ;; Setup the key to search with two characters
  ;; @INFO: This is how function should be bind to kbd
  (spacemacs/set-leader-keys (kbd "j W") 'avy-goto-char-2)
  (spacemacs/set-leader-keys (kbd "b C-r") 'revert-buffer)

  ;; Disable some wrong defaults and add some global modes
  (setq evil-move-cursor-back nil)
  (setq fci-rule-column 120)
  (fset 'evil-visual-update-x-selection 'ignore)
  (golden-ratio-mode 1)
  (whitespace-mode 1)
  (editorconfig-mode 1)
  (semantic-mode 1) ;; One of the best package you MUST CHECK IT OUT
  (global-company-mode)

  ;; Fix for the web-mode do not delete
  (add-hook 'web-mode-hook (lambda () (company-mode -1)) 'append)
  (add-hook 'web-mode-hook (lambda () (smartparens-mode 1)) 'append)

  ;; Configure flycheck and flyspell
  (define-key flyspell-mode-map (kbd "C-;") 'flyspell-correct-previous-word-generic)
  (add-hook 'hack-local-variables-hook (lambda () (setq truncate-lines t)))
  (add-hook 'spacemacs-buffer-mode-hook (lambda ()
                                          (set (make-local-variable 'mouse-1-click-follows-link) nil))))
