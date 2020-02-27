;; -*- mode: emacs-lisp; lexical-binding: t -*-
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
   dotspacemacs-enable-lazy-installation 'nil

   ;; If non-nil then Spacemacs will ask for confirmation before installing
   ;; a layer lazily. (default t)
   dotspacemacs-ask-for-lazy-installation t

   ;; If non-nil layers with lazy install support are lazy installed.
   ;; List f additional paths where to look for configuration layers.
   ;; Paths must have a trailing slash (i.e. `~/.mycontribs/')
   dotspacemacs-configuration-layer-path '()

   ;; List of configuration layers to load.
   dotspacemacs-configuration-layers '(
                                       (auto-completion :variables
                                                        auto-completion-enable-snippets-in-popup nil
                                                        auto-completion-return-key-behavior nil
                                                        auto-completion-enable-help-tooltip t
                                                        auto-completion-enable-sort-by-usage t
                                                        auto-completion-complete-with-key-sequence-delay 0.3
                                                        auto-completion-tab-key-behavior 'complete)

                                       (terraform :variables terraform-auto-format-on-save t)

                                       asciidoc
                                       (spell-checking :variables spell-checking-enable-auto-dictionary t)
                                       coffeescript

                                       (rust :variables
                                             rust-backend 'lsp
                                             rust-format-on-save t)

                                       lsp

                                       (erc :variables
                                            erc-enable-logging t
                                            erc-enable-notifications t
                                            erc-server-list '(("irc.freenode.net"
                                                               :port "6697"
                                                               :ssl t
                                                               :nick "FieryCod"
                                                               :password nil)))

                                       multiple-cursors

                                       ;; Python layer config
                                       (python :variables
                                               python-backend 'anaconda)

                                       ;; Git layer config
                                       git github

                                       ;; Version-control layer config
                                       version-control

                                       dap

                                       ;; Org layer config
                                       (org :variables
                                            org-enable-github-support t)

                                       ;; Shell layer config
                                       (shell :variables
                                              shell-default-height 35
                                              shell-default-position 'bottom
                                              shell-default-shell 'multi-term
                                              shell-default-term-shell "/bin/zsh")

                                       ;; Syntax checking
                                       (syntax-checking :variables
                                                        syntax-checking-enable-by-default t
                                                        syntax-checking-enable-tooltips t)

                                       (treemacs :variables
                                                 treemacs-use-filewatch-mode nil
                                                 treemacs-use-follow-mode nil
                                                 treemacs-silent-refresh nil)

                                       plantuml
                                       (typescript :variables
                                                   typescript-backend 'tide)

                                       (ruby :variables
                                             ruby-version-manager 'rvm
                                             ruby-enable-enh-ruby-mode t)

                                       (clojure :variables
                                                clojure-enable-fancify-symbols t
                                                clojure-enable-sayid t
                                                clojure-enable-linters 'joker
                                                clojure-enable-clj-refactor t)

                                       ivy

                                       (java :variables
                                             java-backend 'lsp)

                                       (go :variables
                                           go-use-gometalinter t
                                           go-backend 'lsp)

                                       nginx ruby-on-rails yaml
                                       colors spotify
                                       ;; haskell
                                       docker javascript evil-commentary
                                       csv better-defaults
                                       html github emacs-lisp
                                       plantuml sql)
   ;; List of additional packages that will be installed without being
   ;; wrapped in a layer. If you need some configuration for these
   ;; packages, then consider creating a layer. You can also put the
   ;; configuration in `dotspacemacs/user-config'.
   dotspacemacs-additional-packages '(highlight-indent-guides color-theme-sanityinc-tomorrow graphql-mode
                                      terraform-mode company-terraform org-fancy-priorities
                                      company-shell rjsx-mode pylint
                                      elisp-format flycheck-clojure
                                      rich-minority exec-path-from-shell
                                      writeroom-mode doom-themes focus 4clojure rainbow-delimiters
                                      editorconfig edit-indirect git
                                      all-the-icons-gnus all-the-icons-ivy
                                      anaconda-mode company-anaconda indium sphinx-doc)

   ;; A list of packages that cannot be updated.
   dotspacemacs-frozen-packages '()
   ;; A list of packages that will not be installed and loaded.
   dotspacemacs-excluded-packages '(tern company-tern clojure-cheatsheet)

   ;; Defines the behaviour of Spacemacs when installing packages.
   ;; Possible values are `used-only', `used-but-keep-unused' and `all'.
   ;; `used-only' installs only explicitly used packages and deletes any unused
   ;; packages as well as their unused dependencies. `used-but-keep-unused'
   ;; installs only the used packages but won't delete unused ones. `all'
   ;; installs *all* packages supported by Spacemacs and never uninstalls them.
   ;; (default is `used-only')
   dotspacemacs-install-packages 'used-only))

(defun dotspacemacs/init ()
  "Initialization: This function is called at the very beginning of Spacemacs startup,
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
   ;;   ./emacs --dump-file=~/.emacs.d/.cache/dumps/spacemacs.pdmp
   ;; (default spacemacs.pdmp)
   dotspacemacs-emacs-dumper-dump-file "spacemacs.pdmp"

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

   ;; If non-nil then Spacelpa repository is the primary source to install
   ;; a locked version of packages. If nil then Spacemacs will install the
   ;; latest version of packages from MELPA. (default nil)
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

   ;; If non-nil output loading progress in `*Messages*' buffer. (default nil)
   dotspacemacs-verbose-loading t

   ;; Specify the startup banner. Default value is `official', it displays
   ;; the official spacemacs logo. An integer value is the index of text
   ;; banner, `random' chooses a random text banner in `core/banners'
   ;; directory. A string value must be a path to an image format supported
   ;; by your Emacs build.
   ;; If the value is nil then no banner is displayed. (default 'official)
   dotspacemacs-startup-banner nil

   ;; List of items to show in startup buffer or an association list of
   ;; the form `(list-type . list-size)`. If nil then it is disabled.
   ;; Possible values for list-type are:
   ;; `recents' `bookmarks' `projects' `agenda' `todos'.
   ;; List sizes may be nil, in which case
   ;; `spacemacs-buffer-startup-lists-length' takes effect.
   dotspacemacs-startup-lists '((agenda . 5)
                                (recents . 3))

   ;; True if the home buffer should respond to resize events. (default t)
   dotspacemacs-startup-buffer-responsive t

   ;; Default major mode for a new empty buffer. Possible values are mode
   ;; names such as `text-mode'; and `nil' to use Fundamental mode.
   ;; (default `text-mode')
   dotspacemacs-new-empty-buffer-major-mode 'text-mode

   ;; Default major mode of the scratch buffer (default `text-mode')
   dotspacemacs-scratch-mode 'text-mode

   ;; Initial message in the scratch buffer, such as "Welcome to Spacemacs!"
   ;; (default nil)
   dotspacemacs-initial-scratch-message nil

   ;; List of themes, the first of the list is loaded when spacemacs starts.
   ;; Press `SPC T n' to cycle to the next theme in the list (works great
   ;; with 2 themes variants, one dark and one light)
   dotspacemacs-themes '(doom-dracula
                         spacemacs-dark
                         doom-tomorrow-night
                         doom-one
                         spacemacs-light)

   ;; Set the theme for the Spaceline. Supported themes are `spacemacs',
   ;; `all-the-icons', `custom', `vim-powerline' and `vanilla'. The first three
   ;; are spaceline themes. `vanilla' is default Emacs mode-line. `custom' is a
   ;; user defined themes, refer to the DOCUMENTATION.org for more info on how
   ;; to create your own spaceline theme. Value can be a symbol or list with\
   ;; additional properties.
   ;; (default '(spacemacs :separator wave :separator-scale 1.5))
   dotspacemacs-mode-line-theme nil
   ;; '(doom :separator nil
   ;;        :separator-scale 1.5)

   ;; If non-nil the cursor color matches the state color in GUI Emacs.
   ;; (default t)
   dotspacemacs-colorize-cursor-according-to-state t

   ;; Default font, or prioritized list of fonts. `powerline-scale' allows to
   ;; quickly tweak the mode-line size to make separators look not too crappy.
   ;; dotspacemacs-default-font '("Fira Code"
   ;;                             :size 16
   ;;                             :weight normal
   ;;                             :width normal
   ;;                             :powerline-scale 0.2)
   ;; PragmataPro Mono Liga
   dotspacemacs-default-font '("PragmataPro Liga"
                               :size 18
                               :weight normal
                               :width normal
                               :powerline-scale 0.2)

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
   dotspacemacs-distinguish-gui-tab t

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

   ;; If non-nil, the paste transient-state is enabled. While enabled, after you
   ;; paste something, pressing `C-j' and `C-k' several times cycles through the
   dotspacemacs-enable-paste-transient-state nil

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
   dotspacemacs-loading-progress-bar nil

   ;; If non-nil the frame is fullscreen when Emacs starts up. (default nil)
   ;; (Emacs 24.4+ only)
   dotspacemacs-fullscreen-at-startup t

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
   dotspacemacs-undecorated-at-startup nil

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
   dotspacemacs-mode-line-unicode-symbols nil

   ;; If non-nil smooth scrolling (native-scrolling) is enabled. Smooth
   ;; scrolling overrides the default behavior of Emacs which recenters point
   ;; when it reaches the top or bottom of the screen. (default t)
   dotspacemacs-smooth-scrolling nil
   ;; Control line numbers activation.
   ;; If set to `t', `relative' or `visual' then line numbers are enabled in all
   ;; `prog-mode' and `text-mode' derivatives. If set to `relative', line
   ;; numbers are relative. If set to `visual', line numbers are also relative,
   ;; but lines are only visual lines are counted. For example, folded lines
   ;; will not be counted and wrapped lines are counted as multiple lines.
   ;; This variable can also be set to a property list for finer control:
   ;; '(:relative nil
   ;;   :disabled-for-modes dired-mode
   ;;                       doc-view-mode
   ;;                       markdown-mode
   ;;                       org-mode
   ;;                       pdf-view-mode
   ;;                       text-mode
   ;;   :size-limit-kb 1000)
   ;; When used in a plist, `visual' takes precedence over `relative'.
   ;; (default nil)
   dotspacemacs-line-numbers nil

   ;; Code folding method. Possible values are `evil' and `origami'.
   ;; (default 'evil)
   dotspacemacs-folding-method 'evil

   ;; If non-nil `smartparens-strict-mode' will be enabled in programming modes.
   ;; (default nil)
   dotspacemacs-smartparens-strict-mode nil

   ;; If non-nil pressing the closing parenthesis `)' key in insert mode passes
   ;; over any automatically added closing parenthesis, bracket, quote, etc...
   ;; This can be temporary disabled by pressing `C-q' before `)'. (default nil)
   dotspacemacs-smart-closing-parenthesis t

   ;; Select a scope to highlight delimiters. Possible values are `any',
   ;; `current', `all' or `nil'. Default is `all' (highlight any scope and
   ;; emphasis the current one). (default 'all)
   dotspacemacs-highlight-delimiters 'all

   ;; If non-nil, start an Emacs server if one is not already running.
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
   ;; (default "%I@%S")
   dotspacemacs-frame-title-format "%I@%S"

   ;; Format specification for setting the icon title format
   ;; (default nil - same as frame-title-format)
   dotspacemacs-icon-title-format nil

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

(setq prettify-symbols-unprettify-at-point t)

(defconst pragmatapro-prettify-symbols-alist
  (mapcar (lambda (s)
            `(,(car s)
              .
              ,(vconcat
                (apply 'vconcat
                       (make-list
                        (- (length (car s)) 1)
                        (vector (decode-char 'ucs #X0020) '(Br . Bl))))
                (vector (decode-char 'ucs (cadr s))))))
          '(("[ERROR]"    #XE2C0)
            ("[DEBUG]"    #XE2C1)
            ("[INFO]"     #XE2C2)
            ("[WARN]"     #XE2C3)
            ("[WARNING]"  #XE2C4)
            ("[ERR]"      #XE2C5)
            ("[FATAL]"    #XE2C6)
            ("[TRACE]"    #XE2C7)
            ("[FIXME]"    #XE2C8)
            ("[TODO]"     #XE2C9)
            ("[TODO]"     #XE2C9)
            ("[BUG]"      #XE2CA)
            ("[NOTE]"     #XE2CB)
            ("[HACK]"     #XE2CC)
            ("[MARK]"     #XE2CD)
            ("# ERROR"    #XE2F0)
            ("# DEBUG"    #XE2F1)
            ("# INFO"     #XE2F2)
            ("# WARN"     #XE2F3)
            ("# WARNING"  #XE2F4)
            ("# ERR"      #XE2F5)
            ("# FATAL"    #XE2F6)
            ("# TRACE"    #XE2F7)
            ("# FIXME"    #XE2F8)
            ("# TODO"     #XE2F9)
            ("# BUG"      #XE2FA)
            ("# NOTE"     #XE2FB)
            ("# HACK"     #XE2FC)
            ("# MARK"     #XE2FD)
            ("// ERROR"   #XE2E0)
            ("// DEBUG"   #XE2E1)
            ("// INFO"    #XE2E2)
            ("// WARN"    #XE2E3)
            ("// WARNING" #XE2E4)
            ("// ERR"     #XE2E5)
            ("// FATAL"   #XE2E6)
            ("// TRACE"   #XE2E7)
            ("// FIXME"   #XE2E8)
            ("// TODO"    #XE2E9)
            ("// BUG"     #XE2EA)
            ("// NOTE"    #XE2EB)
            ("// HACK"    #XE2EC)
            ("// MARK"    #XE2ED)
            ("!!"         #XE900)
            ("!="         #XE901)
            ("!=="        #XE902)
            ("!!!"        #XE903)
            ("!≡"         #XE904)
            ("!≡≡"        #XE905)
            ("!>"         #XE906)
            ("!=<"        #XE907)
            ("#("         #XE920)
            ("#_"         #XE921)
            ("#{"         #XE922)
            ("#?"         #XE923)
            ("#>"         #XE924)
            ("##"         #XE925)
            ("#_("        #XE926)
            ("%="         #XE930)
            ("%>"         #XE931)
            ("%>%"        #XE932)
            ("%<%"        #XE933)
            ("&%"         #XE940)
            ("&&"         #XE941)
            ("&*"         #XE942)
            ("&+"         #XE943)
            ("&-"         #XE944)
            ("&/"         #XE945)
            ("&="         #XE946)
            ("&&&"        #XE947)
            ("&>"         #XE948)
            ("$>"         #XE955)
            ("***"        #XE960)
            ("*="         #XE961)
            ("*/"         #XE962)
            ("*>"         #XE963)
            ("++"         #XE970)
            ("+++"        #XE971)
            ("+="         #XE972)
            ("+>"         #XE973)
            ("++="        #XE974)
            ("--"         #XE980)
            ("-<"         #XE981)
            ("-<<"        #XE982)
            ("-="         #XE983)
            ("->"         #XE984)
            ("->>"        #XE985)
            ("---"        #XE986)
            ("-->"        #XE987)
            ("-+-"        #XE988)
            ("-\\/"       #XE989)
            ("-|>"        #XE98A)
            ("-<|"        #XE98B)
            (".."         #XE990)
            ("..."        #XE991)
            ("..<"        #XE992)
            (".>"         #XE993)
            (".~"         #XE994)
            (".="         #XE995)
            ("/*"         #XE9A0)
            ("//"         #XE9A1)
            ("/>"         #XE9A2)
            ("/="         #XE9A3)
            ("/=="        #XE9A4)
            ("///"        #XE9A5)
            ("/**"        #XE9A6)
            (":::"        #XE9AF)
            ("::"         #XE9B0)
            (":="         #XE9B1)
            (":≡"         #XE9B2)
            (":>"         #XE9B3)
            (":=>"        #XE9B4)
            (":("         #XE9B5)
            (":-("        #XE9B6)
            (":)"         #XE9B7)
            (":-)"        #XE9B8)
            (":/"         #XE9B9)
            (":\\"        #XE9BA)
            (":3"         #XE9BB)
            (":D"         #XE9BC)
            (":P"         #XE9BD)
            (":>:"        #XE9BE)
            (":<:"        #XE9BF)
            ("<$>"        #XE9C0)
            ("<*"         #XE9C1)
            ("<*>"        #XE9C2)
            ("<+>"        #XE9C3)
            ("<-"         #XE9C4)
            ("<<"         #XE9C5)
            ("<<<"        #XE9C6)
            ("<<="        #XE9C7)
            ("<="         #XE9C8)
            ("<=>"        #XE9C9)
            ("<>"         #XE9CA)
            ("<|>"        #XE9CB)
            ("<<-"        #XE9CC)
            ("<|"         #XE9CD)
            ("<=<"        #XE9CE)
            ("<~"         #XE9CF)
            ("<~~"        #XE9D0)
            ("<<~"        #XE9D1)
            ("<$"         #XE9D2)
            ("<+"         #XE9D3)
            ("<!>"        #XE9D4)
            ("<@>"        #XE9D5)
            ("<#>"        #XE9D6)
            ("<%>"        #XE9D7)
            ("<^>"        #XE9D8)
            ("<&>"        #XE9D9)
            ("<?>"        #XE9DA)
            ("<.>"        #XE9DB)
            ("</>"        #XE9DC)
            ("<\\>"       #XE9DD)
            ("<\">"       #XE9DE)
            ("<:>"        #XE9DF)
            ("<~>"        #XE9E0)
            ("<**>"       #XE9E1)
            ("<<^"        #XE9E2)
            ("<!"         #XE9E3)
            ("<@"         #XE9E4)
            ("<#"         #XE9E5)
            ("<%"         #XE9E6)
            ("<^"         #XE9E7)
            ("<&"         #XE9E8)
            ("<?"         #XE9E9)
            ("<."         #XE9EA)
            ("</"         #XE9EB)
            ("<\\"        #XE9EC)
            ("<\""        #XE9ED)
            ("<:"         #XE9EE)
            ("<->"        #XE9EF)
            ("<!--"       #XE9F0)
            ("<--"        #XE9F1)
            ("<~<"        #XE9F2)
            ("<==>"       #XE9F3)
            ("<|-"        #XE9F4)
            ("<<|"        #XE9F5)
            ("<-<"        #XE9F7)
            ("<-->"       #XE9F8)
            ("<<=="       #XE9F9)
            ("<=="        #XE9FA)
            ("==<"        #XEA00)
            ("=="         #XEA01)
            ("==="        #XEA02)
            ("==>"        #XEA03)
            ("=>"         #XEA04)
            ("=~"         #XEA05)
            ("=>>"        #XEA06)
            ("=/="        #XEA07)
            ("=~="        #XEA08)
            ("==>>"       #XEA09)
            ("≡≡"         #XEA10)
            ("≡≡≡"        #XEA11)
            ("≡:≡"        #XEA12)
            (">-"         #XEA20)
            (">="         #XEA21)
            (">>"         #XEA22)
            (">>-"        #XEA23)
            (">=="        #XEA24)
            (">>>"        #XEA25)
            (">=>"        #XEA26)
            (">>^"        #XEA27)
            (">>|"        #XEA28)
            (">!="        #XEA29)
            (">->"        #XEA2A)
            ("??"         #XEA40)
            ("?~"         #XEA41)
            ("?="         #XEA42)
            ("?>"         #XEA43)
            ("???"        #XEA44)
            ("?."         #XEA45)
            ("^="         #XEA48)
            ("^."         #XEA49)
            ("^?"         #XEA4A)
            ("^.."        #XEA4B)
            ("^<<"        #XEA4C)
            ("^>>"        #XEA4D)
            ("^>"         #XEA4E)
            ("\\\\"       #XEA50)
            ("\\>"        #XEA51)
            ("\\/-"       #XEA52)
            ("@>"         #XEA57)
            ("|="         #XEA60)
            ("||"         #XEA61)
            ("|>"         #XEA62)
            ("|||"        #XEA63)
            ("|+|"        #XEA64)
            ("|->"        #XEA65)
            ("|-->"       #XEA66)
            ("|=>"        #XEA67)
            ("|==>"       #XEA68)
            ("|>-"        #XEA69)
            ("|<<"        #XEA6A)
            ("||>"        #XEA6B)
            ("|>>"        #XEA6C)
            ("|-"         #XEA6D)
            ("||-"        #XEA6E)
            ("~="         #XEA70)
            ("~>"         #XEA71)
            ("~~>"        #XEA72)
            ("~>>"        #XEA73)
            ("[["         #XEA80)
            ("]]"         #XEA81)
            ("\">"        #XEA90)
            ("_|_"        #XEA97))))

(defun add-pragmatapro-prettify-symbols-alist ()
  (setq prettify-symbols-alist pragmatapro-prettify-symbols-alist))

;; enable prettified symbols on comments
(defun setup-compose-predicate ()
  (setq prettify-symbols-compose-predicate
        (defun my-prettify-symbols-default-compose-p (start end _match)
          "Same as `prettify-symbols-default-compose-p', except compose symbols in comments as well."
          (let* ((syntaxes-beg (if (memq (char-syntax (char-after start)) '(?w ?_))
                                   '(?w ?_) '(?. ?\\)))
                 (syntaxes-end (if (memq (char-syntax (char-before end)) '(?w ?_))
                                   '(?w ?_) '(?. ?\\))))
            (not (or (memq (char-syntax (or (char-before start) ?\s)) syntaxes-beg)
                     (memq (char-syntax (or (char-after end) ?\s)) syntaxes-end)
                     (nth 3 (syntax-ppss))))))))

;; main hook fn, just add to text-mode/prog-mode
(defun prettify-hook ()
  (add-pragmatapro-prettify-symbols-alist)
  (setup-compose-predicate))

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
This function is called at the very end of Spacemacs startup,
after layer configuration. Put your configuration code here,
except for variables that should be set before packages are loaded."

  ;; Remove tide checker
  (setq-default flycheck-disabled-checkers (append flycheck-disabled-checkers '(tsx-tide typescript-tide)))

  (defun my/buffer-name ()
    (interactive)
    (message (buffer-file-name)))

  (setq-default mode-line-format nil)

  ;; Setup windows look
  (setq evil-move-beyond-eol t
        window-divider-default-bottom-width 1
        window-divider-default-right-width 1
        window-divider-default-places t)

  (window-divider-mode-apply t)

  (add-hook 'text-mode-hook (lambda ()
                              (prettify-hook)
                              (prettify-symbols-mode)))
  (add-hook 'prog-mode-hook (lambda ()
                              (prettify-hook)
                              (prettify-symbols-mode)))

  ;; Setup org-mode
  (setq org-log-repeat nil
        org-todo-keywords (quote ((sequence "TODO ☛(t)" "DOING 💁(n)" "|" "DONE ✓(d)")
                                  (sequence "WAITING ⚑(w)" "HOLD ⏱(h)" "|" "CANCELLED ✗(c)")))
        org-todo-keyword-faces (quote (("TODO ☛" :foreground "khaki1" :weight bold)
                                       ("DOING 💁" :foreground "CadetBlue1" :weight bold)
                                       ("DONE ✓" :foreground "chartreuse" :weight bold)
                                       ("WAITING ⚑" :foreground "wheat1" :weight bold)
                                       ("HOLD ⏱" :foreground "magenta" :weight bold)
                                       ("CANCELLED ✗" :foreground "lavender" :weight bold)
                                       ("MEETING 👯" :foreground "deep sky blue" :weight bold)
                                       ("PHONE ☎" :foreground "coral" :weight bold)))
        org-publish-project-alist '(("html"
                                     :base-directory "~/Nextcloud/Todos"
                                     :base-extension "org"
                                     :publishing-directory "~/Nextcloud/Todos/exports"
                                     :publishing-function org-html-publish-to-html)
                                    ("all" :components ("html")))
        org-super-agenda-groups '((:order-multi (0 (:name "Important (Dom)"
                                                          :and (:priority "A" :tag "Dom"))
                                                   (:name "Should do (Dom)"
                                                          :tag "Dom"
                                                          :priority<= "B")))
                                  (:order-multi (1
                                                 (:name "Important (Praca)"
                                                        :and (:priority "A" :tag "Praca"))
                                                 (:name "Should do (Praca)"
                                                        :tag "Praca"
                                                        :priority<= "B")))

                                  (:name "Health"
                                         :tag "Zdrowie"
                                         :order 2)

                                  (:name "Today"
                                         ;; :time-grid t
                                         :todo "TODAY"
                                         :order 3)
                                  (:name "Other"
                                         :time-grid nil
                                         :order 4))
        org-agenda-files '("~/Nextcloud/Todos/personal.org"
                           "~/Nextcloud/Todos/inbox.org"
                           "~/Nextcloud/Todos/retailic.org")
        org-bullets-bullet-list '("◉" "◎" "●" "○" "►" "◇")
        org-priority-faces '((?A . (:foreground "tomato1" :weight 'bold))
                             (?B . (:foreground "yellow"))
                             (?C . (:foreground "green"))
                             (?D . (:foreground "medium spring green"))))

  (add-hook 'org-mode-hook (lambda ()
                             (prettify-symbols-mode -1)
                             (org-indent-mode)
                             (org-super-agenda-mode)
                             (org-fancy-priorities-mode)))

  ;; Tramp fixes
  (eval-after-load 'tramp '(setenv "SHELL" "/bin/bash"))
  (setq tramp-shell-prompt-pattern "^[^$>\n]*[#$%>] *\\(\[[0-9;]*[a-zA-Z] *\\)*")

  ;; Setup company-mode
  (setq company-show-numbers t)
  (setq company-tooltip-align-annotations t)

  ;; Enable flycheck on save
  (setq flycheck-check-syntax-automatically '(save mode-enabled))

  ;; Disable global highlight-line
  (global-hl-line-mode -1)

  ;; (global-set-key (kbd "C-c C-c G") 'markdown-preview-like-god)

  ;; Config for python mode
  (add-hook 'python-mode-hook (lambda ()
                                (require 'sphinx-doc)
                                (sphinx-doc-mode t)
                                (anaconda-eldoc-mode 1)
                                (setq flycheck-checker 'python-pylint
                                      flycheck-checker-error-threshold 900
                                      flycheck-pylintrc ".pylintrc")))

  ;; Dired in macos
  (setq dired-use-ls-dired nil)

  ;; Speed up the projectile
  (setq projectile-mode-line '(:eval (format " Projectile[%s]" (projectile-project-name))))

  ;; Enable projectile caching
  (setq projectile-enable-caching t)

  ;; Set yasnippet company keybinding
  (global-set-key (kbd "C-M-y") 'company-yasnippet)

  ;; Support for dotenv files
  (add-to-list 'auto-mode-alist '("\\.env\\..*\\'" . fundamental-mode))

  ;; Remove the alt as a extra modifier
  (setq ns-right-alternate-modifier nil)

  ;; Setup for plantuml-mode for UML files
  (add-to-list 'auto-mode-alist '("\\.wsd\\'" . plantuml-mode))
  (add-hook 'plantuml-mode-hook (lambda () (plantuml-set-output-type "png")))

  ;; Support for c-c++
  (setq clang-format-executable "/usr/bin/clang-format-6.0")
  (add-hook 'semantic-init-hooks (lambda () (semantic-add-system-include "/usr/include/" 'c++-mode)))

  ;; Setup the zen-mode and binds it to correct key
  ;; Should not be used with buffers which use mmm-mode like .vue
  ;; or edit-indirect
  (setq writeroom-width 90)
  (setq dired-listing-switches "-alh")

  ;; Add hook which enables to collapse elements
  (add-hook 'prog-mode-hook
            (lambda ()
              (setq sp-pair-list (add-to-list 'sp-pair-list '("\\`" . "\\`")))
              (company-mode 1)
              (editorconfig-mode 1)
              (smartparens-mode 1)
              (show-smartparens-mode -1)
              (hs-minor-mode 1)))

  ;; Configure emacs-lisp
  (remove-hook 'emacs-lisp-mode-hook 'auto-compile-mode)

  ;; Enable auto complete everywhere
  (setq company-dabbrev-code-everywhere t)

  ;; Setup google translate
  (setq google-translate-default-target-language "pl")

  ;; Add hook for text-mode
  (add-hook 'text-mode-hook 'smartparens-mode)

  ;; Add config for scss files
  (add-to-list 'auto-mode-alist '("\\.scss\\'" . scss-mode))
  (add-to-list 'auto-mode-alist '("\\.edn\\'" . clojure-mode))
  (add-to-list 'auto-mode-alist '("\\.joker\\'" . clojure-mode))

  ;; Adds some basic indirect keybindings
  (global-set-key (kbd "C-c o") '(lambda ()
                                   (interactive)
                                   (dired "~/Nextcloud/Todos")))

  ;; Add auto-complete to C-<tab>
  (global-set-key (kbd "C-<tab>") 'company-complete)

  ;; Show todos in project
  (spacemacs/set-leader-keys (kbd "s C-t") 'show-todos-without-front-whitespaces)

  ;; Setup the key to search with two characters
  (spacemacs/set-leader-keys (kbd "j W") 'avy-goto-char-2)
  (spacemacs/set-leader-keys (kbd "p X") 'projectile-remove-known-project)
  (spacemacs/set-leader-keys (kbd "b C-r") 'revert-buffer)
  (spacemacs/set-leader-keys (kbd "s S") (defun swiper-symbol ()
                                            (interactive)
                                            (swiper (format "\\<%s\\>" (thing-at-point 'symbol)))))

  ;; Disable some wrong defaults and add some global modes
  (setq evil-move-cursor-back nil)
  (fset 'evil-visual-update-x-selection 'ignore)

  (setq org-capture-templates
        '(("h" "Home Todo" entry (file+headline "~/Nextcloud/Todos/personal.org" "Różne")
           "

* TODO ☛ [#C] %?                                       :Dom:

  DEADLINE: NONE
  [%U]

*** Description
Add some great description here

*** Action [%]
1) [ ] Step1
2) [ ] Step2

*** Outcome
Some great outcome there

  ")
          ("i" "Inbox" entry (file+headline "~/Nextcloud/Todos/inbox.org" "Inbox")
           "

* TODO ☛ %?                                            :Inbox:
  [%U]
  ")


          ("w" "Work Todo" entry (file+headline "~/Nextcloud/Todos/retailic.org" "Różne")
           "

* TODO ☛ [#C] %?                                        :Praca:
  DEADLINE: %t
  [%U]

*** Description
Add some great description here

*** Action [%]
1) [ ] Step1
2) [ ] Step2

*** Outcome
Some great outcome there

*** Time summary
# Time summary is autogenerated from logbook
#+BEGIN: clocktable :scope tree3 :maxlevel 3 :block today
#+END:

# Loogbook is autogenerated by the time command
:LOGBOOK:
:END:
=Captured At=
%a")
          ("j" "Journal" entry (file+datetree "~/Nextcloud/Todos/journal.org")
           "* %?\nEntered on %U\n  %i\n  %a")))

  ;; (set-face-attribute 'italic nil :family "Ubuntu Mono" :height 130)
  (org-agenda-list 1))

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
 '(auth-sources (quote ("~/.authinfo.gpg")))
 '(epg-gpg-home-directory "~/.gnupg")
 '(evil-want-Y-yank-to-eol nil)
 '(org-directory "~/Nextcloud/Todos/")
 '(package-selected-packages
   (quote
    (org-fancy-priorities add-node-modules-path ansi package-build shut-up epl commander f dash s flycheck-joker wakatime-mode company-box flycheck-gometalinter yasnippet-snippets yapfify yaml-mode xterm-color ws-butler writeroom-mode winum which-key wgrep web-mode web-beautify vue-mode volatile-highlights vi-tilde-fringe uuidgen use-package unfill treemacs-projectile treemacs-evil toml-mode toc-org tide tagedit symon string-inflection sql-indent spotify sphinx-doc spaceline-all-the-icons smex smeargle slim-mode shell-pop seeing-is-believing scss-mode sayid sass-mode rvm ruby-tools ruby-test-mode ruby-refactor ruby-hash-syntax rubocop rspec-mode robe rjsx-mode rich-minority restart-emacs rbenv rainbow-mode rainbow-identifiers rainbow-delimiters racer pyvenv pytest pylint pyenv-mode py-isort pug-mode projectile-rails prettier-js popwin plantuml-mode pippel pipenv pip-requirements persp-mode password-generator parinfer paradox ox-gfm overseer orgit org-projectile org-present org-pomodoro org-mime org-download org-bullets open-junk-file nginx-mode nameless mwim multi-term move-text minitest magithub magit-svn magit-gitflow macrostep lsp-ui lorem-ipsum livid-mode live-py-mode link-hint json-navigator js-doc ivy-yasnippet ivy-xref ivy-rich ivy-purpose ivy-hydra indium indent-guide importmagic impatient-mode hungry-delete hlint-refactor hl-todo hindent highlight-parentheses highlight-numbers highlight-indentation highlight-indent-guides helm-make haskell-snippets google-translate golden-ratio godoctor go-tag go-rename go-impl go-guru go-gen-test go-fill-struct go-eldoc gnuplot gitignore-templates gitignore-mode github-search github-clone gitconfig-mode gitattributes-mode git-timemachine git-messenger git-link git-gutter-fringe git-gutter-fringe+ git gist fuzzy forge font-lock+ focus flycheck-rust flycheck-pos-tip flycheck-haskell flycheck-clojure flx-ido fill-column-indicator feature-mode fancy-battery eyebrowse expand-region exec-path-from-shell evil-visualstar evil-visual-mark-mode evil-unimpaired evil-tutor evil-surround evil-org evil-numbers evil-mc evil-matchit evil-magit evil-lisp-state evil-lion evil-indent-plus evil-iedit-state evil-goggles evil-exchange evil-escape evil-ediff evil-commentary evil-cleverparens evil-args evil-anzu eshell-z eshell-prompt-extras esh-help enh-ruby-mode emojify emoji-cheat-sheet-plus emmet-mode elisp-slime-nav elisp-format editorconfig dumb-jump dotenv-mode doom-themes doom-modeline dockerfile-mode docker diminish diff-hl define-word cython-mode csv-mode counsel-spotify counsel-projectile counsel-css company-web company-terraform company-statistics company-shell company-quickhelp company-lsp company-go company-ghci company-emoji company-cabal company-anaconda column-enforce-mode color-identifiers-mode cmm-mode clojure-snippets clj-refactor clean-aindent-mode cider-eval-sexp-fu chruby centered-cursor-mode cargo bundler browse-at-remote auto-yasnippet auto-highlight-symbol auto-compile all-the-icons-ivy all-the-icons-gnus aggressive-indent adoc-mode ace-link ac-ispell 4clojure)))
 '(paradox-automatically-star t)
 '(safe-local-variable-values
   (quote
    ((cider-boot-parameters . "develop")
     (typescript-backend . tide)
     (typescript-backend . lsp)
     (javascript-backend . tern)
     (javascript-backend . lsp)
     (go-backend . go-mode)
     (go-backend . lsp)))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(window-divider ((t (:inherit vertical-border :foreground "plum")))))
)
