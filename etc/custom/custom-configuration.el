(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(TeX-source-correlate-method (quote synctex))
 '(TeX-source-correlate-mode t)
 '(TeX-source-correlate-start-server t)
 '(c-default-style
   (quote
    ((c++-mode . "llvm.org")
     (java-mode . "java")
     (awk-mode . "awk")
     (other . "linux"))))
 '(change-major-mode-with-file-name t)
 '(comint-input-autoexpand t)
 '(comint-input-ignoredups t)
 '(comint-prompt-read-only t)
 '(comint-scroll-show-maximum-output nil)
 '(comint-scroll-to-bottom-on-input nil)
 '(compilation-ask-about-save nil)
 '(compilation-message-face (quote default))
 '(compilation-scroll-output t)
 '(cua-global-mark-cursor-color "#2aa198")
 '(cua-normal-cursor-color "#657b83")
 '(cua-overwrite-cursor-color "#b58900")
 '(cua-read-only-cursor-color "#859900")
 '(custom-safe-themes
   (quote
    ("43c1a8090ed19ab3c0b1490ce412f78f157d69a29828aa977dae941b994b4147" "8aebf25556399b58091e533e455dd50a6a9cba958cc4ebb0aab175863c25b9a4" "a8245b7cc985a0610d71f9852e9f2767ad1b852c2bdea6f4aadc12cce9c4d6d0" "fa2b58bb98b62c3b8cf3b6f02f058ef7827a8e497125de0254f56e373abee088" "71ecffba18621354a1be303687f33b84788e13f40141580fa81e7840752d31bf" "40bc0ac47a9bd5b8db7304f8ef628d71e2798135935eb450483db0dbbfff8b11" "b7b2cd8c45e18e28a14145573e84320795f5385895132a646ff779a141bbda7e" "0ec59d997a305e938d9ec8f63263a8fc12e17990aafc36ff3aff9bc5c5a202f0" "8453c6ba2504874309bdfcda0a69236814cefb860a528eb978b5489422cb1791" "9ff70d8009ce8da6fa204e803022f8160c700503b6029a8d8880a7a78c5ff2e5" "196cc00960232cfc7e74f4e95a94a5977cb16fd28ba7282195338f68c84058ec" "b571f92c9bfaf4a28cb64ae4b4cdbda95241cd62cf07d942be44dc8f46c491f4" "3b24f986084001ae46aa29ca791d2bc7f005c5c939646d2b800143526ab4d323" "04dd0236a367865e591927a3810f178e8d33c372ad5bfef48b5ce90d4b476481" "c86f868347919095aa44d2a6129dd714cbcf8feaa88ba954f636295b14ceff8f" "7ceb8967b229c1ba102378d3e2c5fef20ec96a41f615b454e0dc0bfa1d326ea6" "71f976f739af2d418242ea668dcb5b520a7043edd22ccf598e3a4fdf415f80f7" "7feeed063855b06836e0262f77f5c6d3f415159a98a9676d549bfeb6c49637c4" "77bd459212c0176bdf63c1904c4ba20fce015f730f0343776a1a14432de80990" "c1fb68aa00235766461c7e31ecfc759aa2dd905899ae6d95097061faeb72f9ee" default)))
 '(default-input-method "ukrainian-computer")
 '(diary-entry-marker (quote font-lock-variable-name-face))
 '(elm-indent-offset 2)
 '(emms-mode-line-icon-image-cache
   (quote
    (image :type xpm :ascent center :data "/* XPM */
static char *note[] = {
/* width height num_colors chars_per_pixel */
\"    10   11        2            1\",
/* colors */
\". c #1ba1a1\",
\"# c None s None\",
/* pixels */
\"###...####\",
\"###.#...##\",
\"###.###...\",
\"###.#####.\",
\"###.#####.\",
\"#...#####.\",
\"....#####.\",
\"#..######.\",
\"#######...\",
\"######....\",
\"#######..#\" };")))
 '(exec-path-from-shell-check-startup-files nil)
 '(explicit-shell-file-name "/bin/bash")
 '(fci-rule-color "#444444")
 '(flycheck-clang-language-standard nil)
 '(fstar-enabled-modules
   (quote
    (font-lock prettify indentation comments flycheck interactive)))
 '(fstar-executable "fstar-wrapper")
 '(git-commit-fill-column 100)
 '(git-commit-summary-max-length 100)
 '(git-rebase-confirm-cancel nil)
 '(global-mark-ring-max 17)
 '(global-undo-tree-mode t)
 '(gnus-logo-colors (quote ("#4c8383" "#bababa")) t)
 '(gnus-mode-line-image-cache
   (quote
    (image :type xpm :ascent center :data "/* XPM */
static char *gnus-pointer[] = {
/* width height num_colors chars_per_pixel */
\"    18    13        2            1\",
/* colors */
\". c #1ba1a1\",
\"# c None s None\",
/* pixels */
\"##################\",
\"######..##..######\",
\"#####........#####\",
\"#.##.##..##...####\",
\"#...####.###...##.\",
\"#..###.######.....\",
\"#####.########...#\",
\"###########.######\",
\"####.###.#..######\",
\"######..###.######\",
\"###....####.######\",
\"###..######.######\",
\"###########.######\" };")) t)
 '(godef-command "godef")
 '(haskell-complete-module-preferred
   (quote
    ("Data.ByteString" "Data.ByteString.Lazy" "Data.Conduit" "Data.Function" "Data.List" "Data.Map" "Data.Maybe" "Data.Monoid" "Data.Ord")))
 '(haskell-doc-prettify-types (quote nil))
 '(haskell-hoogle-url "https://www.stackage.org/lts/hoogle?q=%s")
 '(haskell-indent-spaces 2)
 '(haskell-indentation-show-indentations (quote nil))
 '(haskell-interactive-mode-eval-mode (quote haskell-mode))
 '(haskell-interactive-mode-eval-pretty nil)
 '(haskell-interactive-mode-include-file-name nil)
 '(haskell-interactive-popup-errors (quote nil))
 '(haskell-notify-p t)
 '(haskell-process-args-ghci
   (quote
    ("DYLD_INSERT_LIBRARIES=/System/Library/Frameworks/GLUT.framework/GLUT" "stack" "ghci" "--ghci-options" "-fno-ghci-sandbox")))
 '(haskell-process-auto-import-loaded-modules t)
 '(haskell-process-generate-tags nil)
 '(haskell-process-log t)
 '(haskell-process-path-ghci "env")
 '(haskell-process-reload-with-fbytecode nil)
 '(haskell-process-suggest-haskell-docs-imports nil)
 '(haskell-process-suggest-remove-import-lines nil)
 '(haskell-process-type (quote ghci))
 '(haskell-process-use-presentation-mode t)
 '(haskell-stylish-on-save nil)
 '(haskell-tags-on-save nil)
 '(helm-descbinds-mode t)
 '(helm-locate-command "locate %s %s")
 '(helm-mode t)
 '(helm-swoop-use-line-number-face t)
 '(highlight-changes-colors (quote ("#FD5FF0" "#AE81FF")))
 '(highlight-symbol-colors
   (--map
    (solarized-color-blend it "#fdf6e3" 0.25)
    (quote
     ("#b58900" "#2aa198" "#dc322f" "#6c71c4" "#859900" "#cb4b16" "#268bd2"))))
 '(highlight-symbol-foreground-color "#586e75")
 '(highlight-tail-colors
   (quote
    (("#49483E" . 0)
     ("#67930F" . 20)
     ("#349B8D" . 30)
     ("#21889B" . 50)
     ("#968B26" . 60)
     ("#A45E0A" . 70)
     ("#A41F99" . 85)
     ("#49483E" . 100))))
 '(hindent-style "gibiansky")
 '(hl-bg-colors
   (quote
    ("#DEB542" "#F2804F" "#FF6E64" "#F771AC" "#9EA0E5" "#69B7F0" "#69CABF" "#B4C342")))
 '(hl-fg-colors
   (quote
    ("#fdf6e3" "#fdf6e3" "#fdf6e3" "#fdf6e3" "#fdf6e3" "#fdf6e3" "#fdf6e3" "#fdf6e3")))
 '(hl-paren-background-colors (quote ("#2492db" "#95a5a6" nil)))
 '(hl-paren-colors (quote ("#ecf0f1" "#ecf0f1" "#c0392b")))
 '(ibuffer-expert t)
 '(imagex-auto-adjust-mode t)
 '(imagex-global-sticky-mode t)
 '(line-spacing 0.02)
 '(magit-commit-arguments (quote ("-v")))
 '(magit-commit-ask-to-stage nil)
 '(magit-commit-show-diff t)
 '(magit-commit-squash-confirm nil)
 '(magit-diff-use-overlays nil)
 '(magit-no-confirm
   (quote
    (reverse discard rename resurrect trash delete abort-merge merge-dirty drop-stashes resect-bisect kill-process delete-unmerged-branch stage-all-changes unstage-all-changes safe-with-wip)))
 '(magit-push-always-verify nil)
 '(magit-revert-buffers (quote silent) t)
 '(magit-save-repository-buffers nil)
 '(markdown-gfm-use-electric-backquote nil)
 '(multishell-command-key [C-M-return])
 '(nrepl-message-colors
   (quote
    ("#dc322f" "#cb4b16" "#b58900" "#546E00" "#B4C342" "#00629D" "#2aa198" "#d33682" "#6c71c4")))
 '(package-selected-packages
   (quote
    (ac-cider typescript-mode flycheck-flow cql-mode emojify dash-functional helm projectile auctex julia-mode markdown-mode js2-mode company octave-mode highlight-sexp-mode elm-mode phi-search nameframe-projectile haskell-mode dumb-jump magit indy flycheck elpy yaml-mode ws-butler writeroom-mode window-number win-switch web-mode unipoint undo-tree tao-theme swift-mode spacemacs-theme solarized-theme sml-mode smartscan skewer-mode rainbow-delimiters puppet-mode popwin php-mode paredit org-trello org-tree-slide nyan-mode nodejs-repl nix-mode naquadah-theme monokai-theme molokai-theme markdown-toc leuven-theme latex-preview-pane js2-refactor js-comint ir-black-theme intero idris-mode idomenu highlight-sexp hemisu-theme helm-swoop helm-projectile helm-dired-recent-dirs helm-descbinds helm-cider helm-ag go-mode gnuplot-mode glsl-mode gist fstar-mode fsharp-mode flx-ido flatui-theme fill-column-indicator expand-region exec-path-from-shell ess-view erlang ein dockerfile-mode diminish dash-at-point cyberpunk-theme company-irony company-coq cmake-font-lock cider-spy cider-profile cider-decompile char-menu browse-at-remote auctex-latexmk ag ace-jump-mode)))
 '(pos-tip-background-color "#eee8d5")
 '(pos-tip-foreground-color "#586e75")
 '(preview-auto-reveal
   (quote
    (eval
     (preview-arrived-via
      (key-binding
       [left])
      (key-binding
       [right])
      (quote backward-char)
      (quote forward-char)
      (quote backward-word)
      (quote forward-word)))))
 '(prolog-system (quote swi))
 '(proof-prog-name "/Users/vladki/src/HoTT/hoqtop")
 '(proof-splash-enable nil)
 '(safe-local-eval-forms
   (quote
    ((add-hook
      (quote write-file-hooks)
      (quote time-stamp))
     (add-hook
      (quote write-file-functions)
      (quote time-stamp))
     (add-hook
      (quote before-save-hook)
      (quote time-stamp)
      nil t)
     (add-hook
      (quote before-save-hook)
      (quote delete-trailing-whitespace)
      nil t)
     (let
         ((default-directory
            (locate-dominating-file buffer-file-name ".dir-locals.el")))
       (make-local-variable
        (quote coq-prog-name))
       (setq coq-prog-name
             (expand-file-name "../hoqtop"))))))
 '(safe-local-variable-values
   (quote
    ((eval setq-local flycheck-erlang-library-path
           (list "/usr/local/Cellar/ejabberd/16.09/lib/cache_tab-1.0.4/ebin" "/usr/local/Cellar/ejabberd/16.09/lib/ejabberd-16.09/ebin" "/usr/local/Cellar/ejabberd/16.09/lib/esip-1.0.8/ebin" "/usr/local/Cellar/ejabberd/16.09/lib/ezlib-1.0.1/ebin" "/usr/local/Cellar/ejabberd/16.09/lib/fast_tls-1.0.7/ebin" "/usr/local/Cellar/ejabberd/16.09/lib/fast_xml-1.1.15/ebin" "/usr/local/Cellar/ejabberd/16.09/lib/fast_yaml-1.0.6/ebin" "/usr/local/Cellar/ejabberd/16.09/lib/goldrush-0.1.8/ebin" "/usr/local/Cellar/ejabberd/16.09/lib/iconv-1.0.2/ebin" "/usr/local/Cellar/ejabberd/16.09/lib/jiffy-0.14.7/ebin" "/usr/local/Cellar/ejabberd/16.09/lib/lager-3.2.1/ebin" "/usr/local/Cellar/ejabberd/16.09/lib/luerl-1/ebin" "/usr/local/Cellar/ejabberd/16.09/lib/p1_mysql-1.0.1/ebin" "/usr/local/Cellar/ejabberd/16.09/lib/p1_oauth2-0.6.1/ebin" "/usr/local/Cellar/ejabberd/16.09/lib/p1_pam-1.0.0/ebin" "/usr/local/Cellar/ejabberd/16.09/lib/p1_pgsql-1.0.1/ebin" "/usr/local/Cellar/ejabberd/16.09/lib/p1_utils-1.0.5/ebin" "/usr/local/Cellar/ejabberd/16.09/lib/stringprep-1.0.6/ebin" "/usr/local/Cellar/ejabberd/16.09/lib/stun-1.0.7/ebin"))
     (eval setq-local flycheck-erlang-include-path
           (list "../../../../src/ejabberd/include/"))
     (flycheck-erlang-library-path list "/usr/local/Cellar/ejabberd/16.09/lib/cache_tab-1.0.4/ebin" "/usr/local/Cellar/ejabberd/16.09/lib/ejabberd-16.09/ebin" "/usr/local/Cellar/ejabberd/16.09/lib/esip-1.0.8/ebin" "/usr/local/Cellar/ejabberd/16.09/lib/ezlib-1.0.1/ebin" "/usr/local/Cellar/ejabberd/16.09/lib/fast_tls-1.0.7/ebin" "/usr/local/Cellar/ejabberd/16.09/lib/fast_xml-1.1.15/ebin" "/usr/local/Cellar/ejabberd/16.09/lib/fast_yaml-1.0.6/ebin" "/usr/local/Cellar/ejabberd/16.09/lib/goldrush-0.1.8/ebin" "/usr/local/Cellar/ejabberd/16.09/lib/iconv-1.0.2/ebin" "/usr/local/Cellar/ejabberd/16.09/lib/jiffy-0.14.7/ebin" "/usr/local/Cellar/ejabberd/16.09/lib/lager-3.2.1/ebin" "/usr/local/Cellar/ejabberd/16.09/lib/luerl-1/ebin" "/usr/local/Cellar/ejabberd/16.09/lib/p1_mysql-1.0.1/ebin" "/usr/local/Cellar/ejabberd/16.09/lib/p1_oauth2-0.6.1/ebin" "/usr/local/Cellar/ejabberd/16.09/lib/p1_pam-1.0.0/ebin" "/usr/local/Cellar/ejabberd/16.09/lib/p1_pgsql-1.0.1/ebin" "/usr/local/Cellar/ejabberd/16.09/lib/p1_utils-1.0.5/ebin" "/usr/local/Cellar/ejabberd/16.09/lib/stringprep-1.0.6/ebin" "/usr/local/Cellar/ejabberd/16.09/lib/stun-1.0.7/ebin")
     (flycheck-erlang-include-path list "../../../../src/ejabberd/include/")
     (encoding . utf-8)
     (TeX-command-extra-options . "-shell-escape")
     (scheme-program-name . "scsh")
     (eval progn
           (pyvenv-workon "env")))))
 '(shm-auto-insert-bangs t)
 '(shm-auto-insert-skeletons t)
 '(shm-use-presentation-mode t)
 '(smartrep-mode-line-active-bg (solarized-color-blend "#859900" "#657b83" 0.2))
 '(sml/active-background-color "#34495e")
 '(sml/active-foreground-color "#ecf0f1")
 '(sml/inactive-background-color "#dfe4ea")
 '(sml/inactive-foreground-color "#34495e")
 '(solarized-high-contrast-mode-line t)
 '(tab-width 4)
 '(term-default-bg-color "#fdf6e3")
 '(term-default-fg-color "#657b83")
 '(typescript-indent-level 2)
 '(vc-annotate-background "#ecf0f1")
 '(vc-annotate-color-map
   (quote
    ((30 . "#e74c3c")
     (60 . "#c0392b")
     (90 . "#e67e22")
     (120 . "#d35400")
     (150 . "#f1c40f")
     (180 . "#d98c10")
     (210 . "#2ecc71")
     (240 . "#27ae60")
     (270 . "#1abc9c")
     (300 . "#16a085")
     (330 . "#2492db")
     (360 . "#0a74b9"))))
 '(vc-annotate-very-old-color "#0a74b9")
 '(weechat-color-list
   (unspecified "#272822" "#49483E" "#A20C41" "#F92672" "#67930F" "#A6E22E" "#968B26" "#E6DB74" "#21889B" "#66D9EF" "#A41F99" "#FD5FF0" "#349B8D" "#A1EFE4" "#F8F8F2" "#F8F8F0"))
 '(xterm-color-names
   ["#eee8d5" "#dc322f" "#859900" "#b58900" "#268bd2" "#d33682" "#2aa198" "#073642"])
 '(xterm-color-names-bright
   ["#fdf6e3" "#cb4b16" "#93a1a1" "#839496" "#657b83" "#6c71c4" "#586e75" "#002b36"]))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
