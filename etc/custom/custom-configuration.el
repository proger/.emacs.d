(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(c-default-style
   (quote
    ((c++-mode . "llvm.org")
     (java-mode . "java")
     (awk-mode . "awk")
     (other . "linux"))))
 '(change-major-mode-with-file-name t)
 '(comint-scroll-to-bottom-on-input t)
 '(compilation-message-face (quote default))
 '(custom-safe-themes
   (quote
    ("b7b2cd8c45e18e28a14145573e84320795f5385895132a646ff779a141bbda7e" "0ec59d997a305e938d9ec8f63263a8fc12e17990aafc36ff3aff9bc5c5a202f0" "8453c6ba2504874309bdfcda0a69236814cefb860a528eb978b5489422cb1791" "9ff70d8009ce8da6fa204e803022f8160c700503b6029a8d8880a7a78c5ff2e5" "196cc00960232cfc7e74f4e95a94a5977cb16fd28ba7282195338f68c84058ec" "b571f92c9bfaf4a28cb64ae4b4cdbda95241cd62cf07d942be44dc8f46c491f4" "3b24f986084001ae46aa29ca791d2bc7f005c5c939646d2b800143526ab4d323" "04dd0236a367865e591927a3810f178e8d33c372ad5bfef48b5ce90d4b476481" "c86f868347919095aa44d2a6129dd714cbcf8feaa88ba954f636295b14ceff8f" "7ceb8967b229c1ba102378d3e2c5fef20ec96a41f615b454e0dc0bfa1d326ea6" "71f976f739af2d418242ea668dcb5b520a7043edd22ccf598e3a4fdf415f80f7" "7feeed063855b06836e0262f77f5c6d3f415159a98a9676d549bfeb6c49637c4" "77bd459212c0176bdf63c1904c4ba20fce015f730f0343776a1a14432de80990" "c1fb68aa00235766461c7e31ecfc759aa2dd905899ae6d95097061faeb72f9ee" default)))
 '(default-input-method "ukrainian-computer")
 '(diary-entry-marker (quote font-lock-variable-name-face))
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
 '(explicit-shell-file-name "/bin/bash")
 '(fci-rule-color "#444444")
 '(flycheck-clang-language-standard "c++14")
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
 '(haskell-indent-spaces 2)
 '(haskell-indentation-show-indentations (quote nil))
 '(haskell-interactive-mode-eval-mode (quote haskell-mode))
 '(haskell-interactive-mode-eval-pretty nil)
 '(haskell-interactive-mode-include-file-name nil)
 '(haskell-interactive-popup-errors (quote nil))
 '(haskell-notify-p t)
 '(haskell-process-args-ghci
   (quote
    ("NIX_PATH=/Users/vladki/src/eris/deps:ops=/Users/vladki/src/ops" "AWSFILE=/Users/vladki/priv/awspersonal.csv" "DYLD_INSERT_LIBRARIES=/System/Library/Frameworks/GLUT.framework/GLUT" "with-aws" "stack" "ghci" "--ghci-options" "-fno-ghci-sandbox")))
 '(haskell-process-auto-import-loaded-modules t)
 '(haskell-process-generate-tags nil)
 '(haskell-process-log t)
 '(haskell-process-path-ghci "env")
 '(haskell-process-reload-with-fbytecode nil)
 '(haskell-process-suggest-haskell-docs-imports t)
 '(haskell-process-suggest-remove-import-lines nil)
 '(haskell-process-type (quote ghci))
 '(haskell-process-use-presentation-mode t)
 '(haskell-stylish-on-save nil)
 '(haskell-tags-on-save nil)
 '(helm-descbinds-mode t)
 '(helm-locate-command "locate %s %s")
 '(helm-mode t)
 '(highlight-changes-colors (quote ("#FD5FF0" "#AE81FF")))
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
 '(hl-paren-background-colors (quote ("#2492db" "#95a5a6" nil)))
 '(hl-paren-colors (quote ("#ecf0f1" "#ecf0f1" "#c0392b")))
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
 '(multishell-command-key [C-M-return])
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
 '(shm-auto-insert-bangs t)
 '(shm-auto-insert-skeletons t)
 '(shm-use-presentation-mode t)
 '(sml/active-background-color "#34495e")
 '(sml/active-foreground-color "#ecf0f1")
 '(sml/inactive-background-color "#dfe4ea")
 '(sml/inactive-foreground-color "#34495e")
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
   (unspecified "#272822" "#49483E" "#A20C41" "#F92672" "#67930F" "#A6E22E" "#968B26" "#E6DB74" "#21889B" "#66D9EF" "#A41F99" "#FD5FF0" "#349B8D" "#A1EFE4" "#F8F8F2" "#F8F8F0")))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:background "#FFFFFE"))))
 '(helm-source-header ((t (:background "#2F69BF" :foreground "white" :weight bold :height 1 :family "Menlo"))))
 '(shm-current-face ((t (:background "#30344A"))))
 '(shm-quarantine-face ((t (:inherit font-lock-error)))))
