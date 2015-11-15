(setq
  live-root-dir     user-emacs-directory
  live-tmp-dir      (file-name-as-directory (concat live-root-dir "tmp"))
  live-etc-dir      (file-name-as-directory (concat live-root-dir "etc"))
  live-pscratch-dir (file-name-as-directory (concat live-tmp-dir  "pscratch"))
  live-lib-dir      (file-name-as-directory (concat live-root-dir "lib"))
  live-packs-dir    (file-name-as-directory (concat live-root-dir "packs"))
  live-autosaves-dir(file-name-as-directory (concat live-tmp-dir  "autosaves"))
  live-backups-dir  (file-name-as-directory (concat live-tmp-dir  "backups"))
  live-custom-dir   (file-name-as-directory (concat live-etc-dir  "custom"))
  live-load-pack-dir nil
  live-disable-zone t)

;; create tmp dirs if necessary
(make-directory live-etc-dir t)
(make-directory live-tmp-dir t)
(make-directory live-autosaves-dir t)
(make-directory live-backups-dir t)
(make-directory live-custom-dir t)
(make-directory live-pscratch-dir t)

(setq custom-file (concat live-custom-dir "custom-configuration.el"))
(when (file-exists-p custom-file)
  (load custom-file))
