(require 'gpkg)

(gpkg-config
 ("use-package" "https://github.com/jwiegley/use-package"
  "39a8b8812c2c9f6f0b299e6a04e504ef393694ce") ; 2.4
 ("evil" "https://github.com/emacs-evil/evil"
  "3766a521a60e6fb0073220199425de478de759ad"  ; 1.2.14
  :removal '("evil-tests.el" "^ert.el"))
 ("evil-smartparens" "https://github.com/expez/evil-smartparens"
  "9fe4eed1c6327197afe6c13bb0771e18908aff00"  ; 0.4.0
  :removal '("^tests$"))
 ("sanityinc" "https://github.com/purcell/color-theme-sanityinc-tomorrow"
  "81d8990085960824f700520d08027e6aca58feaa") ; 1.17
 ("simple-httpd" "https://github.com/skeeto/emacs-web-server"
  "b191b07c942e44c946a22a826c4d9c9a0475fd7e") ; 1.5.1
 ("htmlize" "https://github.com/hniksic/emacs-htmlize"
  "88e2cb6588827893d7bc619529393887c264d15a") ; release/1.53
 ("impatient-mode" "https://github.com/skeeto/impatient-mode"
  "eba1efce3dd20b5f5017ab64bae0cfb3b181c2b0") ; 1.1
 ("dash" "https://github.com/magnars/dash.el"
  "a74f4cfcdc8d0642a9f602ad494f0354f27dacc9"  ; 2.14.1
  :removal '("^dev$"))
 ("smartparens" "https://github.com/Fuco1/smartparens"
  "4873352b5d0a1c5142658122de1b6950b8fe7e4d") ; 1.11.0
 ("markdown-mode" "https://github.com/jrblevin/markdown-mode"
  "cde5c5d2bcce470c494b76e23cfe1364b6291c20") ; v2.3
 ("js2-mode" "https://github.com/mooz/js2-mode"
  "ed955e7f1608cfd2d2713129d65f5fd734842ae4") ; 20190219
 ("skewer-mode" "https://github.com/skeeto/skewer-mode"
  "a10955db9ef95b0243ee31bcd30a6fb07ce5302b") ; 1.8.0
 ("nasm-mode" "https://github.com/skeeto/nasm-mode"
  "d990ed94d902b74a5c834fb567e03307607cee45") ; 1.1.1
 ("x86-lookup" "https://github.com/skeeto/x86-lookup"
  "609b2ba70dc5a246ac9b4b5f89eb5ef4331519bf") ; 1.2.0
 ("rainbow-delimiters" "https://github.com/Fanael/rainbow-delimiters"
  "93cd2dc873e7fedca7abc599cd97d46db4376ac7") ; 2.1.3
 ("slime" "https://github.com/slime/slime"
  "cf30941e5858e93eb91574ad91499075222a447b") ; 2.27
 ("org" "https://git.savannah.gnu.org/git/emacs/org-mode.git"
  "767a4ad31f0ced1839f10b6943ddda1713b44a27") ; 9.5.3
("projectile" "https://github.com/bbatsov/projectile.git"
 "a2a1aba8aa12d0ff0e044c4336a5c2598c259720") ; 2.5.0
("ivy" "https://github.com/abo-abo/swiper.git"
 "cd634c6f51458f81898ecf2821ac3169cb65a1eb")) ; 0.13.0

;; Set up some extra load-path directories
(add-to-list 'load-path (gpkg-path "evil" "lib"))
