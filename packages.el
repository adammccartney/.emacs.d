(require 'gpkg)

(gpkg-config
 ("use-package" "https://github.com/jwiegley/use-package"
  "caa92f1d64fc25480551757d854b4b49981dfa6b") ; 2.4.1
 ("annalist" "https://github.com/noctuid/annalist.el"
  "0da9812e419b1687cf1e7040384f983be32d5328") ; One Eye
 ("evil" "https://github.com/emacs-evil/evil"
  "162a94cbce4f2c09fa7dd6bd8ca56080cb0ab63b")  ; 1.14.2
 ("evil-magit" "https://github.com/emacs-evil/evil-magit.git"
  "a24186be7cc2cdab24b56f6dcc4665eeb8349c1a") ; v0.4.3
 ("evil-collection" "https://github.com/emacs-evil/evil-collection"
  "a63cb007bf15c3fd4c3322f29f85d18d5574f7ba") ; 0.0.8
 ("sanityinc" "https://github.com/purcell/color-theme-sanityinc-tomorrow"
  "81d8990085960824f700520d08027e6aca58feaa") ; 1.17
 ("markdown-mode" "https://github.com/jrblevin/markdown-mode"
  "cde5c5d2bcce470c494b76e23cfe1364b6291c20") ; v2.3
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
  "cd634c6f51458f81898ecf2821ac3169cb65a1eb") ; 0.13.0
 ("counsel-projectile" "https://github.com/ericdanan/counsel-projectile.git"
  "e30150792a96968f55f34638cbfe63eaa30839cc")) ; 0.3.2
 ;;("dash" "https://github.com/magnars/dash.el.git"
 ;; "39d067b9fbb2db65fc7a6938bfb21489ad990cb4") ; 2.19.1
 ;;("transient" "https://github.com/magit/transient.git"
 ;; "74cba5a418ff1b1661494fc2970c330ecdbb4b22") ; 0.3.7
 ;;("with-editor" "https://github.com/magit/with-editor.git"
 ;; "5519b6a67ecd66865b4fdd5447425eee900c54f4") ; 3.0.4
 ;;("magit" "https://github.com/magit/magit.git"
 ;; "f44f6c14500476d918e9c01de8449edb20af4113")) ; 3.3.0

;; Set up some extra load-path directories
(add-to-list 'load-path (gpkg-path "evil" "lib"))
