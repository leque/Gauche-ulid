;;
;; Package Gauche-ulid
;;

(define-gauche-package "Gauche-ulid"
  ;;
  :version "0.1"

  ;; Description of the package.  The first line is used as a short
  ;; summary.
  :description "An ULID implementation for Gauche"

  ;; List of dependencies.
  ;; Example:
  ;;     :require (("Gauche" (>= "0.9.5"))  ; requires Gauche 0.9.5 or later
  ;;               ("Gauche-gl" "0.6"))     ; and Gauche-gl 0.6
  :require (("Gauche" (>= "0.9.7")))

  ;; List of providing modules
  ;; NB: This will be recognized >= Gauche 0.9.7.
  ;; Example:
  ;;      :providing-modules (util.algorithm1 util.algorithm1.option)
  ;:providing-modules ()

  ;; List name and contact info of authors.
  ;; e.g. ("Eva Lu Ator <eval@example.com>"
  ;;       "Alyssa P. Hacker <lisper@example.com>")
  :authors ("OOHASHI Daichi <dico.leque.comicron@gmail.com>")

  ;; List name and contact info of package maintainers, if they differ
  ;; from authors.
  ;; e.g. ("Cy D. Fect <c@example.com>")
  :maintainers ()

  ;; List licenses
  :licenses ("mit")

  ;; Homepage URL, if any.
  :homepage "http://github.com/leque/Gauche-ulid"

  ;; Repository URL, e.g. github
  :repository "http://github.com/leque/Gauche-ulid.git"
  )
