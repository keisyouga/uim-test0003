(require "i18n.scm")
(require "generic-key-custom.scm")

;; TODO:
;; I can not figure out how to set rule-list with uim-pref-*.
;; so comment out define-custom-group to disable uim-pref-*.
;; if want to customize, please edit ~/.uim.
;;
;; have problem in uim-module-manager if comment out this code, so uncomment it.
;; 2020-01-08
(define-custom-group 'switchtable
  (N_ "switchtable")
  (N_ "switchtable input method"))

;;; config

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; rule setting
;; e.g.
;; (define ruleN-setting
;;   '((label . LABEL) (autocommit . #t) (prediction . #t) (wildcard . #f)
;;     (tablefile . "PATH-TO-TABLEFILE")))
;; (define switchtable-rule-list
;;   (list switchtable-ruleN-setting ... ))
;; label, tablefile are required
;; autocommit, prediction, wildcard are optional
;; after define rule-setting, define switchtable-rule-list to list of rule-setting

(define switchtable-rule1-setting
  (list
   '(label . hiragana)
   '(autocommit . #t)
   '(prediction . #t)
   '(wildcard . #f)
   '(tablefile . "tables/hiragana.txt")
   ))

(define switchtable-rule2-setting
  (list
   '(label . katakana)
   '(autocommit . #t)
   '(prediction . #t)
   '(wildcard . #f)
   '(tablefile . "tables/katakana.txt")
   ))

(define switchtable-rule3-setting
  (list
   '(label . cj)
   '(autocommit . #f)
   '(prediction . #f)
   '(wildcard . #t)
   '(tablefile . "tables/cj-jis.txt")
   ))

;; define default rule-list setting unless already defined in ~/.uim
(define switchtable-rule-list
  (if (symbol-bound? 'switchtable-rule-list)
      switchtable-rule-list
      (list switchtable-rule1-setting
            switchtable-rule2-setting
            switchtable-rule3-setting))
  )

;;; test
;; (define-custom-group 'switchtable-rule1
;;   (N_ "switchtable rule1")
;;   (N_ "long description will be here."))

;; (define-custom 'switchtable-rule1-label "hiragana"
;;   '(switchtable-rule1)
;;   '(string ".*")
;;   (N_ "label")
;;   (N_ "long description will be here."))

;; (define-custom 'switchtable-rule1-autocommit #t
;;   '(switchtable-rule1)
;;   '(boolean)
;;   (N_ "autocommit")
;;   (N_ "long description will be here."))

;; (define-custom 'switchtable-rule1-prediction #t
;;   '(switchtable-rule1)
;;   '(boolean)
;;   (N_ "prediction")
;;   (N_ "long description will be here."))

;; (define-custom 'switchtable-rule1-wildcard #f
;;   '(switchtable-rule1)
;;   '(boolean)
;;   (N_ "wildcard")
;;   (N_ "long description will be here."))

;; (define-custom 'switchtable-rule1-tablefile "tables/hiragana.txt"
;;   '(switchtable-rule1)
;;   '(pathname regular-file)
;;   (N_ "tablefile")
;;   (N_ "long description will be here."))

;;; candwin

(define-custom 'switchtable-nr-candidates-max 10
  '(switchtable candwin)
  '(integer 1 20)
  (N_ "Candidate window size")
  (N_ "long description will be here."))

(define-custom 'switchtable-use-candidate-window? #t
  '(switchtable candwin)
  '(boolean)
  (N_ "Use candidate window")
  (N_ "long description will be here."))

(define-custom 'switchtable-commit-candidate-by-numeral-key? #t
  '(switchtable candwin)
  '(boolean)
  (N_ "Select candidate by numeral keys")
  (N_ "long description will be here."))

;;; keys

(define-custom-group 'switchtable-keys
  (N_ "switchtable key bindings")
  (N_ "long description will be here."))

(define-custom 'switchtable-on-key '(generic-on-key)
  '(switchtable switchtable-keys)
  '(key)
  (N_ "[switchtable] on")
  (N_ "long description will be here"))

(define-custom 'switchtable-off-key '(generic-off-key)
  '(switchtable switchtable-keys)
  '(key)
  (N_ "[switchtable] off")
  (N_ "long description will be here"))

(define-custom 'switchtable-commit-key '(" " generic-commit-key)
  '(switchtable switchtable-keys)
  '(key)
  (N_ "[switchtable] commit")
  (N_ "long description will be here"))

(define-custom 'switchtable-cancel-key '(generic-cancel-key)
  '(switchtable switchtable-keys)
  '(key)
  (N_ "[switchtable] cancel")
  (N_ "long description will be here"))

(define-custom 'switchtable-next-candidate-key '(generic-next-candidate-key)
  '(switchtable switchtable-keys)
  '(key)
  (N_ "[switchtable] next candidate")
  (N_ "long description will be here"))

(define-custom 'switchtable-prev-candidate-key '(generic-prev-candidate-key)
  '(switchtable switchtable-keys)
  '(key)
  (N_ "[switchtable] previous candidate")
  (N_ "long description will be here"))

(define-custom 'switchtable-next-page-key '(generic-next-page-key
                                         generic-go-right-key)
  '(switchtable switchtable-keys)
  '(key)
  (N_ "[switchtable] next page of candidate window")
  (N_ "long description will be here"))

(define-custom 'switchtable-prev-page-key '(generic-prev-page-key
                                         generic-go-left-key)
  '(switchtable switchtable-keys)
  '(key)
  (N_ "[switchtable] previous page of candidate window")
  (N_ "long description will be here"))

(define-custom 'switchtable-backspace-key '(generic-backspace-key)
  '(switchtable switchtable-keys)
  '(key)
  (N_ "[switchtable] backspace")
  (N_ "long description will be here"))

(define-custom 'switchtable-toggle-candidate-window-key '("<Alt>c")
  '(switchtable switchtable-keys)
  '(key)
  (N_ "[switchtable] toggle candidate window")
  (N_ "long description will be here"))

(define-custom 'switchtable-toggle-prediction-key '("<Alt>p")
  '(switchtable switchtable-keys)
  '(key)
  (N_ "[switchtable] toggle prediction")
  (N_ "long description will be here"))

(define-custom 'switchtable-toggle-autocommit-key '("<Alt>a")
  '(switchtable switchtable-keys)
  '(key)
  (N_ "[switchtable] toggle autocommit")
  (N_ "long description will be here"))

(define-custom 'switchtable-toggle-wildcard-key '("<Alt>w")
  '(switchtable switchtable-keys)
  '(key)
  (N_ "[switchtable] toggle autocommit")
  (N_ "long description will be here"))

(define-custom 'switchtable-switch-rule1-key '("F6" "<Alt>1")
  '(switchtable switchtable-keys)
  '(key)
  (N_ "[switchtable] switch rule1")
  (N_ "long description will be here"))

(define-custom 'switchtable-switch-rule2-key '("F7" "<Alt>2")
  '(switchtable switchtable-keys)
  '(key)
  (N_ "[switchtable] switch rule2")
  (N_ "long description will be here"))

(define-custom 'switchtable-switch-rule3-key '("F8" "<Alt>3")
  '(switchtable switchtable-keys)
  '(key)
  (N_ "[switchtable] switch rule3")
  (N_ "long description will be here"))

(define-custom 'switchtable-switch-rule4-key '("<Alt>4")
  '(switchtable switchtable-keys)
  '(key)
  (N_ "[switchtable] switch rule4")
  (N_ "long description will be here"))

(define-custom 'switchtable-switch-rule5-key '("<Alt>5")
  '(switchtable switchtable-keys)
  '(key)
  (N_ "[switchtable] switch rule5")
  (N_ "long description will be here"))

(define-custom 'switchtable-switch-rule6-key '("<Alt>6")
  '(switchtable switchtable-keys)
  '(key)
  (N_ "[switchtable] switch rule6")
  (N_ "long description will be here"))

(define-custom 'switchtable-switch-rule7-key '("<Alt>7")
  '(switchtable switchtable-keys)
  '(key)
  (N_ "[switchtable] switch rule7")
  (N_ "long description will be here"))

(define-custom 'switchtable-switch-rule8-key '("<Alt>8")
  '(switchtable switchtable-keys)
  '(key)
  (N_ "[switchtable] switch rule8")
  (N_ "long description will be here"))

(define-custom 'switchtable-switch-rule9-key '("<Alt>9")
  '(switchtable switchtable-keys)
  '(key)
  (N_ "[switchtable] switch rule9")
  (N_ "long description will be here"))
