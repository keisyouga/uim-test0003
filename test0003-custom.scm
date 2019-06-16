(require "i18n.scm")
(require "generic-key-custom.scm")

;; TODO:
;; I can not figure out how to set rule-list with uim-pref-*.
;; so comment out define-custom-group to disable uim-pref-*.
;; please set rule-list with ~/.uim.
;; (define-custom-group 'test0003
;;   (N_ "test0003")
;;   (N_ "test0003 input method"))

;;; config

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; rule setting
;; e.g. (define ruleN-setting
;;       '((label . LABEL) (autocommit . #t) (prediction . #t) (wildcard . #f)
;;         (rulefile . "PATH-TO-RULE.DATA.SCM")
;;        ))
;; lable, rulefile are required
;; autocommit, prediction, wildcard are optional
;; after define rule, list in test0003-rule-list
(define test0003-rule1-setting
  (list
   '(label . hiragana)
   '(autocommit . #t)
   '(prediction . #t)
   '(wildcard . #f)
   (cons 'rulefile (test0003-find-module "hiragana-rule.data"))
   ))

(define test0003-rule2-setting
  (list
   '(label . katakana)
   '(autocommit . #t)
   '(prediction . #t)
   '(wildcard . #f)
   (cons 'rulefile (test0003-find-module "katakana-rule.data"))
   ))

(define test0003-rule3-setting
  (list
   '(label . cj)
   '(autocommit . #f)
   '(prediction . #f)
   '(wildcard . #t)
   (cons 'rulefile (test0003-find-module "cangjie-jis-rule.data"))
   ))

(define-custom 'test0003-rule-list ()
  '(test0003)
  '(key)
  (N_ "[test0003] rule-list")
  (N_ "long description will be here"))

(if (null? test0003-rule-list)
    (set! test0003-rule-list
          (list test0003-rule1-setting
                test0003-rule2-setting
                test0003-rule3-setting)))

(define-custom 'test0003-nr-candidates-max 10
  '(test0003)
  '(integer 1 20)
  (N_ "Candidate window size")
  (N_ "long description will be here."))

(define-custom 'test0003-use-candidate-window? #t
  '(test0003)
  '(boolean)
  (N_ "Use candidate window")
  (N_ "long description will be here."))

;;; keys

(define-custom-group 'test0003-keys
  (N_ "test0003 key bindings")
  (N_ "long description will be here."))

(define-custom 'test0003-on-key '(generic-on-key)
  '(test0003 test0003-keys)
  '(key)
  (N_ "[test0003] on")
  (N_ "long description will be here"))

(define-custom 'test0003-off-key '(generic-off-key)
  '(test0003 test0003-keys)
  '(key)
  (N_ "[test0003] off")
  (N_ "long description will be here"))

(define-custom 'test0003-commit-key '(" " generic-commit-key)
  '(test0003 test0003-keys)
  '(key)
  (N_ "[test0003] commit")
  (N_ "long description will be here"))

(define-custom 'test0003-cancel-key '(generic-cancel-key)
  '(test0003 test0003-keys)
  '(key)
  (N_ "[test0003] cancel")
  (N_ "long description will be here"))

(define-custom 'test0003-next-candidate-key '(generic-next-candidate-key)
  '(test0003 test0003-keys)
  '(key)
  (N_ "[test0003] next candidate")
  (N_ "long description will be here"))

(define-custom 'test0003-prev-candidate-key '(generic-prev-candidate-key)
  '(test0003 test0003-keys)
  '(key)
  (N_ "[test0003] previous candidate")
  (N_ "long description will be here"))

(define-custom 'test0003-next-page-key '(generic-next-page-key
                                         generic-go-right-key)
  '(test0003 test0003-keys)
  '(key)
  (N_ "[test0003] next page of candidate window")
  (N_ "long description will be here"))

(define-custom 'test0003-prev-page-key '(generic-prev-page-key
                                         generic-go-left-key)
  '(test0003 test0003-keys)
  '(key)
  (N_ "[test0003] previous page of candidate window")
  (N_ "long description will be here"))

(define-custom 'test0003-backspace-key '(generic-backspace-key)
  '(test0003 test0003-keys)
  '(key)
  (N_ "[test0003] backspace")
  (N_ "long description will be here"))

(define-custom 'test0003-toggle-candidate-window-key '("<Alt>c")
  '(test0003 test0003-keys)
  '(key)
  (N_ "[test0003] toggle candidate window")
  (N_ "long description will be here"))

(define-custom 'test0003-toggle-prediction-key '("<Alt>p")
  '(test0003 test0003-keys)
  '(key)
  (N_ "[test0003] toggle prediction")
  (N_ "long description will be here"))

(define-custom 'test0003-toggle-autocommit-key '("<Alt>a")
  '(test0003 test0003-keys)
  '(key)
  (N_ "[test0003] toggle autocommit")
  (N_ "long description will be here"))

(define-custom 'test0003-toggle-wildcard-key '("<Alt>w")
  '(test0003 test0003-keys)
  '(key)
  (N_ "[test0003] toggle autocommit")
  (N_ "long description will be here"))

(define-custom 'test0003-switch-rule1-key '("F6" "<Alt>1")
  '(test0003 test0003-keys)
  '(key)
  (N_ "[test0003] switch rule1")
  (N_ "long description will be here"))

(define-custom 'test0003-switch-rule2-key '("F7" "<Alt>2")
  '(test0003 test0003-keys)
  '(key)
  (N_ "[test0003] switch rule2")
  (N_ "long description will be here"))

(define-custom 'test0003-switch-rule3-key '("F8" "<Alt>3")
  '(test0003 test0003-keys)
  '(key)
  (N_ "[test0003] switch rule3")
  (N_ "long description will be here"))

(define-custom 'test0003-switch-rule4-key '("<Alt>4")
  '(test0003 test0003-keys)
  '(key)
  (N_ "[test0003] switch rule4")
  (N_ "long description will be here"))

(define-custom 'test0003-switch-rule5-key '("<Alt>5")
  '(test0003 test0003-keys)
  '(key)
  (N_ "[test0003] switch rule5")
  (N_ "long description will be here"))

(define-custom 'test0003-switch-rule6-key '("<Alt>6")
  '(test0003 test0003-keys)
  '(key)
  (N_ "[test0003] switch rule6")
  (N_ "long description will be here"))

(define-custom 'test0003-switch-rule7-key '("<Alt>7")
  '(test0003 test0003-keys)
  '(key)
  (N_ "[test0003] switch rule7")
  (N_ "long description will be here"))

(define-custom 'test0003-switch-rule8-key '("<Alt>8")
  '(test0003 test0003-keys)
  '(key)
  (N_ "[test0003] switch rule8")
  (N_ "long description will be here"))

(define-custom 'test0003-switch-rule9-key '("<Alt>9")
  '(test0003 test0003-keys)
  '(key)
  (N_ "[test0003] switch rule9")
  (N_ "long description will be here"))
