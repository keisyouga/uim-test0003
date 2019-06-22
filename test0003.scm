;;; debug message use format
;; (require-extension (srfi 48))                ; format

;;; return scm-file-name in full path
;;; do not add ".scm" to scm
(define test0003-find-module
  (lambda (scm)
    (find-module-scm-path uim-plugin-scm-load-path scm)))

(define alist-get
  (lambda (alist key)
    (cdr (or (assq key alist) '(#f . #f)))))

(define alist-set!
  (lambda (alist key value)
    (let ((target (assq key alist)))
      (if target
          (set-cdr! target value)
          ;; if no key, add new cell (key . value) to alist
          (begin
            (set-cdr! alist (cons (car alist) (cdr alist)))
            (set-car! alist (cons key value)))))))

;; ;; return position of first found of elt in lst. specify 0 for idx.
;; (define list-index
;;   (lambda (lst elt . index)
;;     (let ((idx (if (pair? index) (car index) 0)))
;;       (cond ((not (pair? lst)) #f)
;;             ((eq? (car lst) elt) idx)
;;             (else (list-index (cdr lst) elt (+ idx 1)))))))

;; `require' do not search ~/.uim.d/plugin/ directory
;; (require-custom "test0003-custom.scm")
(require-custom (test0003-find-module "test0003-custom"))

;;; user configs

;; widgets and actions

;; widgets
(define test0003-widgets '(widget_test0003_input_mode))

;; default activity for each widgets
(define default-widget_test0003_input_mode 'action_test0003_off)

;; actions of widget_test0003_input_mode
;; (define test0003-input-mode-actions
;;   '(action_test0003_off
;;    action_test0003_on
;;     ))
;; action for each rule
;; coordinate with register-action
(define test0003-input-mode-actions
  (cons 'action_test0003_off
        (map
         (lambda (x)
           (string->symbol
            (string-append
             "action_test0003_" (symbol->string (alist-get x 'label)))))
         test0003-rule-list)))

;;; implementations

(define test0003-prepare-activation
  (lambda (tc)
    (test0003-clear tc)
    (test0003-update-preedit tc)))

(define test0003-load-rule-file
  (lambda (tc rule-setting)
    (let* ((rule (call-with-input-file
                     (alist-get rule-setting 'rulefile) read)))
      (test0003-context-set-rule! tc rule)
      )))

(define test0003-switch-rule-setting
  (lambda (tc rule-setting)
    (test0003-load-rule-file tc rule-setting)
    (test0003-context-set-rule-setting! tc rule-setting)
    (test0003-update-candidate tc)))

(register-action 'action_test0003_off
                 (lambda (tc)
                   (list
                    'off
                    "-"
                    (N_ "off")
                    (N_ "Direct Input Mode")))
                 (lambda (tc)
                   (not (test0003-context-on tc)))
                 (lambda (tc)
                   (test0003-prepare-activation tc)
                   (test0003-context-set-on! tc #f)))

;; (register-action 'action_test0003_on
;;                  (lambda (tc)
;;                    (list
;;                     'on
;;                     "O"
;;                     (N_ "on")
;;                     (string-append "on" (N_" Mode"))))
;;                  (lambda (tc)
;;                    (test0003-context-on tc))
;;                  (lambda (tc)
;;                    (test0003-prepare-activation tc)
;;                    (test0003-context-set-on! tc #t)))

;; register-action for each rule
;; coordinate with mode-actions
(for-each
 (lambda (x)
   (let* ((label (alist-get x 'label))
          (id (string->symbol
               (string-append
                "action_test0003_" (symbol->string label)))))
     (register-action
      id
      (lambda (tc)
        (list id
              (substring (symbol->string label) 0 2)
              (symbol->string label)
              (string-append (symbol->string label) " mode")))
      (lambda (tc)
        (and (test0003-context-on tc)
             (eq? (test0003-context-rule-setting tc)
                  x)))
      (lambda (tc)
        (test0003-prepare-activation tc)
        (test0003-switch-rule-setting tc x)
        (test0003-context-set-on! tc #t)))))
 test0003-rule-list
 )

;; Update widget definitions based on action configurations. The
;; procedure is needed for on-the-fly reconfiguration involving the
;; custom API
(define test0003-configure-widgets
  (lambda ()
    (register-widget 'widget_test0003_input_mode
                     (activity-indicator-new test0003-input-mode-actions)
                     (actions-new test0003-input-mode-actions))))

(define test0003-context-rec-spec
  (append
   context-rec-spec
   '((on #f)
     (seq ())
     (candidate-window #f)
     (cands ())
     (cand-nth 0)
     (preedit-string "")
     (raw-commit #f)
     (rule ())
     (rule-setting #f)
     )))
(define-record 'test0003-context test0003-context-rec-spec)
(define test0003-context-new-internal test0003-context-new)

(define test0003-context-new
  (lambda args
    (let ((tc (apply test0003-context-new-internal args)))
      (test0003-context-set-widgets! tc test0003-widgets)
      tc)))

;; return selected candidate or current input sequences
(define test0003-get-preedit-string
  (lambda (tc)
    (let ((str (if (pair? (test0003-context-cands tc))
                   (cdr (list-ref
                         (test0003-context-cands tc)
                         (test0003-context-cand-nth tc)))
                   (apply string-append (reverse (test0003-context-seq tc))))))
      str)))

(define test0003-update-preedit
  (lambda (tc)
    (if (test0003-context-raw-commit tc)
        (test0003-context-set-raw-commit! tc #f)
        (let ((str (test0003-get-preedit-string tc)))
          (test0003-context-set-preedit-string! tc str)
          (im-clear-preedit tc)
          (im-pushback-preedit tc preedit-underline str)
          (im-update-preedit tc)))))

;;; find rule-cell which matches first character of seq.
;;; rule must be sorted.
(define test0003-reduce-rule
  (lambda (seq rule)
    ;;TODO: check using wildcard or not
    (if (or (equal? (car seq) "*") (equal? (car seq) "?"))
        rule
        (take-while
         (lambda (x) (equal? (caaar x) (car seq)))
         (or (find-tail (lambda (x) (equal? (caaar x) (car seq))) rule)
             ())                        ; if find-tail returns #f, use ()
         ))))

(define test0003-make-cands-wildcard
  (lambda (seq rule prediction)
    (let* ((cands ()))
      (for-each (lambda (x)
                  (if (wildcard-match-list? (caar x) seq prediction)
                      ;; add all candidates in the cell
                      (map (lambda (y)
                             (set! cands (cons (cons (caar x) y) cands)))
                           (nth 1 x))))
                rule)
      (reverse cands))))

(define test0003-make-cands-no-wildcard
  (lambda (seq rule prediction)
    (let* ((cands ()))
      (for-each (lambda (x)
                  (if (match-list? (caar x) seq prediction)
                      ;; add all candidates in the cell
                      (map (lambda (y)
                             (set! cands (cons (cons (caar x) y) cands)))
                           (nth 1 x))))
                rule)
      (reverse cands))))

;;; update context-cands from context-seq
;;; call it if context-seq is changed
(define test0003-update-candidate
  (lambda (tc)
    (if (null? (test0003-context-seq tc))
        #f                                        ; no sequence, do nothing
        (let* ((seq (reverse (test0003-context-seq tc)))
               ;;(rule (test0003-context-rule tc))
               (rule (test0003-reduce-rule seq (test0003-context-rule tc)))
               (prediction (alist-get (test0003-context-rule-setting tc)
                                      'prediction))
               (cands (if (alist-get (test0003-context-rule-setting tc) 'wildcard)
                          ;;(test0003-lib-make-cands-wildcard seq rule prediction)
                          (test0003-make-cands-wildcard seq rule prediction)
                          (test0003-make-cands-no-wildcard seq rule prediction))))
          (test0003-context-set-cands! tc cands)
          (test0003-context-set-cand-nth! tc 0)
          (if (and test0003-use-candidate-window? (pair? cands))
              (test0003-open-window
               tc (length cands) test0003-nr-candidates-max)
              (test0003-close-window tc))
          (im-select-candidate tc 0)))))

(define test0003-init-handler
  (lambda (id im arg)
    (let ((tc (test0003-context-new id im)))
      ;; load first rule
      (test0003-switch-rule-setting tc (car test0003-rule-list))
      tc)))

;;; compare sequence with wildcard pattern
;;; (wildcard-match '("a" "b" "c" "d" "e") '("a" "*" "c" "?" "e")) => #t
(define wildcard-match
  (lambda (seq pat)
    (if (null? seq)
        ;; no more sequence
        (if (null? pat)
            #t
            ;; whether all remaining pattern is "*" ?
            (not (find-tail (lambda (x) (not (equal? x "*"))) pat)))
        ;; has sequence
        (if (null? pat)
            #f
            (if (equal? "*" (car pat))
                (or (wildcard-match seq (cdr pat))
                    (wildcard-match (cdr seq) pat))
                (if (or (equal? (car seq) (car pat)) (equal? "?" (car pat)))
                    (wildcard-match (cdr seq) (cdr pat))
                    #f))))))

;;; if partial, append "*" to pat
(define wildcard-match-list?
  (lambda (seq pat . opt-partial)
    (let ((partial (if (pair? opt-partial) (car opt-partial) #f)))
      (if (equal? pat '("*")) #t        ; always match, return #t. bit fast
          (wildcard-match seq (if partial (append pat '("*")) pat))))))

;;; (match-list? '("a" "b" "c") '("a" "b")) => #f
;;; (match-list? '("a" "b" "c") '("a" "b") #t) => #t
;;; (match-list? '("a" "b" "c") '("a" "b") #f) => #f
(define match-list?
  (lambda (seq pat . opt-partial)
    (let ((partial (if (pair? opt-partial) (car opt-partial) #f)))
      (if (null? seq)
          (null? pat)
          (if (null? pat)
              partial
              (if (not (equal? (car seq) (car pat)))
                  #f
                  (match-list? (cdr seq) (cdr pat) partial)))))))

(define test0003-commit-by-numkey
  (lambda (tc key)
    (let* ((pagen (quotient (test0003-context-cand-nth tc)
                            test0003-nr-candidates-max))
           (keynum (- (numeric-ichar->integer key) 1))
           (keynum2 (if (< keynum 0) (+ keynum 10) keynum))
           (idx (+ keynum2 (* pagen test0003-nr-candidates-max))))
      (if (< idx (length (test0003-context-cands tc)))
          (begin
            ;;(uim-notify-info (format "~s" idx))
            (test0003-context-set-cand-nth! tc idx)
            (test0003-update-preedit tc) (test0003-commit tc))))))

(define test0003-proc-on-state
  (lambda (tc key key-state)
    (cond
     ;; off
     ((test0003-off-key? key key-state)
      ;;(test0003-commit tc)
      (test0003-clear tc)
      (test0003-context-set-on! tc #f))
     ;; switch rule
     ((test0003-switch-rule1-key? key key-state)
      (if (> (length test0003-rule-list) 0)
          (test0003-switch-rule-setting tc (nth 0 test0003-rule-list))
          (test0003-commit-raw tc)))
     ((test0003-switch-rule2-key? key key-state)
      (if (> (length test0003-rule-list) 1)
          (test0003-switch-rule-setting tc (nth 1 test0003-rule-list))
          (test0003-commit-raw tc)))
     ((test0003-switch-rule3-key? key key-state)
      (if (> (length test0003-rule-list) 2)
          (test0003-switch-rule-setting tc (nth 2 test0003-rule-list))
          (test0003-commit-raw tc)))
     ((test0003-switch-rule4-key? key key-state)
      (if (> (length test0003-rule-list) 3)
          (test0003-switch-rule-setting tc (nth 3 test0003-rule-list))
          (test0003-commit-raw tc)))
     ((test0003-switch-rule5-key? key key-state)
      (if (> (length test0003-rule-list) 4)
          (test0003-switch-rule-setting tc (nth 4 test0003-rule-list))
          (test0003-commit-raw tc)))
     ((test0003-switch-rule6-key? key key-state)
      (if (> (length test0003-rule-list) 5)
          (test0003-switch-rule-setting tc (nth 5 test0003-rule-list))
          (test0003-commit-raw tc)))
     ((test0003-switch-rule7-key? key key-state)
      (if (> (length test0003-rule-list) 6)
          (test0003-switch-rule-setting tc (nth 6 test0003-rule-list))
          (test0003-commit-raw tc)))
     ((test0003-switch-rule8-key? key key-state)
      (if (> (length test0003-rule-list) 7)
          (test0003-switch-rule-setting tc (nth 7 test0003-rule-list))
          (test0003-commit-raw tc)))
     ((test0003-switch-rule9-key? key key-state)
      (if (> (length test0003-rule-list) 8)
          (test0003-switch-rule-setting tc (nth 8 test0003-rule-list))
          (test0003-commit-raw tc)))
     ;; commit
     ((test0003-commit-key? key key-state)
      (if (null? (test0003-context-seq tc))
          (test0003-commit-raw tc)
          (test0003-commit tc)))
     ;; cancel
     ((test0003-cancel-key? key key-state)
      (if (null? (test0003-context-seq tc))
          (test0003-commit-raw tc)
          (test0003-clear tc)))
     ;; next candidate
     ((test0003-next-candidate-key? key key-state)
      (if (null? (test0003-context-cands tc))
          (test0003-commit-raw tc)
          (let ((nr (length (test0003-context-cands tc)))
                (n (+ (test0003-context-cand-nth tc) 1)))
            (test0003-context-set-cand-nth! tc (if (> nr n) n 0))
            (im-select-candidate tc (test0003-context-cand-nth tc)))))
     ;; prev candidate
     ((test0003-prev-candidate-key? key key-state)
      (if (null? (test0003-context-cands tc))
          (test0003-commit-raw tc)
          (let ((nr (length (test0003-context-cands tc)))
                (n (- (test0003-context-cand-nth tc) 1)))
            (test0003-context-set-cand-nth! tc (if (<= 0 n) n (- nr 1)))
            (im-select-candidate tc (test0003-context-cand-nth tc)))))
     ;; next candidate page
     ((test0003-next-page-key? key key-state)
      (if (null? (test0003-context-cands tc))
          (test0003-commit-raw tc)
          (im-shift-page-candidate tc #t)))
     ;; prev candidate page
     ((test0003-prev-page-key? key key-state)
      (if (null? (test0003-context-cands tc))
          (test0003-commit-raw tc)
          (im-shift-page-candidate tc #f)))
     ;; first candidate
     ((eq? key 'home)
      (if (null? (test0003-context-cands tc))
          (test0003-commit-raw tc)
          (begin (test0003-context-set-cand-nth! tc 0)
                 (im-select-candidate tc 0))))
     ;; last candidate
     ((eq? key 'end)
      (if (null? (test0003-context-cands tc))
          (test0003-commit-raw tc)
          (let ((pos (- (length (test0003-context-cands tc)) 1)))
            (test0003-context-set-cand-nth! tc pos)
            (im-select-candidate tc pos))))
     ;; toggle candidate window
     ((test0003-toggle-candidate-window-key? key key-state)
      (set! test0003-use-candidate-window? (not test0003-use-candidate-window?))
      (test0003-update-candidate tc))
     ;; toggle prediction
     ((test0003-toggle-prediction-key? key key-state)
      (alist-set! (test0003-context-rule-setting tc) 'prediction
                  (not (alist-get
                        (test0003-context-rule-setting tc) 'prediction)))
      (test0003-update-candidate tc))
     ;; toggle autocommit
     ((test0003-toggle-autocommit-key? key key-state)
      (alist-set! (test0003-context-rule-setting tc) 'autocommit
                  (not (alist-get
                        (test0003-context-rule-setting tc) 'autocommit))))
     ;; toggle wildcard-match
     ((test0003-toggle-wildcard-key? key key-state)
      (alist-set! (test0003-context-rule-setting tc) 'wildcard
                  (not (alist-get
                        (test0003-context-rule-setting tc) 'wildcard))))
     ;; backspace
     ((test0003-backspace-key? key key-state)
      (let ((seq (test0003-context-seq tc)))
        (if (null? seq)
            (test0003-commit-raw tc)
            (begin
              (test0003-context-set-seq! tc (cdr seq))
              (if (null? (test0003-context-seq tc))
                  (test0003-clear tc)
                  (test0003-update-candidate tc))))))
     ;; modifier except shift
     ((and (modifier-key-mask key-state)
           (not (shift-key-mask key-state)))
      (im-commit-raw tc))
     ;; input character
     ((not (symbol? key))
      (let ((old-cands (test0003-context-cands tc))
            (old-cand-nth (test0003-context-cand-nth tc))
            (key-str (charcode->string key)))
        (test0003-context-set-seq!
         tc (if (null? (test0003-context-seq tc))
                (list key-str)
                (cons key-str (test0003-context-seq tc))))
        (test0003-update-candidate tc)
        (cond
         ((and test0003-commit-candidate-by-numeral-key?
               (ichar-numeric? key)
               (null? (test0003-context-cands tc))
               (pair? old-cands))
          (test0003-context-set-seq! tc (cdr (test0003-context-seq tc)))
          (test0003-context-set-cands! tc old-cands)
          (test0003-context-set-cand-nth! tc old-cand-nth)
          (test0003-commit-by-numkey tc key))
         (else
          (if (alist-get (test0003-context-rule-setting tc) 'autocommit)
              (cond
               ;; only one candidate, commit
               ((= (length (test0003-context-cands tc)) 1)
                (test0003-commit tc))
               ;; no candidate
               ;; if has old-cands, commit old-cands and process new sequence
               ;; if don't exist old-cands, commit preedit
               ((= (length (test0003-context-cands tc)) 0)
                (test0003-context-set-cands! tc old-cands)
                (test0003-context-set-cand-nth! tc old-cand-nth)
                (test0003-commit tc)
                (if (pair? old-cands)
                    ;; process new sequence. be careful about infinite loop
                    (test0003-proc-on-state tc key key-state)))))))
        ))
     (else (test0003-commit-raw tc))
     )))

(define test0003-proc-off-state
  (lambda (tc key key-state)
    (if (test0003-on-key? key key-state)
        (test0003-context-set-on! tc #t)
        (im-commit-raw tc))))

;;; commit current preedit string
(define test0003-commit
  (lambda (tc)
    (im-commit tc (test0003-get-preedit-string tc))
    (test0003-clear tc)))

;;; open candidate window
(define test0003-open-window
  (lambda (tc len nr)
    (im-activate-candidate-selector tc len nr)
    (test0003-context-set-candidate-window! tc #t)))

;;; close candidate window
(define test0003-close-window
  (lambda (tc)
    (if (test0003-context-candidate-window tc)
        (begin
          (im-deactivate-candidate-selector tc)
          (test0003-context-set-candidate-window! tc #f)))))

;;; clear context variable.
;;; call this after commit-key, cancel-key
(define test0003-clear
  (lambda (tc)
    (test0003-context-set-cands! tc ())
    (test0003-context-set-cand-nth! tc 0)
    (test0003-context-set-seq! tc ())
    (test0003-context-set-preedit-string! tc "")
    (test0003-close-window tc)))

(define test0003-release-handler
  (lambda (tc)
    #f))

(define test0003-key-press-handler
  (lambda (tc key key-state)
    ;; (uim-notify-info
    ;;  (format "test0003-key-press-handler: key=~s state=~s" key key-state))
    (if (ichar-control? key)
        (im-commit-raw tc)
        (if (test0003-context-on tc)
            (test0003-proc-on-state tc key key-state)
            (test0003-proc-off-state tc key key-state)
            ))
    (test0003-update-preedit tc)
    ))

(define test0003-commit-raw
  (lambda (tc)
    (im-commit-raw tc)
    (test0003-context-set-raw-commit! tc #t)))

(define test0003-key-release-handler
  (lambda (tc key key-state)
    (if (or (ichar-control? key)
            (not (test0003-context-on tc)))
        ;; don't discard key release event for apps
        (test0003-commit-raw tc))))

(define test0003-reset-handler
  (lambda (tc)
    ;; (uim-notify-info "test0003-reset-handler")
    (if (test0003-context-on tc)
        (test0003-clear tc))
    ))

;;; return the idx'th candidate
(define test0003-get-candidate-handler
  (lambda (tc idx accel-enum-hint)
    ;; (uim-notify-info (format "test0003-get-candidate-handler:~d" idx))
    (let* ((cands (test0003-context-cands tc))
           (cell (nth idx cands))
           (cand (if (pair? cell)
                     (string-append (cdr cell) ":"
                                    (apply string-append (car cell)))
                     cell)))
      ;; (uim-notify-info (format "~s" cell))
      (list cand
            (digit->string
             ;; reduce display space
             ;;(+ idx 1))
             (remainder (+ idx 1) 10))
            ""))))

;;; set selected candidate number
(define test0003-set-candidate-index-handler
  (lambda (tc idx)
    ;; (uim-notify-info (format "candidate-index-handler: ~s" idx))
    (test0003-context-set-cand-nth! tc idx)
    (test0003-update-preedit tc)))

(test0003-configure-widgets)

(register-im
 'test0003
 "*"
 "UTF-8"
 (N_ "test0003")
 (N_ "test0003 description")
 #f
 test0003-init-handler
 test0003-release-handler
 context-mode-handler
 test0003-key-press-handler
 test0003-key-release-handler
 test0003-reset-handler
 test0003-get-candidate-handler
 test0003-set-candidate-index-handler
 context-prop-activate-handler
 #f
 #f
 #f
 #f
 #f
 )

;;; Local Variables:
;;; tab-width: 8
;;; End:
