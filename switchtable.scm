;;; debug message use format
;; (require-extension (srfi 48))                ; format

;;; return scm-file-name in full path
;;; do not add ".scm" to scm
(define switchtable-find-module
  (lambda (scm)
    (find-module-scm-path uim-plugin-scm-load-path scm)))

;; search table file in sys-pkgdatadir and config-path
(define switchtable-find-tablefile
  (lambda (tablefile)
    (cond ((not (string? tablefile))        ; not a string
           #f)
          ((file-readable? tablefile)       ; try no prefix first
           tablefile)
          ((file-readable? (string-append (get-config-path #f) "/" tablefile))
           (string-append (get-config-path #f) "/" tablefile))
          ((file-readable? (string-append (sys-pkgdatadir) "/" tablefile))
           (string-append (sys-pkgdatadir) "/" tablefile))
          (else                         ; not found
           #f))))

(define alist-get
  (lambda (alist key)
    (cdr (or (assq key alist) '(#f . #f)))))

(define alist-set!
  (lambda (alist key value)
    (if (pair? alist)
        (let ((target (assq key alist)))
          (if target
              (set-cdr! target value)
              ;; if no key, add new cell (key . value) to alist
              (begin
                (set-cdr! alist (cons (car alist) (cdr alist)))
                (set-car! alist (cons key value))))))))

;; ;; return position of first found of elt in lst. specify 0 for idx.
;; (define list-index
;;   (lambda (lst elt . index)
;;     (let ((idx (if (pair? index) (car index) 0)))
;;       (cond ((not (pair? lst)) #f)
;;             ((eq? (car lst) elt) idx)
;;             (else (list-index (cdr lst) elt (+ idx 1)))))))

;; `require' do not search ~/.uim.d/plugin/ directory
;; (require-custom "switchtable-custom.scm")
(require-custom (switchtable-find-module "switchtable-custom"))

;;; user configs

;; widgets and actions

;; widgets
(define switchtable-widgets '(widget_switchtable_input_mode
                           widget_switchtable_toggle_autocommit
                           widget_switchtable_toggle_candidate_window
                           widget_switchtable_toggle_prediction
                           widget_switchtable_toggle_wildcard))

;; default activity for each widgets
(define default-widget_switchtable_input_mode 'action_switchtable_off)

;; actions of widget_switchtable_input_mode
;; (define switchtable-input-mode-actions
;;   '(action_switchtable_off
;;    action_switchtable_on
;;     ))
;; action for each rule
;; coordinate with register-action
(define switchtable-input-mode-actions
  (cons 'action_switchtable_off
        (map
         (lambda (x)
           (string->symbol
            (string-append
             "action_switchtable_" (symbol->string (alist-get x 'label)))))
         switchtable-rule-list)))

(define switchtable-toggle-autocommit-actions
  '(action_switchtable_autocommit_on action_switchtable_autocommit_off))

(define switchtable-toggle-candidate-window-actions
  '(action_switchtable_candidate_window_on action_switchtable_candidate_window_off))

(define switchtable-toggle-prediction-actions
  '(action_switchtable_prediction_on action_switchtable_prediction_off))

(define switchtable-toggle-wildcard-actions
  '(action_switchtable_wildcard_on action_switchtable_wildcard_off))

;;; implementations

(define switchtable-prepare-activation
  (lambda (tc)
    (switchtable-clear tc)
    (switchtable-update-preedit tc)))

(define switchtable-switch-rule-setting
  (lambda (tc rule-setting)
    ;;(uim-notify-info "switch-rule-setting")
    (switchtable-context-set-rule-setting! tc rule-setting)
    (switchtable-update-candidate tc)))

(register-action 'action_switchtable_off
                 (lambda (tc)
                   (list
                    'off
                    "-"
                    (N_ "off")
                    (N_ "Direct Input Mode")))
                 (lambda (tc)
                   (not (switchtable-context-on tc)))
                 (lambda (tc)
                   (switchtable-prepare-activation tc)
                   (switchtable-context-set-on! tc #f)))

;; (register-action 'action_switchtable_on
;;                  (lambda (tc)
;;                    (list
;;                     'on
;;                     "O"
;;                     (N_ "on")
;;                     (string-append "on" (N_" Mode"))))
;;                  (lambda (tc)
;;                    (switchtable-context-on tc))
;;                  (lambda (tc)
;;                    (switchtable-prepare-activation tc)
;;                    (switchtable-context-set-on! tc #t)))

;; register-action for each rule
;; coordinate with mode-actions
(for-each
 (lambda (x)
   (let* ((label (alist-get x 'label))
          (id (string->symbol
               (string-append
                "action_switchtable_" (symbol->string label)))))
     (register-action
      id
      (lambda (tc)
        (list id
              (substring (symbol->string label) 0 2)
              (symbol->string label)
              (string-append (symbol->string label) " mode")))
      (lambda (tc)
        (and (switchtable-context-on tc)
             (eq? (switchtable-context-rule-setting tc)
                  x)))
      (lambda (tc)
        (switchtable-prepare-activation tc)
        (switchtable-switch-rule-setting tc x)
        (switchtable-context-set-on! tc #t)))))
 switchtable-rule-list
 )

;; register action: autocommit, candidate_window, prediction, wildcard,
(register-action 'action_switchtable_autocommit_on
                 (lambda (tc)
                   (list 'autocommit
                         "a"
                         (N_ "autocommit on")
                         (N_ "autocommit mode")))
                 (lambda (tc)
                   (and (switchtable-context-on tc)
                        (alist-get
                         (switchtable-context-rule-setting tc)
                         'autocommit)))
                 (lambda (tc)
                   (alist-set! (switchtable-context-rule-setting tc) 'autocommit #t)))

(register-action 'action_switchtable_autocommit_off
                 (lambda (tc)
                   (list 'autocommit
                         "_"
                         (N_ "autocommit off")
                         (N_ "autocommit mode")))
                 (lambda (tc)
                   (and (switchtable-context-on tc)
                        (not (alist-get
                              (switchtable-context-rule-setting tc)
                              'autocommit))))
                 (lambda (tc)
                   (alist-set! (switchtable-context-rule-setting tc) 'autocommit #f)))

(register-action 'action_switchtable_candidate_window_on
                 (lambda (tc)
                   (list 'candidate_window
                         "c"
                         (N_ "candidate-window on")
                         (N_ "candidate-window mode")))
                 (lambda (tc)
                   (and (switchtable-context-on tc)
                        switchtable-use-candidate-window?))
                 (lambda (tc)
                   (set! switchtable-use-candidate-window? #t)))

(register-action 'action_switchtable_candidate_window_off
                 (lambda (tc)
                   (list 'candidate_window
                         "_"
                         (N_ "candidate-window off")
                         (N_ "candidate-window mode")))
                 (lambda (tc)
                   (and (switchtable-context-on tc)
                        (not switchtable-use-candidate-window?)))
                 (lambda (tc)
                   (set! switchtable-use-candidate-window? #f)))

(register-action 'action_switchtable_prediction_on
                 (lambda (tc)
                   (list 'prediction
                         "p"
                         (N_ "prediction on")
                         (N_ "prediction mode")))
                 (lambda (tc)
                   (and (switchtable-context-on tc)
                        (alist-get
                         (switchtable-context-rule-setting tc)
                         'prediction)))
                 (lambda (tc)
                   (alist-set! (switchtable-context-rule-setting tc) 'prediction #t)
                   (switchtable-update-candidate tc)))

(register-action 'action_switchtable_prediction_off
                 (lambda (tc)
                   (list 'prediction
                         "_"
                         (N_ "prediction off")
                         (N_ "prediction mode")))
                 (lambda (tc)
                   (and (switchtable-context-on tc)
                        (not (alist-get
                              (switchtable-context-rule-setting tc)
                              'prediction))))
                 (lambda (tc)
                   (alist-set! (switchtable-context-rule-setting tc) 'prediction #f)
                   (switchtable-update-candidate tc)))

(register-action 'action_switchtable_wildcard_on
                 (lambda (tc)
                   (list 'wildcard
                         "w"
                         (N_ "wildcard on")
                         (N_ "wildcard mode")))
                 (lambda (tc)
                   (and (switchtable-context-on tc)
                        (alist-get
                         (switchtable-context-rule-setting tc)
                         'wildcard)))
                 (lambda (tc)
                   (alist-set! (switchtable-context-rule-setting tc) 'wildcard #t)))

(register-action 'action_switchtable_wildcard_off
                 (lambda (tc)
                   (list 'wildcard
                         "_"
                         (N_ "wildcard off")
                         (N_ "wildcard mode")))
                 (lambda (tc)
                   (and (switchtable-context-on tc)
                        (not (alist-get
                              (switchtable-context-rule-setting tc)
                              'wildcard))))
                 (lambda (tc)
                   (alist-set! (switchtable-context-rule-setting tc) 'wildcard #f)))

;; Update widget definitions based on action configurations. The
;; procedure is needed for on-the-fly reconfiguration involving the
;; custom API
(define switchtable-configure-widgets
  (lambda ()
    (register-widget 'widget_switchtable_input_mode
                     (activity-indicator-new switchtable-input-mode-actions)
                     (actions-new switchtable-input-mode-actions))
    (register-widget 'widget_switchtable_toggle_autocommit
                     (activity-indicator-new switchtable-toggle-autocommit-actions)
                     (actions-new switchtable-toggle-autocommit-actions))
    (register-widget 'widget_switchtable_toggle_candidate_window
                     (activity-indicator-new switchtable-toggle-candidate-window-actions)
                     (actions-new switchtable-toggle-candidate-window-actions))
    (register-widget 'widget_switchtable_toggle_prediction
                     (activity-indicator-new switchtable-toggle-prediction-actions)
                     (actions-new switchtable-toggle-prediction-actions))
    (register-widget 'widget_switchtable_toggle_wildcard
                     (activity-indicator-new switchtable-toggle-wildcard-actions)
                     (actions-new switchtable-toggle-wildcard-actions))
    (context-list-replace-widgets! 'switchtable switchtable-widgets)))

(define switchtable-context-rec-spec
  (append
   context-rec-spec
   '((on #f)
     (seq ())
     (candidate-window #f)
     (cands ())
     (cand-nth 0)
     (preedit-string "")
     (raw-commit #f)
     (rule-setting ())
     )))
(define-record 'switchtable-context switchtable-context-rec-spec)
(define switchtable-context-new-internal switchtable-context-new)

(define switchtable-context-new
  (lambda args
    (let ((tc (apply switchtable-context-new-internal args)))
      (switchtable-context-set-widgets! tc switchtable-widgets)
      tc)))

;; return selected candidate or current input sequences
(define switchtable-get-preedit-string
  (lambda (tc)
    (let ((str (if (pair? (switchtable-context-cands tc))
                   (cdr (list-ref
                         (switchtable-context-cands tc)
                         (switchtable-context-cand-nth tc)))
                   (apply string-append (reverse (switchtable-context-seq tc))))))
      str)))

(define switchtable-update-preedit
  (lambda (tc)
    (if (switchtable-context-raw-commit tc)
        (switchtable-context-set-raw-commit! tc #f)
        (let ((str (switchtable-get-preedit-string tc)))
          (switchtable-context-set-preedit-string! tc str)
          (im-clear-preedit tc)
          (im-pushback-preedit tc preedit-underline str)
          (im-update-preedit tc)))))

;;; update context-cands from context-seq
;;; call it if context-seq is changed
(define switchtable-update-candidate
  (lambda (tc)
    (if (null? (switchtable-context-seq tc))
        #f                              ; no sequence, do nothing
        (let* ((rule-setting (switchtable-context-rule-setting tc))
               (pre (alist-get rule-setting 'prediction))
               (wc (alist-get rule-setting 'wildcard))
               (query (apply string-append (reverse (switchtable-context-seq tc))))
               (table (switchtable-find-tablefile (alist-get rule-setting 'tablefile)))
               (cands
                (if wc
                    (if pre
                        ;; prefixs wildcard
                        (switchtable-lib-make-cands-wildcard-prefix query table)
                        ;; whole wildcard
                        (switchtable-lib-make-cands-wildcard query table))
                    (if pre
                        ;; prefixs search
                        (switchtable-lib-make-cands-find-prefix query table)
                        ;; whole search
                        (switchtable-lib-make-cands-find query table))))
               )
          ;;(uim-notify-info (format "~s" cands))
          (switchtable-context-set-cands! tc cands)
          (switchtable-context-set-cand-nth! tc 0)
          (if (and switchtable-use-candidate-window? (pair? cands))
              (begin
                (switchtable-open-window tc (length cands) switchtable-nr-candidates-max)
                (im-select-candidate tc 0))
              (switchtable-close-window tc))
          ))))

(define switchtable-init-handler
  (lambda (id im arg)
    (let ((tc (switchtable-context-new id im)))
      ;; load first rule
      (switchtable-switch-rule-setting tc (car switchtable-rule-list))
      tc)))

(define switchtable-commit-by-numkey
  (lambda (tc key)
    (let* ((pagen (quotient (switchtable-context-cand-nth tc)
                            switchtable-nr-candidates-max))
           (keynum (- (numeric-ichar->integer key) 1))
           (keynum2 (if (< keynum 0) (+ keynum 10) keynum))
           (idx (+ keynum2 (* pagen switchtable-nr-candidates-max))))
      (if (< idx (length (switchtable-context-cands tc)))
          (begin
            ;;(uim-notify-info (format "~s" idx))
            (switchtable-context-set-cand-nth! tc idx)
            (switchtable-update-preedit tc) (switchtable-commit tc))))))

(define switchtable-proc-on-state
  (lambda (tc key key-state)
    (cond
     ;; off
     ((switchtable-off-key? key key-state)
      ;;(switchtable-commit tc)
      (switchtable-clear tc)
      (switchtable-context-set-on! tc #f))
     ;; switch rule
     ((switchtable-switch-rule1-key? key key-state)
      (if (> (length switchtable-rule-list) 0)
          (switchtable-switch-rule-setting tc (nth 0 switchtable-rule-list))
          (switchtable-commit-raw tc)))
     ((switchtable-switch-rule2-key? key key-state)
      (if (> (length switchtable-rule-list) 1)
          (switchtable-switch-rule-setting tc (nth 1 switchtable-rule-list))
          (switchtable-commit-raw tc)))
     ((switchtable-switch-rule3-key? key key-state)
      (if (> (length switchtable-rule-list) 2)
          (switchtable-switch-rule-setting tc (nth 2 switchtable-rule-list))
          (switchtable-commit-raw tc)))
     ((switchtable-switch-rule4-key? key key-state)
      (if (> (length switchtable-rule-list) 3)
          (switchtable-switch-rule-setting tc (nth 3 switchtable-rule-list))
          (switchtable-commit-raw tc)))
     ((switchtable-switch-rule5-key? key key-state)
      (if (> (length switchtable-rule-list) 4)
          (switchtable-switch-rule-setting tc (nth 4 switchtable-rule-list))
          (switchtable-commit-raw tc)))
     ((switchtable-switch-rule6-key? key key-state)
      (if (> (length switchtable-rule-list) 5)
          (switchtable-switch-rule-setting tc (nth 5 switchtable-rule-list))
          (switchtable-commit-raw tc)))
     ((switchtable-switch-rule7-key? key key-state)
      (if (> (length switchtable-rule-list) 6)
          (switchtable-switch-rule-setting tc (nth 6 switchtable-rule-list))
          (switchtable-commit-raw tc)))
     ((switchtable-switch-rule8-key? key key-state)
      (if (> (length switchtable-rule-list) 7)
          (switchtable-switch-rule-setting tc (nth 7 switchtable-rule-list))
          (switchtable-commit-raw tc)))
     ((switchtable-switch-rule9-key? key key-state)
      (if (> (length switchtable-rule-list) 8)
          (switchtable-switch-rule-setting tc (nth 8 switchtable-rule-list))
          (switchtable-commit-raw tc)))
     ;; commit
     ((switchtable-commit-key? key key-state)
      (if (null? (switchtable-context-seq tc))
          (switchtable-commit-raw tc)
          (switchtable-commit tc)))
     ;; cancel
     ((switchtable-cancel-key? key key-state)
      (if (null? (switchtable-context-seq tc))
          (switchtable-commit-raw tc)
          (switchtable-clear tc)))
     ;; next candidate
     ((switchtable-next-candidate-key? key key-state)
      (if (null? (switchtable-context-cands tc))
          (switchtable-commit-raw tc)
          (let ((nr (length (switchtable-context-cands tc)))
                (n (+ (switchtable-context-cand-nth tc) 1)))
            (switchtable-context-set-cand-nth! tc (if (> nr n) n 0))
            (if switchtable-use-candidate-window?
                (im-select-candidate tc (switchtable-context-cand-nth tc))))))
     ;; prev candidate
     ((switchtable-prev-candidate-key? key key-state)
      (if (null? (switchtable-context-cands tc))
          (switchtable-commit-raw tc)
          (let ((nr (length (switchtable-context-cands tc)))
                (n (- (switchtable-context-cand-nth tc) 1)))
            (switchtable-context-set-cand-nth! tc (if (<= 0 n) n (- nr 1)))
            (if switchtable-use-candidate-window?
                (im-select-candidate tc (switchtable-context-cand-nth tc))))))
     ;; next candidate page
     ((switchtable-next-page-key? key key-state)
      (if (null? (switchtable-context-cands tc))
          (switchtable-commit-raw tc)
          (if switchtable-use-candidate-window?
              (im-shift-page-candidate tc #t)
              (let* ((nr (length (switchtable-context-cands tc)))
                     (nth (switchtable-context-cand-nth tc))
                     (n (+ nth switchtable-nr-candidates-max)))
                (switchtable-context-set-cand-nth! tc (if (> nr n) n 0)))
              )))
     ;; prev candidate page
     ((switchtable-prev-page-key? key key-state)
      (if (null? (switchtable-context-cands tc))
          (switchtable-commit-raw tc)
          (if switchtable-use-candidate-window?
              (im-shift-page-candidate tc #f)
              (let* ((nr (length (switchtable-context-cands tc)))
                     (nth (switchtable-context-cand-nth tc))
                     (n (- nth switchtable-nr-candidates-max)))
                (switchtable-context-set-cand-nth! tc (if (<= 0 n) n (- nr 1))))
              )))
     ;; first candidate
     ((eq? key 'home)
      (if (null? (switchtable-context-cands tc))
          (switchtable-commit-raw tc)
          (begin (switchtable-context-set-cand-nth! tc 0)
                 (if switchtable-use-candidate-window?
                     (im-select-candidate tc 0)))))
     ;; last candidate
     ((eq? key 'end)
      (if (null? (switchtable-context-cands tc))
          (switchtable-commit-raw tc)
          (let ((pos (- (length (switchtable-context-cands tc)) 1)))
            (switchtable-context-set-cand-nth! tc pos)
            (if switchtable-use-candidate-window?
                (im-select-candidate tc pos)))))
     ;; toggle candidate window
     ((switchtable-toggle-candidate-window-key? key key-state)
      (set! switchtable-use-candidate-window? (not switchtable-use-candidate-window?))
      ;; open window if switchtable-use-candidate-window? & candidates exists,
      ;; otherwise close window
      (let ((cands (switchtable-context-cands tc)))
        (if (and switchtable-use-candidate-window? (pair? cands))
            (begin
              (switchtable-open-window tc (length cands) switchtable-nr-candidates-max)
              (im-select-candidate tc (switchtable-context-cand-nth tc)))
            (switchtable-close-window tc))))
     ;; toggle prediction
     ((switchtable-toggle-prediction-key? key key-state)
      (alist-set! (switchtable-context-rule-setting tc) 'prediction
                  (not (alist-get
                        (switchtable-context-rule-setting tc) 'prediction)))
      (switchtable-update-candidate tc))
     ;; toggle autocommit
     ((switchtable-toggle-autocommit-key? key key-state)
      (alist-set! (switchtable-context-rule-setting tc) 'autocommit
                  (not (alist-get
                        (switchtable-context-rule-setting tc) 'autocommit))))
     ;; toggle wildcard-match
     ((switchtable-toggle-wildcard-key? key key-state)
      (alist-set! (switchtable-context-rule-setting tc) 'wildcard
                  (not (alist-get
                        (switchtable-context-rule-setting tc) 'wildcard))))
     ;; backspace
     ((switchtable-backspace-key? key key-state)
      (let ((seq (switchtable-context-seq tc)))
        (if (null? seq)
            (switchtable-commit-raw tc)
            (begin
              (switchtable-context-set-seq! tc (cdr seq))
              (if (null? (switchtable-context-seq tc))
                  (switchtable-clear tc)
                  (switchtable-update-candidate tc))))))
     ;; modifier except shift
     ((and (modifier-key-mask key-state)
           (not (shift-key-mask key-state)))
      (im-commit-raw tc))
     ;; control key should be ignored here?
     ((control-key-mask key-state)
      (im-commit-raw tc))
     ;; input character
     ((not (symbol? key))
      (let ((old-cands (switchtable-context-cands tc))
            (old-cand-nth (switchtable-context-cand-nth tc))
            (key-str (charcode->string key)))
        (switchtable-context-set-seq!
         tc (if (null? (switchtable-context-seq tc))
                (list key-str)
                (cons key-str (switchtable-context-seq tc))))
        (switchtable-update-candidate tc)
        (cond
         ((and switchtable-commit-candidate-by-numeral-key?
               (ichar-numeric? key)
               (null? (switchtable-context-cands tc))
               (pair? old-cands))
          (switchtable-context-set-seq! tc (cdr (switchtable-context-seq tc)))
          (switchtable-context-set-cands! tc old-cands)
          (switchtable-context-set-cand-nth! tc old-cand-nth)
          (switchtable-commit-by-numkey tc key))
         (else
          (if (alist-get (switchtable-context-rule-setting tc) 'autocommit)
              (cond
               ;; only one candidate, commit
               ((= (length (switchtable-context-cands tc)) 1)
                (switchtable-commit tc))
               ;; no candidate
               ;; if has old-cands, commit old-cands and process new sequence
               ;; if don't exist old-cands, commit preedit
               ((= (length (switchtable-context-cands tc)) 0)
                (switchtable-context-set-cands! tc old-cands)
                (switchtable-context-set-cand-nth! tc old-cand-nth)
                (switchtable-commit tc)
                (if (pair? old-cands)
                    ;; process new sequence. be careful about infinite loop
                    (switchtable-proc-on-state tc key key-state)))))))
        ))
     (else (switchtable-commit-raw tc))
     )))

(define switchtable-proc-off-state
  (lambda (tc key key-state)
    ;;(uim-notify-info "switchtable-proc-off-state")
    (if (switchtable-on-key? key key-state)
        (switchtable-context-set-on! tc #t)
        (im-commit-raw tc))))

;;; commit current preedit string
(define switchtable-commit
  (lambda (tc)
    (im-commit tc (switchtable-get-preedit-string tc))
    (switchtable-clear tc)))

;;; open candidate window
(define switchtable-open-window
  (lambda (tc len nr)
    (im-activate-candidate-selector tc len nr)
    (switchtable-context-set-candidate-window! tc #t)))

;;; close candidate window
(define switchtable-close-window
  (lambda (tc)
    (if (switchtable-context-candidate-window tc)
        (begin
          (im-deactivate-candidate-selector tc)
          (switchtable-context-set-candidate-window! tc #f)))))

;;; clear context variable.
;;; call this after commit-key, cancel-key
(define switchtable-clear
  (lambda (tc)
    (switchtable-context-set-cands! tc ())
    (switchtable-context-set-cand-nth! tc 0)
    (switchtable-context-set-seq! tc ())
    (switchtable-context-set-preedit-string! tc "")
    (switchtable-close-window tc)))

(define switchtable-release-handler
  (lambda (tc)
    ;;(uim-notify-info "release-handler")
    #f))

(define switchtable-key-press-handler
  (lambda (tc key key-state)
    ;; (uim-notify-info
    ;;  (format "switchtable-key-press-handler: key=~s state=~s" key key-state))
    (if (ichar-control? key)
        (im-commit-raw tc)
        (if (switchtable-context-on tc)
            (switchtable-proc-on-state tc key key-state)
            (switchtable-proc-off-state tc key key-state)
            ))
    (switchtable-update-preedit tc)
    ))

(define switchtable-commit-raw
  (lambda (tc)
    (im-commit-raw tc)
    (switchtable-context-set-raw-commit! tc #t)))

(define switchtable-key-release-handler
  (lambda (tc key key-state)
    (if (or (ichar-control? key)
            (not (switchtable-context-on tc)))
        ;; don't discard key release event for apps
        (switchtable-commit-raw tc))))

(define switchtable-reset-handler
  (lambda (tc)
    ;; (uim-notify-info "switchtable-reset-handler")
    (if (switchtable-context-on tc)
        (switchtable-clear tc))
    ))

;;; return the idx'th candidate
(define switchtable-get-candidate-handler
  (lambda (tc idx accel-enum-hint)
    ;; (uim-notify-info (format "switchtable-get-candidate-handler:~d" idx))
    (let* ((cands (switchtable-context-cands tc))
           (cell (nth idx cands))
           (cand (string-append (cdr cell) ":" (car cell)))
           )
      ;; (uim-notify-info (format "~s" cell))
      (list cand
            (digit->string
             ;; reduce display space
             ;;(+ idx 1))
             (remainder (+ idx 1) 10))
            ""))))

;;; set selected candidate number
(define switchtable-set-candidate-index-handler
  (lambda (tc idx)
    ;; (uim-notify-info (format "candidate-index-handler: ~s" idx))
    (switchtable-context-set-cand-nth! tc idx)
    (switchtable-update-preedit tc)))

(switchtable-configure-widgets)

(register-im
 'switchtable
 "*"
 "UTF-8"
 (N_ "switchtable")
 (N_ "switchtable description")
 #f
 switchtable-init-handler
 switchtable-release-handler
 context-mode-handler
 switchtable-key-press-handler
 switchtable-key-release-handler
 switchtable-reset-handler
 switchtable-get-candidate-handler
 switchtable-set-candidate-index-handler
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
