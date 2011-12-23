(eval-when-compile (require 'cl))

;;------------------------------
;; Define constant variable
;;------------------------------
(defconst adcal:buffer-name "*AdventCalendarList*"
  "Buffer name used to `adcal:http-get'")

(defconst adcal:list-uri "http://d.hatena.ne.jp/tt_clown/20111110/advent_calendar_2011_jp"
  "URL of Advent Calendar table")

(defvar adcal:cache '()
  "Cache which stores vim hacks list obtained by `adcal:paese-list'")

;;------------------------------
;; Define exception error
;;------------------------------
(put 'adcal:exception-not-retrieved 'error-message
     "Advent Calendar - Not retrieved")
(put 'adcal:exception-not-retrieved 'error-conditions
     '(adcal:exception-not-retrieved error))

;;------------------------------
;; Network Function
;;------------------------------
(defun adcal:http-get (url &optional buffer rdcoding)
  (if (null buffer) (setq buffer adcal:buffer-name))
  (ignore-errors (kill-buffer buffer))
  (let ((coding-system-for-read rdcoding))
    (unless (eq 0 (call-process "curl" nil `(,buffer nil) nil
                                "-f"
                                "-X" "GET"
                                url))
      (signal 'adcal:exception-not-retrieved
              "The requested URL returned error"))))

;;------------------------------
;; Convert Function
;;------------------------------
(defun adcal:uri2rsspath (uri)
  "Convert advent calendar uri to atnd.org comment rss path

Example:
  (adcal:uri2rsspath \"http://atnd.org/events/21936\")
    ;; => \"http://atnd.org/comments/21936.rss\""
  (concat (replace-regexp-in-string "events" "comments" uri) ".rss"))

(defun adcal:remove-newline (str)
  (replace-regexp-in-string "\\s-*\n+\\s-*" " " str))

;;------------------------------
;; Parse Function
;;------------------------------
(defun adcal:parse-callback (func &optional buffer)
  (cond ((stringp buffer) (setq buffer (get-buffer buffer)))
        ((null buffer) (setq buffer adcal:buffer-name)))
  (save-current-buffer
    (set-buffer buffer)
    (goto-char (point-min))
    (funcall func)))

(defun adcal:parse-list (&optional buffer)
  "Advent Calendar のタイトルとリンクのコンスセルリストを返す。

`adcal:list-uri' の HTML 構造がなぜか
    - <table>..<tbody>...</table> となってて </tbody>が無い
    - <tr><td></td></tr><td></td></tr> みたいに <tr> が無い
みたいなことになってるので、そこらへん考慮していろいろ。

Example:
  (adcal:parse-list)
      ;; => ((\"Hoge Advent Calendar\" . \"http://atnd.org/events/xxxxx\")
             (\"Fuga Advent Calendar\" . \"http://atnd.org/events/yyyyy\")
             (\"Hago Advent Calendar\" . \"http://atnd.org/events/zzzzz\"))"
  (adcal:parse-callback
   (lambda ()
     (let (begin end html nodes list)
       (search-forward "<tbody>" nil t)
       (setq begin (point))
       (search-forward "</table>" nil t)
       (setq end (match-beginning 0))
       (setq html (buffer-substring-no-properties begin end))
       (setq list (with-temp-buffer
                    (insert html)
                    (goto-char (point-min))
                    (keep-lines "<td style=\"padding:0.5em 1em\">")
                    (goto-char (point-min)) (insert "<hoge>")
                    (goto-char (point-max)) (insert "</hoge>")
                    (xml-get-children
                     (car (xml-parse-region (point-min) (point-max))) 'td)))
       (mapcar (lambda (x)
                 (let* ((link (nth 2 x))
                        (href (xml-get-attribute link 'href))
                        (name (nth 2 link)))
                   (cons name href)))
               list)))
   buffer))

(defun adcal:parse-day-with-a (desc)
  "コメントに a タグがある場合、(comment . href) を返す。なければ nil

a タグが複数ある場合は、後方にあるやつを href として返します

Example:
  (adcal:parse-day-with-a \"aiueo <a href=\\\"http://example.com/\\\">12345</a> aiueo\")
    ;; => (\"aiueo 12345 aiueo\" . \"http://example.com/\")

  (adcal:parse-day-with-a \"aiueo 12345 aiueo\")
    ;; => nil"
  (condition-case err
      (let ((nodes (with-temp-buffer
                     (insert "<hoge>" desc "</hoge>")
                     (car (xml-parse-region (point-min) (point-max))))))
        (if (xml-get-children nodes 'a)
            (let (str link)
              (setq str (mapconcat
                         (lambda (x)
                           (cond ((stringp x) x)
                                 ((eq (xml-node-name x) 'a)
                                  (setq link (xml-get-attribute x 'href))
                                  (nth 2 x))))
                         (cddr nodes) ""))
              (cons (adcal:remove-newline str) link))
          nil))
    (error
     (message "!!Error コメント「" desc "」の解析に失敗しました。html が不完全かもね！")
     nil
     )))

(defun adcal:parse-day-with-uri (desc)
  "コメントに http/https がある場合、(comment . uri) を返す。なければ nil

Example:
  (adcal:parse-day-with-uri \"aiueo http://example.com aiueo\")
    ;; => (\"aiueo  aiueo\" . \"http://example.com/\")

  (adcal:parse-day-with-uri \"aiueo 12345 aiueo\")
    ;; => nil"
  (let ((uri-re "\\(?:\\'\\|^\\|\\s-\\)\\(\\(?:http\\|https\\):[^ \n]+\\)")
        href)
    (if (string-match uri-re desc)
        (progn
          (setq href (match-string 1 desc))
          (setq desc (replace-regexp-in-string (regexp-quote href) "" desc))
          (cons (adcal:remove-newline desc) href))
      nil)))

(defun adcal:parse-days (&optional buffer)
  (adcal:parse-callback
   (lambda ()
     (let (root items)
       (setq root (xml-parse-region (point-min) (point-max)))
       (setq items (xml-get-children (car (cdddar root)) 'item))
       (delq nil (mapcar
                  (lambda (item)
                    (let ((author (nth 2 (car (xml-get-children item 'author))))
                          (description (nth 3 (car (xml-get-children item 'description))))
                          comment)
                      (or (adcal:parse-day-with-a description)
                          (adcal:parse-day-with-uri description)
                          nil)
                      ))
                  items))
       ))
   buffer))

(defun adcal:get-list ()
  "`adcal:parse-list' をキャッシュして返す"
  (when (= (safe-length adcal:cache) 0)
    (adcal:http-get adcal:list-uri nil 'euc-japan)
    (setq adcal:cache (adcal:parse-list)))
  adcal:cache)

(defun adcal:get-days (rss)
  (adcal:http-get rss)
  (adcal:parse-days))

(defun adcal:view-calendar (uri)
  (if (string-match "atnd.org" uri)
      (let ((rss (adcal:uri2rsspath uri)))
        (kill-buffer anything-action-buffer)
        (anything
         `((name . "Advent Calendar")
           (candidates . ,(adcal:get-days rss))
           (action
            ("Open Browser" . (lambda (x) (browse-url x)))
            )
           )))
    (message "Sorry, only supported on the Advent Calendar in the atnd.org.")))

;;------------------------------
;; User Function
;;------------------------------
(defun adcal:anything ()
  (interactive)
  (anything
   `((name . "Advent Calendar")
     (candidates . ,(adcal:get-list))
     (action
      ("Open calendar on browser"  . (lambda (x) (browse-url x)))
      ("View calendar" . (lambda (x) (adcal:view-calendar x)))
      )
     (migemo)
     )))


(defun adcal:anything-refresh ()
  (interactive)
  (setq adcal:cache '())
  (adcal:anything))

(provide 'anything-advent-calendar)
