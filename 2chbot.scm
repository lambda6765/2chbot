(define-module 2chbot
  (use rfc.uri)    ;; uri-encode-string
  (use rfc.http)   ;; http-post
  (use gauche.parameter)
  (use gauche.charconv)
  (use gauche.collection)
  (use gauche.sequence)
  (use util.list)
  (use util.match)
  (use srfi-1)
  (use srfi-13) ;; string-null?
  (use srfi-27) ;; random-integer
  (export-all)) 

(select-module 2chbot)

(define bbsmenu-host (make-parameter "menu.2ch.net"))
(define bbsmenu-path (make-parameter "/bbsmenu.html"))
(define user-agent   (make-parameter "Monazilla/1.00"))
(define cgi-path     (make-parameter "/test/bbs.cgi"))

;;;; class definition & MOP
(define-class <2ch-top> (<collection>)
  ((boards :init-value '()
           :init-keyword :boards)))

(define-method call-with-iterator ((top <2ch-top>) proc . args)
  (apply call-with-iterator (ref top 'boards) proc args))

(define-class <2ch-board> (<collection>)
  ((host :init-value #f
         :init-keyword :host)
   (board-key :init-value #f
              :init-keyword :board-key)
   (board-title :init-value #f
                :init-keyword :board-title)
   (threads :init-value '()
            :init-keyword :threads)))

(define-method object-apply ((obj <2ch-board>) . args)
  (apply ref* obj args))

(define-method call-with-iterator ((bd <2ch-board>) proc . args)
  (apply call-with-iterator (ref bd 'threads) proc args))

(define-class <2ch-thread> (<2ch-board>)
  ((thread-key :init-value #f
               :init-keyword :thread-key)
   (thread-title :init-value #f
                 :init-keyword :thread-title)
   (responses :init-value '() 
              :initkeyword :responses)))

(define-method call-with-iterator ((th <2ch-thread>) proc . args)
  (apply call-with-iterator (ref th 'responses) proc args))

(define-class <2ch-response> (<2ch-thread>)
  ((num :init-value #f
        :init-keyword :num)
   (name :init-value #f
         :init-keyword :name)
   (mail :init-value #f
         :init-keyword :mail)
   (date :init-value #f
         :init-keyword :date)
   (id :init-value #f
       :init-keyword :id)
   (time :init-value #f
         :init-keyword :time)
   (subject :init-value #f
            :init-keyword :subject)))

(define-class <2chbot> ()
  ((FROM :init-value ""
         :init-keyword :FROM) 
   (mail :init-value "sage"
         :init-keyword :mail) 
   (MESSAGE :init-value ""
            :init-keyword :MESSAGE)
   (cookie :init-value #f)
   (time :init-value "1"
         :init-keyword :time))) 

;; *.2ch.net or *.bbspink.com
(define (get-boards)
  (receive (stat _ html)
    (http-get (bbsmenu-host)
              (bbsmenu-path)
              :user-agent (user-agent))
    (cond
      ((not (string=? stat "200"))
       (error "could not get boards"))
      (else
        (let* ((lines (string-split (ces-convert html "*jp") "\n"))
               (boards
                 (fold
                   (lambda (line boards)
                     (let1 m (#/<A HREF=http:\/\/(.+2ch\.net|bbspink\.com)\/(.+)\/>(.+)<\/A>/
                              line)
                       (if m
                         (cons (make <2ch-board>
                                     :host (m 1)
                                     :board-key (m 2)
                                     :board-title (m 3))
                               boards)
                         boards)))
                   '() lines)))
          (make <2ch-top> :boards boards))))))

(define (not-string-null? str)
  (not (string-null? str)))

(define (get-subject host board-key)
  (receive (stat _ subject)
    (http-get host
              #`"/,|board-key|/subject.txt"
              :user-agent (user-agent))
    (if (not (string=? stat "200"))
      (error "could not get subject.txt")
      (ces-convert subject "*jp"))))

(define-method get-threads ((bd <2ch-board>))
  (let1 threads (get-threads (ref bd 'host) (ref bd 'board-key))
    (for-each
      (lambda (th)
        (set! (ref th 'board-title) (ref bd 'board-title)))
      threads)
    (set! (ref bd 'threads) threads)
    bd))

(define-method get-threads ((host <string>) (board-key <string>))
  (let1 subj (get-subject host board-key)
    (let1 key&title-alist 
      (map (cut string-split <> "<>")
           (filter not-string-null? (string-split subj"\n")))
      (map (match-lambda
             ((key title)
              (make <2ch-thread>
                    :host host
                    :board-key board-key
                    :thread-key ((#/(\d+)\.dat/ key) 1)
                    :thread-title title)))
           key&title-alist))))

(define (parse-dat dat)
  (map
    (lambda (res)
      (let1 res (string-split res "<>")
        (cond ((= (length res) 5)
               (let* ((date-etc (ref res 2))
                      (date? (#/\d\d\d\d\/\d\d\/\d\d\(.\)/ date-etc))
                      (id? (#/ID:(.{8,9})/ date-etc))
                      (time? (#/\d\d\:\d\d\:\d\d/ date-etc)))
                 (make <2ch-response>
                       :name (ref res 0)
                       :mail (ref res 1)
                       :date (if date? (date?) "")
                       :id   (if id? (id? 1) "")
                       :time (if time? (time?) "")
                       :subject (ref res 3))))
          (else
            (make <2ch-response>)))))
    (filter not-string-null? (string-split dat "\n"))))

(define (get-dat host board-key thread-key)
  (receive (stat _ dat)
    (http-get host 
            #`"/,|board-key|/dat/,|thread-key|.dat"
            :user-agent (user-agent))
    (if (not (string=? stat "200"))
      (error "could not get dat")
      (ces-convert dat "*jp"))))

(define-method get-responses ((th <2ch-thread>))
  (let1 responses (get-responses (ref th 'host) (ref th 'board-key) (ref th 'thread-key))
    (for-each
      (lambda (res)
        (set! (ref res 'board-title) (ref th 'board-title))
        (set! (ref res 'thread-title) (ref th 'thread-title)))
      responses)
    (set! (ref th 'responses) responses)
    th))

(define-method get-responses ((host <string>) (board-key <string>) (thread-key <string>))
  (let1 responses (parse-dat (get-dat host board-key thread-key))
    (map-with-index
      (lambda (i res)
           (set! (ref res 'host) host)
           (set! (ref res 'board-key) board-key)
           (set! (ref res 'thread-key) thread-key)
           (set! (ref res 'num) (+ i 1))
           res)
         responses)))

(define-method num-boards ((top <2ch-top>))
  (length (ref top 'boards)))

(define-method num-threads ((bd <2ch-board>))
  (length (ref bd 'threads)))

(define-method num-responses ((th <2ch-thread>))
  (length (ref th 'responses)))

(define-method random-board ((top <2ch-top>))
  (random-source-randomize! default-random-source)
  (let* ((bds (ref top 'boards))
         (len (length bds)))
    (ref bds (random-integer len))))

(define-method random-thread ((bd <2ch-board>))
   (random-source-randomize! default-random-source)
   (let* ((ths (ref bd 'threads))
          (len (length ths)))
     (ref ths (random-integer len))))

(define-method random-response ((th <2ch-thread>))
   (random-source-randomize! default-random-source)
   (let* ((rs (ref th 'responses))
          (len (length rs)))
     (ref rs (random-integer len))))

(define-method generate-body ((bot <2chbot>)
                              (board-key <string>)
                              (thread-key <string>)) 
    (format "bbs=~a&key=~a&time=~a&submit=~a&FROM=~a&mail=~a&MESSAGE=~a"
            board-key
            thread-key
            (ref bot 'time)
            (uri-encode-string "書き込む" :encoding 'sjis)
            (uri-encode-string (ref bot 'FROM) :encoding 'sjis)
            (uri-encode-string (ref bot 'mail) :encoding 'sjis)
            (uri-encode-string (ref bot 'MESSAGE) :encoding 'sjis)))

(define-method set-cookie! ((bot <2chbot>)
                            (host <string>)
                            (board-key <string>)
                            (thread-key <string>))
  (set! (ref bot 'cookie) #f)
  (receive (stat header _) (2chbot-post bot host board-key thread-key)
    (if (not (string=? stat "200"))
      (error "could not set cookie")
      (set-cookie! bot header))))

(define-method set-cookie! ((bot <2chbot>) (header <list>))
  (let ((pon #f)
        (hap #f))
    (for-each
      (lambda (str)
        (cond ((#/PON=(.+?);/ str)
                => (lambda (m)
                     (set! pon (m 1))))
              ((#/HAP=(.+?);/ str)
                => (lambda (m)
                     (set! hap (m 1))))))
      (map cadr header))
    (let1 cookie 
        (format "NAME=~a; MAIL=~a; PON=~a; HAP=~a; path=/; tepo=don"
                (uri-encode-string (ref bot 'FROM) :encoding 'sjis)
                (uri-encode-string (ref bot 'mail) :encoding 'sjis)
                (uri-encode-string pon)
                (uri-encode-string hap))
        (set! (ref bot 'cookie) cookie))))

(define-method 2chbot-post ((bot <2chbot>)
                           (host <string>)
                           (board-key <string>)
                           (thread-key <string>))
  (if (ref bot 'cookie)
    (http-post host
               (cgi-path)
               (generate-body bot board-key thread-key)
               :user-agent (user-agent)
               :referer #`"http://,|host|/,|board-key|/"
               :connection "close"
               :cookie (ref bot 'cookie))
    (http-post host
               (cgi-path)
               (generate-body bot board-key thread-key)
               :user-agent (user-agent)
               :referer  #`"http://,|host|/,|board-key|/"
               :connection "close")))

(define-method 2chbot-post ((bot <2chbot>) (th <2ch-thread>))
  (2chbot-post bot (ref th 'host) (ref th 'board-key) (ref th 'thread-key)))

(define (respond-success? body)
  (let1 body (ces-convert body "*jp")
    (or (#/<title>書きこみました<\/title>/ body)
        (#/<!-- 2ch_X:true -->|<!-- 2ch_X:false -->/ body))))

(define-method 2chbot-respond ((bot <2chbot>)
                              (host <string>)
                              (board-key <string>)
                              (thread-key <string>)
                              . rest)
  (let1 sleep (get-keyword :sleep rest #f)
    (receive (stat header body) (2chbot-post bot host board-key thread-key)
      (cond 
        ((respond-success? body) (values stat header body))
        (else
          (set-cookie! bot header)
          (if sleep 
            (if (integer? sleep)
              (sys-sleep sleep)
              (error "integer required, but got:" sleep)))
          (2chbot-post bot host board-key thread-key))))))

(define-method 2chbot-respond ((bot <2chbot>)
                              (th <2ch-thread>)
                              . rest)
  (apply 2chbot-respond bot (ref th 'host) (ref th 'board-key) (ref th 'thread-key) rest))

;; Utilities
(define (read-sexp-from-file fname . opts)
  (apply call-with-input-file fname read :encoding "*jp" opts))

(define-method respond-to ((host <string>)
                           (board-key <string>)
                           (thread-key <string>)
                           (res <list>)
                           . rest)
  (let1 bot (apply make <2chbot> res)
    (apply 2chbot-respond bot host board-key thread-key rest)))

(define-method respond-to ((th <2ch-thread>) res . rest)
  (apply respond-to (ref th 'host) (ref th 'board-key) (ref th 'thread-key) res rest))

(define-method respond-to ((url <string>)
                           (res <list>)
                           . rest)
  (let1 m (#/(?:http:\/\/)?(.+)\/test\/read.cgi\/(.+)\/(\d+)/ url)
    (if m
      (apply respond-to (m 1) (m 2) (m 3) res rest)
      (error "invalid url:" url))))

;; res = (:FROM  "name"
;;        :mail  "adress"
;;        :MESSAGE "message" ...)

(define-method thread-url ((th <2ch-thread>))
  (format "http://~a/test/read.cgi/~a/~a/"
          (ref th 'host) (ref th 'board-key) (ref th 'thread-key)))

(provide "2chbot")

