#lang racket
(require 
 db
 net/url
 xml
 xml/path
 file/md5
 net/uri-codec
 web-server/servlet
 web-server/servlet-env
 web-server/formlets
 net/cookie
 web-server/http/cookie
 web-server/http/cookie-parse
 "db-lib.rkt")

(provide/contract (start (request? . -> . response?)))

(define pgc
  (postgresql-connect #:user "feed"
		      #:database "feed"
		      #:password "abc123"))

(define *user-id* 0)

(define get-user-id 
  (lambda (username)
    (vector-ref (get-rssuser pgc username) 3)))

; string-append replacement macro
(define-syntax str
  (syntax-rules ()
    ((_) "")
    ((_ string1 . rest)
     (cond
       ((empty? string1)
	(string-append "" (str . rest)))
       ((number? string1)
	(string-append (number->string string1) (str  . rest)))
       (else
	(string-append string1 (str . rest)))))))

(define user-formlet
  (formlet
   (table
    (tr (td "Username:")
        (td ,{input-string . => . username}))
    (tr (td "Password:")
        (td ,{(to-string (required (password-input))) . => . password})))
   (list username password)))

(define validate-user
  (lambda (req)
    (let* ((username (car (formlet-process user-formlet req)))
	   (password (second (formlet-process user-formlet req)))
	   (cookieid (number->string (random 4294967087)))
	   (query-vector (get-rssuser pgc username))
	   (id-cookie (make-cookie "id" (string-append  (form-urlencoded-decode username) "-" cookieid) #:secure? #t)))
      (cond
       ((= 0 (vector-length query-vector))
	(redirect-to "/")) ;;bad username
       ((not (string=? (bytes->string/latin-1 (md5 password)) (vector-ref query-vector 0)))
	(redirect-to "/")) ;;bad password
       (else
	(query-exec pgc "update rssuser set cookieid=$1 where username=$2" cookieid username)
	(set! *user-id* (vector-ref query-vector 3))
	(redirect-to "home"
		     see-other
		     #:headers (list (cookie->header id-cookie))))))))

(define login-page
  (lambda (req)
    (cond
     ((eq? (check-cookie req) #t)
      (redirect-to "home"))
     (else
      (response/xexpr  
       `(html
	 (head 
	  (link ((rel "stylesheet") (href "jquery-ui.css")) " ")
	  (link ((rel "stylesheet") (href "rss.css")) " ")
	  (script ((src "jquery-1.9.1.min.js")) " ")
	  (script ((src "jquery-ui.min.js"))" ")
	  (script "$function() {$('input[type=submit]').button();}"))
	 (body
	  ((style "background-color;#e5e5e5;"))
	  (div ((class "centered"))
	       (form ((action "validate-user"))
		     ,@(formlet-display user-formlet)
		     (input ((type "submit"))))))))))))

(define mark-read
  (lambda (req)
    (define bindings (request-bindings req))
    (define item-id (extract-binding/single 'item_id bindings))
    (query-exec pgc "insert into read_item(item_id, rssuser_id) values ($1,$2)" (string->number item-id) *user-id*)
    (response/xexpr
     `(id ,item-id)
     #:mime-type #"application/xml")))

(define check-url
  (lambda (req)
    (define bindings (request-bindings req))
    (define url (extract-binding/single 'url bindings))
    (define url-string (string->url (regexp-replace #rx"^[ ]+" url "")))
    (define in (get-pure-port url-string))
    (define response-string (port->string in))
    (close-input-port in)
    (define rss-xexpr (xml->xexpr
		       (document-element
			(read-xml (open-input-string
				   response-string)))))
    (define description 
      (cond
       ((eq? (se-path* '(description) rss-xexpr) #f)
	(se-path* '(subtitle) rss-xexpr)
	)
       (else
	(se-path* '(description) rss-xexpr))))
    (response/xexpr
     `(site
       (title ,(se-path* '(title) rss-xexpr))
       (desc ,description))
     #:mime-type #"application/xml")))

(define add-feed
  (lambda (req)
    (define bindings (request-bindings req))
    (define link (extract-binding/single 'link bindings))
    (define title (extract-binding/single 'title bindings))
    (define username (extract-binding/single 'username bindings))
    (define feed-list (query-rows pgc "select id from feed where url=$1" link))
    (cond
     ((= *user-id* 0)
      (set! *user-id* (get-user-id username))
      ))
    (cond
     ((empty? feed-list)
      (define insert-vector (query-row pgc "insert into feed(title, url) values ($1,$2) returning id" title link))
      (display "id : ")
      (displayln (vector-ref insert-vector 0))
      (query-exec pgc "insert into rssuser_feed(feed_id, rssuser_id) values ($1,$2);" (vector-ref insert-vector 0) *user-id*))
     (else
      (query-exec pgc "insert into rssuser_feed(feed_id, rssuser_id) values ($1,$2);" (vector-ref (car feed-list) 0) *user-id*)))
    (response/xexpr
     `(insert
       (title ,title)
       (link ,link))
     #:mime-type #"application.xml")))

(define search 
  (lambda (req)
    (define bindings (request-bindings req))
    (define q (extract-binding/single 'q bindings))
    (response/xexpr
     `(results
       ,@(for/list (((blog-title item-title url item-date) (search-items pgc q *user-id*)))
	  `(result
	    (blog_title ,blog-title)
	    (item_date ,(str
			  (sql-timestamp-month item-date) "/"
			  (sql-timestamp-day item-date) "/"
			  (sql-timestamp-year item-date)))
	    (item_title ,item-title)
	    (url ,url))))
     #:mime-type #"application/xml")))

(define retrieve-unread
  (lambda (req)
    (define bindings (request-bindings req))
    (define feed-id (extract-binding/single 'feed_id bindings))
    (response/xexpr
     `(results
       ,@(for/list (((title description url id)
		     (fetch-unread-items pgc feed-id (number->string  *user-id*))))
	   `(result
	     (title ,title)
	     (description ,description)
	     (url ,url)
	     (id ,(number->string id))))))))

(define blog-list
  (lambda (req)
    (response/xexpr
     `(html
       (body
	,@(for/list (((title url id)
		     (get-feed-list pgc *user-id*)))
	    (let ((unread-count 
		   (get-unread-count pgc (number->string id) *user-id*)))
	      `(p (a ((id ,(str "blog_title_" id)) (onclick ,(str "retrieve_unread(" id ")")) 
		      (href "javascript:void(0)")) ,(str title " (" unread-count ")"))
		(div ((style "display:none;border:solid 1px black") 
		      (id ,(str "results_" id))))))))))))

(define get-feed-title
  (lambda (req)
    (define bindings (request-bindings req))
    (define feed-id (extract-binding/single 'feed_id bindings))
    (define unread-count (get-unread-count pgc feed-id *user-id*))
    (define feed-title (fetch-feed-title pgc feed-id))
    (response/xexpr
     `(title ,(str feed-title " (" unread-count ")"))
     #:mime-type #"application/xml")))

(define search-page
  (lambda (req)
    (response/xexpr
     `(html 
       (head (script "$('button').button();"))
       (body
	(input ((name "rss_search") (id "rss_search") (onkeydown "if (event.keyCode == 13) search();")
		(type "text") (size "20")))
	(button ((type "button") (onclick "search();")) "Search")
	(div ((id "results"))))))))

(define check-cookie
  (lambda (req)
    (define cookies (request-cookies req))
    (define id-cookie
      (findf (lambda (c)
               (string=? "id" (client-cookie-name c)))
             cookies))
    (if id-cookie
	(begin
	  (let* 
	      ((username (car (regexp-split #rx"-" (client-cookie-value id-cookie))))
	       (cookieid (second (regexp-split #rx"-" (client-cookie-value id-cookie))))
	       (query-vector (get-rssuser pgc username)))
	    (displayln 
	     (str "query-vector : "
		  (vector-ref query-vector 1) " : "
		  (vector-ref query-vector 2) " : "
		  (vector-ref query-vector 3)
		  ))
	    (cond
	     ((equal? cookieid (vector-ref query-vector 2)) 
	      (begin
		(set! *user-id* (vector-ref query-vector 3)) 
		#t))
	     (else #f))))
	#f)))

(define recent-items
  (lambda (req)
    (define cookies (request-cookies req))
    (define id-cookie
      (findf (lambda (c)
               (string=? "id" (client-cookie-name c)))
             cookies))
    (if (eq? (check-cookie req) #t)
	(let* 
	    ((username (car (regexp-split #rx"-" (client-cookie-value id-cookie))))
	     (cookieid (second (regexp-split #rx"-" (client-cookie-value id-cookie))))
	     (query-vector (get-rssuser pgc username)))
	  (cond
	   ((equal? cookieid (vector-ref query-vector 2))
	    (response/xexpr
	     `(html
	       (head
		(title "Shad's Reader")
		(script ((type "text/javascript")(src "rss.js")) " ")
		(link ((rel "stylesheet") (href "jquery-ui.css")) " ")
		(link ((rel "stylesheet") (href "rss.css")) " ")
		(script ((src "jquery-1.9.1.min.js")) " ")
		(script ((src "jquery-ui.min.js"))" ")
		(script "$(function() {$( '#tabs' ).tabs();});main_init();"))
	       (body ((style "background-color;#e5e5e5;"))
		     (div ((id "tabs") (class "centered"))
			  (ul
			   (li (a ((href "#latest_items")) "Latest Items"))
			   (li (a ((href "search-page")) "Search"))
			   (li (a ((href "blog-list")) "Blog List")))
			  (div ((id "latest_items"))
			       (button ((id "opener")) "Subscribe")
			       (div ((id "subscribe_dialog") (title "subscribe"))
				    (table 
				     (tr 
				      (td 
				       (input ((name "feed_link") (id "feed_link") 
					       (onkeydown "if (event.keyCode == 13) search();")
					       (type "text") (size "24"))))
				      (td 
				       (button ((type "button") (onclick ,(str "check_url('"username"',$('#feed_link').val());"))) "Subscribe"))))
				    (div ((id "subscribe_results"))))
			       ,@(for/list (((feed-title title link date desc item-id) 
					     (get-item pgc *user-id*)))
				   `(p (div 
					(a ((href "javascript:void(0)") (class "item_title") 
					    (id ,(string-append "toggle-" (number->string item-id))) 
					    (onclick ,(str "flip('desc-" item-id "');"))) 
					   ,(str
					     (sql-timestamp-month date) "/"
					     (sql-timestamp-day date) "/"
					     (sql-timestamp-year date) " " feed-title)))
				       (a ((href "javascript:void(0)") (onclick  ,(str "window.open('"  link "')"))) ,title)
				       (div ((style "display:none")(id ,(string-append "desc-" (number->string item-id)))) 
					    ,(cdata 'cdata-start 'cdata-end desc))))))))))
	   (else (redirect-to "/"))))
	(redirect-to "/"))))

(define (start request)
  (rss-dispatch request))

(define-values (rss-dispatch rss-url)
  (dispatch-rules
   (("home") recent-items)
   (("") login-page)
   (("mark-read") mark-read)
   (("search") search)
   (("blog-list") blog-list)
   (("search-page") search-page)
   (("get-feed-title") get-feed-title)
   (("check-url") check-url)
   (("add-feed") add-feed)
   (("validate-user") validate-user)
   (("retrieve-unread") retrieve-unread)))

(serve/servlet start
	       #:launch-browser? #f
	       #:listen-ip #f
	       #:quit? #f
	       #:ssl? #t
	       #:port 8000
	       #:servlet-regexp #rx""
	       #:ssl-cert "server-cert.pem"
	       #:ssl-key "private-key.pem"
	       #:extra-files-paths (list 
				    (build-path "./htdocs"))
	       #:servlet-path "")
