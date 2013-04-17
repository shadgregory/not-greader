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

(define db-conn (virtual-connection 
		 (connection-pool
		  (lambda ()
		    (postgresql-connect #:user "feed"
					#:database "feed"
					#:password "abc123")))))

(define *user-id* 0)

(define get-user-id 
  (lambda (username)
    (vector-ref (get-rssuser db-conn username) 3)))

;; string-append replacement macro
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

;;this macro checks the user's cookie before rendering the page
(define-syntax-rule (define-page (id req) . body)
  (begin
    (define id 
      (lambda (req)
	(define cookies (request-cookies req))
	(define id-cookie
	  (findf (lambda (c)
		   (string=? "id" (client-cookie-name c)))
		 cookies))
	(if (eq? (check-cookie req) #t)
	    (begin . body)
	    (response/full 401 #"Unauthorized" (current-seconds) TEXT/HTML-MIME-TYPE 
		      (list (make-header #"Refresh" #"4; /")) 
		      (list #"<html><body><p>There was an authentication problem</p></body></html>")))))))

(define user-formlet
  (formlet
   (table
    (tr (td "Username:")
        (td ,{input-string . => . username}))
    (tr (td "Password:")
        (td ,{(to-string (required (password-input))) . => . password})))
   (list username password)))

(define logout
  (lambda (req)
    (redirect-to "/"
		 see-other
		 #:headers (list (cookie->header 
				  (make-cookie "id" "" 
					       #:max-age 0 
					       #:secure? #t))))))

(define validate-user
  (lambda (req)
    (let* ((username (car (formlet-process user-formlet req)))
	   (password (second (formlet-process user-formlet req)))
	   (cookieid (number->string (random 4294967087)))
	   (query-vector (get-rssuser db-conn username))
	   (id-cookie (make-cookie "id" (string-append  (form-urlencoded-decode username) "-" cookieid) #:secure? #t)))
      (cond
       ((= 0 (vector-length query-vector))
	(redirect-to "/")) ;;bad username
       ((not (string=? (bytes->string/latin-1 (md5 password)) (vector-ref query-vector 0)))
	(redirect-to "/")) ;;bad password
       (else
	(query-exec db-conn "update rssuser set cookieid=$1 where username=$2" cookieid username)
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
	  (script "$(function() {$('input[type=submit]').button();});"))
	 (body
	  ((style "background-color;#e5e5e5;"))
	  (div ((class "centered"))
	       (form ((action "validate-user"))
		     ,@(formlet-display user-formlet)
		     (input ((type "submit"))))))))))))

(define-page (mark-read req)
    (let* ((bindings (request-bindings req))
	   (item-id (extract-binding/single 'item_id bindings)))
      (query-exec db-conn "insert into read_item(item_id, rssuser_id) values ($1,$2)" (string->number item-id) *user-id*)
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
	(se-path* '(subtitle) rss-xexpr))
       (else
	(se-path* '(description) rss-xexpr))))
    (response/xexpr
     `(site
       (title ,(se-path* '(title) rss-xexpr))
       (desc ,description))
     #:mime-type #"application/xml")))

(define-page (add-feed req)
  (let* ((bindings (request-bindings req))
	 (link (extract-binding/single 'link bindings))
	 (title (extract-binding/single 'title bindings))
	 (username (extract-binding/single 'username bindings))
	 (feed-list (query-rows db-conn "select id from feed where url=$1" link)))
    (cond
     ((= *user-id* 0)
      (set! *user-id* (get-user-id username))))
    (cond
     ((empty? feed-list)
      (define insert-vector (query-row db-conn "insert into feed(title, url) values ($1,$2) returning id" title link))
      (query-exec db-conn "insert into rssuser_feed(feed_id, rssuser_id) values ($1,$2);" 
		  (vector-ref insert-vector 0) *user-id*))
     (else
      (query-exec db-conn "insert into rssuser_feed(feed_id, rssuser_id) values ($1,$2);" 
		  (vector-ref (car feed-list) 0) *user-id*)))
    (response/xexpr
     `(insert
       (title ,title)
       (link ,link))
     #:mime-type #"application.xml")))

(define-page (search req)
  (let* ((bindings (request-bindings req))
	 (feed-id (extract-binding/single 'feed bindings))
	 (q (extract-binding/single 'q bindings)))
    (response/xexpr
     `(results
       ,@(for/list (((blog-title item-title url item-date item-id star-id) 
		     (search-items db-conn q (string->number feed-id) *user-id*)))
	   `(result
	     (blog_title ,blog-title)
	     (item_id ,(number->string item-id))
	     (star ,(if (sql-null? star-id) "F" "T"))
	     (item_date ,(str
			  (sql-timestamp-month item-date) "/"
			  (sql-timestamp-day item-date) "/"
			  (sql-timestamp-year item-date)))
	     (item_title ,item-title)
	     (url ,url))))
     #:mime-type #"application/xml")))

(define mark-all-read
  (lambda (req)
    (define bindings (request-bindings req))
    (define feed-id (extract-binding/single 'feed_id bindings))

    (for/list (((title description url date id star-id)
		  (fetch-unread-items db-conn feed-id (number->string  *user-id*))))
      (query-exec db-conn "insert into read_item(item_id,rssuser_id) values($1,$2)"
		  id *user-id*))
    (response/xexpr
     `(feed_id ,feed-id)
     #:mime-type #"application/xml")))

(define star-item
  (lambda (req)
    (define bindings (request-bindings req))
    (define item-id (extract-binding/single 'item_id bindings))
    (query-exec db-conn "insert into star_item(item_id, rssuser_id) values ($1, $2)" (string->number item-id) *user-id*)
    (response/xexpr
     `(star 
       (item_id ,item-id))
     #:mime-type #"application/xml")))

(define unstar-item
  (lambda (req)
    (define bindings (request-bindings req))
    (define item-id (extract-binding/single 'item_id bindings))
    (query-exec db-conn "delete from star_item where item_id=$1 and rssuser_id=$2" (string->number item-id) *user-id*)
    (response/xexpr
     `(unstar 
       (item_id ,item-id))
     #:mime-type #"application/xml")))

(define retrieve-unread
  (lambda (req)
    (define bindings (request-bindings req))
    (define feed-id (extract-binding/single 'feed_id bindings))
    (response/xexpr
     `(results
       ,@(for/list (((title description url date id star-id)
		     (fetch-unread-items db-conn feed-id (number->string  *user-id*))))
	   `(result
	     (title ,title)
	     (description ,description)
	     (url ,url)
	     (star ,(if (sql-null? star-id) "F" "T"))
	     (date ,(str
		     (sql-timestamp-month date) "/"
		     (sql-timestamp-day date) "/"
		     (sql-timestamp-year date)))
	     (id ,(number->string id))))))))

(define star-page
  (lambda (req)
    (response/xexpr
     `(html
       (body
	,@(for/list (((title description url date id)
		      (fetch-star-items db-conn (number->string  *user-id*))))
	    `(p  (span ((onclick ,(str "mark_star(" id ")")) (id ,(str "star_" id)) (class "ui-state-highlight ui-corner-all"))
		       (span ((class "ui-icon ui-icon-star") (style "display:inline-block"))))
		 (a ((href "javascript:void(0);") (onclick ,(str "window.open('" url "');"))) ,title))))))))

(define blog-list
  (lambda (req)
    (response/xexpr
     `(html
       (head (script "main_init();"))
       (body
	,@(for/list (((title url id)
		      (get-feed-list db-conn *user-id*)))
	    (let ((unread-count 
		   (get-unread-count db-conn (number->string id) *user-id*)))
	      `(p 
		(a ((id "mark_all") (class "mark_all") (href "javascript:void(0);") (onclick ,(str "mark_all_read(" id "," *user-id* ")")))
		   "Mark all as read")nbsp
		   (a ((id ,(str "blog_title_" id)) (onclick ,(str "retrieve_unread(" id ")"))
		       (href "javascript:void(0)")) ,(str title " (" unread-count ")"))
		   (div ((style "display:none;border:solid 1px black")
			 (id ,(str "results_" id))))))))))))

(define get-feed-title
  (lambda (req)
    (define bindings (request-bindings req))
    (define feed-id (extract-binding/single 'feed_id bindings))
    (define unread-count (get-unread-count db-conn feed-id *user-id*))
    (define feed-title (fetch-feed-title db-conn feed-id))
    (response/xexpr
     `(title ,(str feed-title " (" unread-count ")"))
     #:mime-type #"application/xml")))

(define search-page
  (lambda (req)
    (response/xexpr
     `(html 
       (head (script "$('#search_button').button({icons:{primary:'ui-icon-search'},text:false,label:'Search'});"))
       (body
	(input ((name "rss_search") (id "rss_search") (onkeydown "if (event.keyCode == 13) search();")
		(type "text") (size "24")))
	(select ((name "feed_select") (id "feed_select"))
		(option ((value "0")) "All")
	,@(for/list (((title id)
		      (in-query db-conn "select title,id from feed order by title")
		      ))
	    `(option ((value ,(number->string id))) ,title)))
	(button ((type "button") (id "search_button") (onclick "search();")))
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
	       (query-vector (get-rssuser db-conn username)))
	    (cond
	     ((equal? cookieid (vector-ref query-vector 2)) 
	      (begin
		(set! *user-id* (vector-ref query-vector 3)) 
		#t))
	     (else #f))))
	#f)))

(define recent-items
  (lambda (req)
    (define bindings (request-bindings req))
    (define username (extract-binding/single 'username bindings))
    (response/xexpr
     `(html 
       (head (script "main_init();"))
       (body
	(div ((id "latest_items"))
	     (button ((id "opener")) "Subscribe")
	     (div ((id "subscribe_dialog") (title "Subscribe"))
		  (table 
		   (tr 
		    (td 
		     (input ((name "feed_link") (id "feed_link") 
			     (onkeydown "if (event.keyCode == 13) search();")
			     (type "text") (size "24"))))
		    (td 
		     (button ((type "button") (id "subscribe_button") (onclick ,(str "check_url('"username"',$('#feed_link').val());"))) "Subscribe"))))
		  (div ((id "subscribe_results"))))
	     ,@(for/list (((feed-title title link date desc item-id star-id) 
			   (get-item db-conn *user-id*)))
		 `(p (div
		      (span ((onclick ,(str "mark_star(" item-id ")")) 
			     (id ,(str "star_" item-id)) 
			     (class 
			       ,(cond ((sql-null? star-id) 
				       "ui-state-default")
				      (else "ui-state-highlight"))))
		       (span ((class "ui-icon ui-icon-star") (style "display:inline-block"))))
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

(define-page (home req)
	(let* 
	    ((cookies (request-cookies req))
	     (id-cookie
	       (findf (lambda (c)
			(string=? "id" (client-cookie-name c)))
		      cookies))
	     (username (car (regexp-split #rx"-" (client-cookie-value id-cookie))))
	     (cookieid (second (regexp-split #rx"-" (client-cookie-value id-cookie))))
	     (query-vector (get-rssuser db-conn username)))
	  (cond
	   ((equal? cookieid (vector-ref query-vector 2))
	    (response/xexpr
	     `(html
	       (head
		(title "Shad's Reader")
		(link ((rel "stylesheet") (href "jquery-ui.css")) " ")
		(link ((rel "stylesheet") (href "rss.css")) " ")
		(script ((src "jquery-1.9.1.min.js"))" ")
		(script ((src "jquery-ui.min.js"))" ")
		(script ((type "text/javascript")(src "rss.js")) " ")
		(script "$(function() {$( '#tabs' ).tabs();});main_init();"))
	       (body ((style "background-color;#e5e5e5;"))
		     (div ((id "tabs") (class "centered"))
			   (div ((style "border:solid 1px black;text-align:right;padding:5px;"))
			        (a ((href "logout")) ,(str "Logout " username)))
			  (ul
			   (li (a ((href ,(str "recent-items?username=" username))) "Latest Items"))
			   (li (a ((href "search-page")) "Search"))
			   (li (a ((href "blog-list")) "Blog List"))
			   (li (a ((href "star-page")) "Starred"))
			   ))))))
	   (else (redirect-to "/")))))

(define (start request)
  (rss-dispatch request))

(define-values (rss-dispatch rss-url)
  (dispatch-rules
   (("home") home)
   (("recent-items") recent-items)
   (("") login-page)
   (("mark-read") mark-read)
   (("mark-all-read") mark-all-read)
   (("search") search)
   (("blog-list") blog-list)
   (("search-page") search-page)
   (("get-feed-title") get-feed-title)
   (("check-url") check-url)
   (("add-feed") add-feed)
   (("logout") logout)
   (("validate-user") validate-user)
   (("star-item") star-item)
   (("star-page") star-page)
   (("unstar-item") unstar-item)
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
	       #:log-file "rss.log"
	       #:extra-files-paths (list 
				    (build-path "./htdocs"))
	       #:servlet-path "")
