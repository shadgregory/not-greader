#lang racket
(require 
 db
 net/url
 xml
 xml/path
 web-server/servlet
 web-server/servlet-env
 "db-lib.rkt")

(provide/contract (start (request? . -> . response?)))

(define pgc
  (postgresql-connect #:user "feed"
		      #:database "feed"
		      #:password "abc123"))

;; string-append replacement
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

(define mark-read
  (lambda (req)
    (define bindings (request-bindings req))
    (define item-id (extract-binding/single 'item_id bindings))
    (query-exec pgc "update item set read=true where id=$1" (string->number item-id))
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
    (response/xexpr
     `(site
       (title ,(se-path* '(title) rss-xexpr))
       (desc ,(se-path* '(description) rss-xexpr)))
     #:mime-type #"application/xml")))

(define add-feed
  (lambda (req)
    (define bindings (request-bindings req))
    (define link (extract-binding/single 'link bindings))
    (define title (extract-binding/single 'title bindings))
    (query-exec pgc "insert into feed(title, url) values ($1,$2);" title link)
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
       ,@(for/list (((blog-title item-title url item-date) (search-items pgc q)))
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
		     (fetch-unread-items pgc feed-id)))
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
		     (get-feed-list pgc)))
	    (let ((unread-count 
		   (get-unread-count pgc (number->string id))))
	      `(p (a ((id ,(str "blog_title_" id)) (onclick ,(str "retrieve_unread(" id ")")) 
		      (href "javascript:void(0)")) ,(str title " (" unread-count ")"))
		(div ((style "display:none;border:solid 1px black") (id 
								     ,(str "results_" id))))))))))))

(define get-feed-title
  (lambda (req)
    (define bindings (request-bindings req))
    (define feed-id (extract-binding/single 'feed_id bindings))
    (define unread-count (get-unread-count pgc feed-id))
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

(define recent-items
  (lambda (req)
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
       (body ((bgcolor "#5c9ccc") (style "background-color;#e5e5e5;"))
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
			       (button ((type "button") (onclick "check_url($('#feed_link').val());")) "Subscribe"))))
			    (div ((id "subscribe_results"))))
		       ,@(for/list (((feed-title title link image-url date desc item-id) 
				     (get-item pgc)))
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
				    ,(cdata 'cdata-start 'cdata-end desc)))))))))))

(define (start request)
  (rss-dispatch request))

(define-values (rss-dispatch rss-url)
  (dispatch-rules
   (("") recent-items)
   (("mark-read") mark-read)
   (("search") search)
   (("blog-list") blog-list)
   (("search-page") search-page)
   (("get-feed-title") get-feed-title)
   (("check-url") check-url)
   (("add-feed") add-feed)
   (("retrieve-unread") retrieve-unread)))

(serve/servlet start
	       #:launch-browser? #f
	       #:listen-ip #f
	       #:quit? #f
	       #:ssl? #f
	       #:port 8000
	       #:servlet-regexp #rx""
	       #:extra-files-paths (list 
				    (build-path "./htdocs"))
	       #:servlet-path "")
