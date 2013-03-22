#lang racket
 
(require 
 db
 xml
 web-server/servlet
 web-server/servlet-env)

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
    (define entry-id (extract-binding/single 'entry_id bindings))
    (query-exec pgc "update entry set read=true where id=$1" (string->number entry-id))
    (response/xexpr
     `(id ,entry-id)
     #:mime-type #"application/xml")))

(define search 
  (lambda (req)
    (define bindings (request-bindings req))
    (define q (extract-binding/single 'q bindings))
    (response/xexpr
     `(results
       ,@(for/list (((blog-title entry-title url entry-date) 
		     (in-query pgc 
			       "select feed.title, entry.title, entry.url, entry.date from entry inner join feed on entry.feed_id = feed.id where lower(entry.title) ~ lower($1) or lower(entry.description) ~ lower($2) order by entry.date desc" q q)))
	  `(result
	    (blog_title ,blog-title)
	    (entry_date ,(str
			  (sql-timestamp-month entry-date) "/"
			  (sql-timestamp-day entry-date) "/"
			  (sql-timestamp-year entry-date)))
	    (entry_title ,entry-title)
	    (url ,url))))
     #:mime-type #"application/xml")))

(define retrieve-unread
  (lambda (req)
    (define bindings (request-bindings req))
    (define feed-id (extract-binding/single 'feed_id bindings))
    (response/xexpr
     `(results
       ,@(for/list (((title description url id)
		     (in-query pgc
			       "select title, description, url, id from entry where feed_id=$1 and read=false limit 50" 
			       (string->number feed-id))))
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
		      (in-query pgc "select title, url, id from feed order by title")))
	    (let ((unread-count (query-value pgc "select count(*) from entry where feed_id=$1 and read=false" id)))
	      `(p (a ((id ,(str "blog_title_" id)) (onclick ,(str "retrieve_unread(" id ")")) 
		      (href "javascript:void(0)")) ,(str title " (" unread-count ")"))
		(div ((style "display:none;border:solid 1px black") (id 
								     ,(str "results_" id))))))))))))
(define get-feed-title
  (lambda (req)
    (define bindings (request-bindings req))
    (define feed-id (extract-binding/single 'feed_id bindings))
    (define unread-count (query-value pgc "select count(*) from entry where feed_id=$1 and read=false" (string->number feed-id)))
    (define feed-title (query-value pgc "select title from feed where id=$1" (string->number feed-id)))
    (response/xexpr
     `(title ,(str feed-title " (" unread-count ")"))
     #:mime-type #"application/xml")))

(define search-page
  (lambda (req)
    (response/xexpr
     `(html 
      (body
	(input ((name "rss_search") (id "rss_search") (onkeydown "if (event.keyCode == 13) search();")(type "text") (size "20")))
	(button ((type "button") (onclick "search();")) "Search")
	(div ((id "results"))))))))

(define recent-entries 
  (lambda (req)
    (response/xexpr
     `(html
       (head
	(title "Shad's Reader")
	(script ((type "text/javascript")(src "rss.js")) " ")
	(link ((rel "stylesheet") (href "http://code.jquery.com/ui/1.10.2/themes/flick/jquery-ui.css")) " ")
	(script ((src "//ajax.googleapis.com/ajax/libs/jquery/1.9.1/jquery.min.js")) " ")
	(script (( src "//ajax.googleapis.com/ajax/libs/jqueryui/1.10.2/jquery-ui.min.js")) " ")
	(script "$(function() {$( '#tabs' ).tabs();});"))
       (body ((bgcolor "#5c9ccc") (style "background-color;#e5e5e5;"))
	     (div ((id "tabs") (style "width:850px;margin-left:auto;margin-right:auto;"))
		  (ul
		   (li (a ((href "#latest_items")) "Latest Items"))
		   (li (a ((href "search-page")) "Search"))
		   (li (a ((href "blog-list")) "Blog List")))
		  (div ((id "latest_items"))
		       ,@(for/list (((feed-title title link image-url date desc entry-id) 
				     (in-query pgc 
					       "select feed.title, entry.title, entry.url, feed.image_url, entry.date, entry.description, entry.id from entry inner join feed on feed.id = entry.feed_id where entry.date > (now () - interval '36 hour') and entry.read = false order by entry.date desc")))
			   `(p (div ,(string-append 
				      (number->string (sql-timestamp-month date)) "/"
				      (number->string (sql-timestamp-day date)) "/"
				      (number->string (sql-timestamp-year date)) " ")
				    (b ,feed-title)) (a ((href "javascript:void(0)") (onclick  ,(str "window.open('"  link "')"))) ,title)
				    (div (a ((href "javascript:void(0)") 
					     (id ,(string-append "toggle-" (number->string entry-id))) 
					     (onclick ,(string-append "flip('toggle-" (number->string entry-id) "', 'desc-" (number->string entry-id) "');")))"Show"))
				    (div ((style "display:none")(id ,(string-append "desc-" (number->string entry-id)))) 
					 ,(cdata 'cdata-start 'cdata-end desc)))))))))))

(define (start request)
  (rss-dispatch request))

(define-values (rss-dispatch rss-url)
  (dispatch-rules
   (("") recent-entries)
   (("mark-read") mark-read)
   (("search") search)
   (("blog-list") blog-list)
   (("search-page") search-page)
   (("get-feed-title") get-feed-title)
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
