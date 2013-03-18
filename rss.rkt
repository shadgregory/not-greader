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
					;select feed.title, entry.title, entry.url from entry inner join 
					;feed on feed.id = entry.feed_id where entry.description like '%emacs%' or entry.title like '%emacs%';
    (define bindings (request-bindings req))
    (define q (extract-binding/single 'q bindings))
    (response/xexpr
     `(results
       ,@(for/list (((blog-title entry-title url) (in-query pgc 
							    (string-append "select feed.title, entry.title, entry.url from entry inner join feed on entry.feed_id = feed.id where entry.title like '%" q "%' or entry.description like '%" q "%' order by entry.date"))))
	  `(result
	    (blog_title ,blog-title)
	    (entry_title ,entry-title)
	    (url ,url))))
     #:mime-type #"application/xml")))

(define search-page
  (lambda (req)
    (response/xexpr
     `(html
       (head
	(script ((src "//ajax.googleapis.com/ajax/libs/jquery/1.9.1/jquery.min.js"))" ")
	(script ((type "text/javascript")(src "rss.js")) " "))
       (body
	(input ((name "rss_search") (id "rss_search") (type "text") (size "20")))
	(button ((type "button") (onclick "search();")) "Search")
	(div ((id "results"))))))))

(define recent-entries 
  (lambda (req)
    (response/xexpr
     `(html
       (head
	(script ((type "text/javascript")(src "rss.js")) " ")
	(link ((rel "stylesheet") (href "http://code.jquery.com/ui/1.10.2/themes/start/jquery-ui.css")) " ")
	(script ((src "//ajax.googleapis.com/ajax/libs/jquery/1.9.1/jquery.min.js")) " ")
	(script (( src "//ajax.googleapis.com/ajax/libs/jqueryui/1.10.2/jquery-ui.min.js")) " ")
	(script "$(function() {$( '#tabs' ).tabs();});"))
       (body ((bgcolor "e5e5e5"))
	     (div ((id "tabs") (style "width:850px;margin-left:auto;margin-right:auto;"))
		  (ul
		   (li (a ((href "#blog_roll")) "Blog Roll"))
		   (li (a ((href "search-page")) "Search")))
		  (div ((id "blog_roll"))
		       ,@(for/list (((feed-title title link image-url date desc entry-id) 
				     (in-query pgc 
					       "select feed.title, entry.title, entry.url, feed.image_url, entry.date, entry.description, entry.id from entry inner join feed on feed.id = entry.feed_id where entry.date > (now () - interval '48 hour') and entry.read = false order by entry.date desc")))
			   `(p (div ,(string-append 
				      (number->string (sql-timestamp-month date)) "/"
				      (number->string (sql-timestamp-day date)) "/"
				      (number->string (sql-timestamp-year date))
				      " ")
				    (b ,feed-title)) (a ((href ,link)) ,title)
				    (div (a ((href "javascript:void(0)") 
					     (id ,(string-append "toggle-" (number->string entry-id))) 
					     (onclick ,(string-append "flip('toggle-" (number->string entry-id) "', 'desc-" (number->string entry-id) "');")))"Show"))
				    (div ((style "display:none")(id ,(string-append "desc-" (number->string entry-id)))) 
					 ,(cdata 'cdata-start 'cdata-end desc))
				    )
			   ))))))))

(define (start request)
  (rss-dispatch request))

(define-values (rss-dispatch rss-url)
  (dispatch-rules
   (("") recent-entries)
   (("mark-read") mark-read)
   (("search") search)
   (("search-page") search-page)
   ))

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
