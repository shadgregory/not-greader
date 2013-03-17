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
     #:mime-type #"application/xml")
    ))

(define recent-entries 
  (lambda (req)
    (response/xexpr
     `(html
       (head
	(script ((type "text/javascript")(src "rss.js")) " ")
	(script ((src "//ajax.googleapis.com/ajax/libs/jquery/1.9.1/jquery.min.js")) " ")
       (body ((bgcolor "e5e5e5"))
	(div
	 ,@(for/list (((feed-title title link image-url date desc entry-id) (in-query pgc 
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
