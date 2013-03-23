#lang racket
(require db)

(define search-items 
  (lambda (pgc query)
    (in-query pgc 
	      "select feed.title, entry.title, entry.url, entry.date from entry inner join feed on entry.feed_id = feed.id where lower(entry.title) ~ lower($1) or lower(entry.description) ~ lower($2) order by entry.date desc" query query)))

(define fetch-unread-items
  (lambda (pgc feed-id)
    (in-query pgc
	      "select title, description, url, id from entry where feed_id=$1 and read=false limit 50" 
	      (string->number feed-id))))

(provide (all-defined-out))
