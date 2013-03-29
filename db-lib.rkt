#lang racket
(require db)

(define search-items 
  (lambda (pgc query)
    (in-query pgc 
	      "select feed.title, item.title, item.url, item.date from item inner join feed on item.feed_id = feed.id where lower(item.title) ~ lower($1) or lower(item.description) ~ lower($2) order by item.date desc" query query)))

(define fetch-unread-items
  (lambda (pgc feed-id)
    (in-query pgc
	      "select title, description, url, id from item where feed_id=$1 and read=false limit 50" 
	      (string->number feed-id))))

(define get-unread-count 
  (lambda (pgc feed-id)  
    (query-value pgc "select count(*) from item where feed_id=$1 and read=false" (string->number feed-id))))

(define fetch-feed-title 
  (lambda (pgc feed-id)
    (query-value pgc "select title from feed where id=$1" (string->number feed-id))))

(define get-feed-list
  (lambda (pgc)
    (in-query pgc "select title, url, id from feed order by title")))

(define get-item
  (lambda (pgc)
    (in-query pgc 
	      "select feed.title, item.title, item.url, feed.image_url, item.date, item.description, item.id from item inner join feed on feed.id = item.feed_id where item.date > (now () - interval '20 hour') and item.read = false order by item.date desc")))

(provide (all-defined-out))
