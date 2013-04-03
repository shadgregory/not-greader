#lang racket
(require db)

(define search-items
  (lambda (pgc query user-id)
    (in-query pgc  
	      "select feed.title, item.title, item.url, item.date AS item_title from feed 
       inner join rssuser_feed on rssuser_feed.feed_id = feed.id 
       inner join rssuser on rssuser_feed.rssuser_id = rssuser.id 
       inner join item on item.feed_id = feed.id 
       where rssuser.id=$1 and (lower(item.title) ~ lower($2) or lower(item.description) ~ lower($3)) order by item.date desc"
	      user-id query query)))

(define fetch-unread-items
  (lambda (pgc feed-id user-id)
    (in-query pgc
	      "select item.title, item.description, item.url, item.id from item 
       inner join feed on item.feed_id = feed.id 
       inner join rssuser_feed on rssuser_feed.feed_id = feed.id 
       inner join rssuser on rssuser.id = rssuser_feed.rssuser_id
       left outer join read_item on read_item.item_id = item.id and read_item.rssuser_id = rssuser.id
       where item.feed_id=$1 and rssuser.id=$2 and read_item.item_id is null"
	      (string->number feed-id) (string->number user-id))))


(define get-unread-count 
  (lambda (pgc feed-id user-id)  
    (query-value pgc "select count(*) from item 
       inner join feed on item.feed_id = feed.id 
       inner join rssuser_feed on rssuser_feed.feed_id = feed.id 
       inner join rssuser on rssuser.id = rssuser_feed.rssuser_id
       left outer join read_item on read_item.item_id = item.id and read_item.rssuser_id = rssuser.id
       where item.feed_id=$1 and rssuser.id=$2 and read_item.item_id is null"
		 (string->number feed-id) user-id)))

(define fetch-feed-title 
  (lambda (pgc feed-id)
    (query-value pgc "select title from feed where id=$1" (string->number feed-id))))

(define get-feed-list
  (lambda (pgc user-id)
    (in-query pgc "select feed.title, feed.url, feed.id from feed 
       inner join rssuser_feed on rssuser_feed.feed_id = feed.id
       inner join rssuser on rssuser_feed.rssuser_id = rssuser.id
       where rssuser.id=$1
       order by title" user-id)))

(define get-item
  (lambda (pgc user-id)
    (in-query pgc 
	      "select feed.title, item.title, item.url, item.date, item.description, item.id from item 
       inner join feed on item.feed_id = feed.id 
       inner join rssuser_feed on rssuser_feed.feed_id = feed.id 
       inner join rssuser on rssuser.id = rssuser_feed.rssuser_id
       left outer join read_item on read_item.item_id = item.id and read_item.rssuser_id = rssuser.id
       where item.date > (now () - interval '20 hour') and rssuser.id = $1 and read_item.item_id is null order by item.date desc" 
	      user-id)))

(define get-rssuser
  (lambda (pgc username) 
    (query-row pgc "select password,username,cookieid,id from rssuser where username=$1" username)))

(provide (all-defined-out))
