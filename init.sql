CREATE TABLE feed (
       id serial,
       url text,
       image_url text,
       title text
);
alter table feed add constraint unique_feed_url unique(url);
alter table feed add constraint unique_feed_id unique(id);

CREATE TABLE item (
       id serial,
       title text,
       url text,
       description text,
       feed_id integer references feed(id),
       date timestamp,
       read boolean default false
);
alter table item add constraint unique_item_url unique(url);
alter table item add constraint unique_item_id unique(id);

CREATE TABLE rssuser (
       id serial,
       username text,
       cookieid text,
       password text
);

alter table rssuser add constraint unique_rssuser_id unique(id);
alter table rssuser add constraint unique_username unique(username);

create table rssuser_feed(
       feed_id integer references feed(id), 
       rssuser_id integer references rssuser(id)
);
alter table rssuser_feed add constraint unique_rssuser_feed unique(feed_id, rssuser_id);

create table read_items(
    item_id integer references item(id),
    rssuser_id integer references rssuser(id)
);
