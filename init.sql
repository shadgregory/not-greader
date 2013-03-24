CREATE TABLE feed (
	id serial,
	url text,
	image_url text,
	title text
);

CREATE TABLE item (
	id serial,
	title text,
	url text,
	description text,
	feed_id integer references feed(id),
	date timestamp,
	read boolean default false
);
