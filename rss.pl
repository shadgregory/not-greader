#!/usr/bin/perl
use strict;
use LWP::Simple;
use DBI;
use XML::RSS;

my @feeds = ();
my $dbh = DBI->connect("dbi:Pg:dbname=feed", "feed","abc123");

#clean up the oldest items
$dbh->do("UPDATE item SET read=true where date < (now() - interval '6 month')");

my $feed_handle = $dbh->prepare("select id, url, title from feed;");
$feed_handle->execute();

while (my @feeds = $feed_handle->fetchrow_array()) {
    my $feed_id = $feeds[0];
    my $feed_url = $feeds[1];
    print $feeds[2] . "\n";
    my $xml = get($feed_url);
    my $rss = new XML::RSS;
    $rss->parse($xml);
    for my $i (@{$rss->{items}}) {
	my $pub_date = $i->{'pubDate'};
	$pub_date = $i->{'dc'}->{'date'} if !$pub_date;
	my $title    = $i->{'title'};
	my $url      = $i->{'link'};
	my $desc     = $i->{'description'};
	next if !$url; #looking at you perl 6 planet!
	my $sth = $dbh->prepare("SELECT * from item where url=?;");
	$sth->execute($url);
	my @data = $sth->fetchrow_array();
	next if @data > 0;
	my $insert_sth = $dbh->prepare("INSERT INTO item(title, url, feed_id, description, date) values(?,?,?,?,?)");
	$insert_sth->execute($title, $url, $feed_id, $desc, $pub_date);
    }
}

