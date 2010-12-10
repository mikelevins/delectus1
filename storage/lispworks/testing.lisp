
(in-package :sqlite)

(with-open-database (db ":memory:"))

(with-open-database (db ":memory:")
  (execute-non-query db "create table users (id integer primary key, user_name text not null, age integer null)")
  (execute-non-query/named db "insert into users (user_name, age) values (:name, :age)" ":name" "Fred" ":age" 101)
  (execute-single db "select id from users where user_name = ?" "Fred")
  (execute-single db "select age from users where user_name = ?" "Fred"))

(time 
 (with-open-database (db "/tmp/sqlite-test.delectus")
   (execute-non-query db "create table users (id integer primary key, first_name text not null, last_name text not null)")
   (execute-non-query/named db "insert into users (first_name, last_name) values (:first_name, :last_name)" ":first_name" "Fred" ":last_name" "Flintstone")
   (execute-non-query/named db "insert into users (first_name, last_name) values (:first_name, :last_name)" ":first_name" "Wilma" ":last_name" "Flintstone")
(execute-non-query/named db "insert into users (first_name, last_name) values (:first_name, :last_name)" ":first_name" "Barney" ":last_name" "Rubble")
(execute-non-query/named db "insert into users (first_name, last_name) values (:first_name, :last_name)" ":first_name" "Betty" ":last_name" "Rubble")
(execute-to-list db "select first_name from users where first_name like '%l%' UNION select first_name from users where last_name like '%l%'")))

(time 
 (with-open-database (db ":memory:")
   (execute-non-query db "create table users (id integer primary key, first_name text not null, last_name text not null)")
   (execute-to-list db "PRAGMA table_info('users')")))

(time 
 (setq $zips (connect "/Users/mikel/Projects/delectus/storage/test-data/zipcode.sqlite")))

(time
 (execute-to-list $zips "PRAGMA table_info('delectus')"))

(time
 (execute-to-list $zips "select city from delectus where city like ? limit 10" "%ab%"))

(time
 (execute-to-list $zips "select city from delectus where city like ? limit 100" "%ab%"))

(time
 (length (execute-to-list $zips "select city from delectus where city like ?" "%ab%")))

(time
 (length (execute-to-list $zips "select * from delectus")))

(time
 (length (execute-to-list $zips "select * from delectus limit 100")))

(time
 (length (execute-to-list $zips "select * from delectus order by city limit 1000")))