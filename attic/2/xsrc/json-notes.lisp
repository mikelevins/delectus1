(in-package :delectus)

#| 

SQL to extract the column labels from an itemState object:

select key from 
  json_each(
    '{"deleted": false,
      "values": {"L84772503adef43ca81e680bff84c302c":"First column", 
                 "La453baa25d3047a7b9eb408015770338":"Second column"}
     }',
    "$.values")


Getting key and value:

select key,value from 
  json_each(
    '{"deleted": false,
      "values": {"L84772503adef43ca81e680bff84c302c":"First column", 
                 "La453baa25d3047a7b9eb408015770338":"Second column"}
     }',
    "$.values")

Get the value from the 'deleted' field:

select 
  json_extract(
    '{"deleted": false,
      "values": {"L84772503adef43ca81e680bff84c302c":"First column", 
                 "La453baa25d3047a7b9eb408015770338":"Second column"}
     }',
    "$.deleted")

Returns a table with one row and one column. The column label is the json_extract expression; the value is zero.

|#
