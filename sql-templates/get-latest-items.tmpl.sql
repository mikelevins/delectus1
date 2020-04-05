SELECT a.*
  FROM (SELECT ROW_NUMBER() OVER (PARTITION BY item ORDER BY revision DESC, origin DESC) rank, *
        FROM `list_data` WHERE optype='item') a
 WHERE a.rank = 1 order by a.revision
