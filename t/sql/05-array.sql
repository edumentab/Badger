-- sub base-query(--> @)
SELECT 1 as result
FROM unnest(VALUES(1, 2));