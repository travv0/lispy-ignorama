ALTER TABLE posts
DROP postlasteditby,
DROP postdeletedby;

ALTER TABLE posts
ADD postlasteditby INT,
ADD postdeletedby INT;
