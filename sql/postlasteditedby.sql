ALTER TABLE posts
DROP postlasteditby;
ALTER TABLE posts
DROP postdeletedby;

ALTER TABLE posts
ADD postlasteditby INT;
ALTER TABLE posts
ADD postdeletedby INT;
