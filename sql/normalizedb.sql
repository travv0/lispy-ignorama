DROP TABLE users;
ALTER TABLE admin RENAME TO users;

ALTER TABLE posts
ADD UserID bigint;

UPDATE posts
SET UserID = (SELECT UserID FROM users WHERE lower(users.UserName) = (posts.ModName));

ALTER TABLE posts
DROP ModName;

ALTER TABLE threads
DROP ThreadIP CASCADE;
