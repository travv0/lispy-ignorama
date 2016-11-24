DROP VIEW IndexThreads;

CREATE OR REPLACE
 VIEW IndexThreads
 AS
 SELECT threads.*,
      tags.NSFW AS TagNSFW,
  (SELECT MAX(PostTime)
   FROM posts
   WHERE threads.ThreadID = posts.ThreadID) AS LatestPostTime,
  (SELECT PostID
   FROM posts
   WHERE threads.ThreadID = posts.ThreadID
   ORDER BY PostID LIMIT 1) AS PostID,
  (SELECT MAX(PostTime)
   FROM posts
   WHERE threads.ThreadID = posts.ThreadID
     AND Bump = true) AS LatestBump,
  tags.TagName as Tag,
  tags.UserStatusID,
  (SELECT COUNT(1)
   FROM posts
   WHERE threads.ThreadID = posts.ThreadID) - 1 AS PostCount,
   MAX(PostID) AS MaxPostID
 FROM threads
 LEFT JOIN posts ON (threads.ThreadID = posts.ThreadID)
 LEFT JOIN tags ON tags.TagID = threads.TagID
 GROUP BY threads.ThreadID,
          tags.NSFW,
          tags.TagName,
          tags.UserStatusID
 ORDER BY Stickied DESC, LatestBump DESC;
