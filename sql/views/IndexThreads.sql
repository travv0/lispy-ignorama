CREATE OR REPLACE
 VIEW IndexThreads
 AS
 SELECT threads.*,
      a.NSFW AS TagNSFW,
  b.NSFW AS SubTagNSFW,
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
  a.TagName as Tag,
  b.TagName as SubTag,
  (SELECT COUNT(1)
   FROM posts
   WHERE threads.ThreadID = posts.ThreadID) - 1 AS PostCount
 FROM threads
 LEFT JOIN posts ON (threads.ThreadID = posts.ThreadID)
 LEFT JOIN tags a ON a.TagID = threads.TagID
 LEFT JOIN tags b ON b.TagID = threads.SubTagID
 GROUP BY threads.ThreadID,
          a.NSFW,
          b.NSFW,
          a.TagName,
          b.TagName
 ORDER BY Stickied DESC, LatestBump;
