DROP VIEW IndexThreads;
CREATE
 ALGORITHM = UNDEFINED
 VIEW `IndexThreads`
 AS
 SELECT `threads`.*,
    	a.NSFW AS TagNSFW,
	b.NSFW AS SubTagNSFW,
	a.AdminOnly AS ForcedTag,
	b.AdminOnly AS ForcedSubTag,
	(SELECT MAX(PostTime)
	 FROM `posts`
	 WHERE threads.ThreadID = posts.ThreadID) AS LatestPostTime,
	(SELECT ModName
	 FROM `posts`
	 WHERE threads.ThreadID = posts.ThreadID
	 ORDER BY PostID LIMIT 0,1) AS ModName,
	(SELECT MAX(PostTime)
	 FROM `posts`
	 WHERE threads.ThreadID = posts.ThreadID
	   AND Bump = 1) AS LatestBump,
	a.TagName as Tag,
	a.ModeratorOnly AS ModTag,
	b.TagName as SubTag,
	b.ModeratorOnly AS ModSubTag,
	(SELECT COUNT(1)
	 FROM `posts`
	 WHERE threads.ThreadID = posts.ThreadID) - 1 AS PostCount
 FROM `threads`
 LEFT JOIN `posts` ON (threads.ThreadID = posts.ThreadID)
 LEFT JOIN tags a ON a.TagID = threads.TagID
 LEFT JOIN tags b ON b.TagID = threads.SubTagID
 GROUP BY threads.ThreadID
 ORDER BY `Stickied` DESC,`LatestBump`
