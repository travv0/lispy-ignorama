UPDATE tags
SET IsActive = false
WHERE UserStatusID = (SELECT UserStatusID
                      FROM UserStatuses
                      WHERE UserStatusDesc = 'User');

UPDATE tags
SET TagName = 'Off-Topic'
WHERE TagName = 'Random';

UPDATE tags
SET IsActive = true
WHERE TagName IN ('Anime & Manga',
                  'Video Games',
                  'Technology',
                  'Off-Topic',
                  'Music');
