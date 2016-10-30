ALTER TABLE tags
ADD COLUMN IsGlobal boolean;

UPDATE tags
SET IsGlobal = false;

UPDATE tags
SET IsGlobal = true
WHERE TagName = 'All Tags';

UPDATE tags
SET TagName = 'All Boards'
WHERE IsGlobal = true;

ALTER TABLE tags
ALTER IsGlobal SET NOT NULL;

ALTER TABLE tags
ADD COLUMN UserStatusID bigint;

DO
    $do$
    BEGIN
        IF (SELECT 1 FROM UserStatuses WHERE UserStatusDesc = 'User') IS NULL THEN
            INSERT INTO UserStatuses (UserStatusDesc, UserStatusRank)
            VALUES ('User', 3);
        ELSE
            RAISE NOTICE 'User status exists';
        END IF;
    END
    $do$;

UPDATE tags
SET UserStatusID = CASE
                        WHEN AdminOnly = true THEN (SELECT UserStatusID
                                                    FROM UserStatuses
                                                    WHERE UserStatusDesc = 'Admin')
                        WHEN ModeratorOnly = true THEN (SELECT UserStatusID
                                                    FROM UserStatuses
                                                    WHERE UserStatusDesc = 'Moderator')
                        ELSE (SELECT UserStatusID
                                                    FROM UserStatuses
                                                    WHERE UserStatusDesc = 'User')
                   END;

ALTER TABLE tags
DROP AdminOnly CASCADE,
DROP ModeratorOnly CASCADE;

ALTER TABLE tags
ALTER UserStatusID SET NOT NULL;
