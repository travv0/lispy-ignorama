ALTER TABLE admin
ALTER COLUMN UserStatusID TYPE bigint USING userstatusid::bigint
