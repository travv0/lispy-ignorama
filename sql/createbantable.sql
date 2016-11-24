DROP TABLE banlog;
DROP TABLE bans;

CREATE TABLE bans (
       banid        SERIAL PRIMARY KEY,
       bannerid     INT,
       banneeid     INT,
       banneeip     TEXT,
       bantime      TIMESTAMP,
       banend       TIMESTAMP,
       banreason    TEXT,
       postid       INT,
       unbanned     BOOLEAN,
       unbannedbyid INT
);
