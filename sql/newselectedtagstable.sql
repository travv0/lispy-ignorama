DROP TABLE selectedtags;

CREATE TABLE selectedtags (
       selectedtagsid SERIAL,
       userid         int,
       userip         text,
       tagid          int
);
