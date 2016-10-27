--
-- Database: tssund93_forums
--

-- --------------------------------------------------------

--
-- Table structure for table tags
--

DROP TABLE IF EXISTS tags;
CREATE TABLE tags (
  TagID int NOT NULL, --AUTO_INCREMENT,
  TagName varchar(20) NOT NULL,
  IsActive tinyint NOT NULL,
  AdminOnly tinyint NOT NULL,
  ModeratorOnly tinyint NOT NULL,
  NSFW tinyint NOT NULL,
  PRIMARY KEY (TagID)
) ENGINE=InnoDB AUTO_INCREMENT=89 DEFAULT CHARSET=latin1;

--
-- Dumping data for table tags
--

INSERT INTO tags (TagID, TagName, IsActive, AdminOnly, ModeratorOnly, NSFW) VALUES
(1, 'Anime & Manga', 1, 0, 0, 0),
(2, 'Video Games', 1, 0, 0, 0),
(5, 'Literature', 1, 0, 0, 0),
(6, 'Movies', 0, 0, 0, 0),
(7, 'Food & Drink', 1, 0, 0, 0),
(8, 'Cooking', 0, 0, 0, 0),
(9, 'Art', 1, 0, 0, 0),
(10, 'Technology', 1, 0, 0, 0),
(11, 'Cosplay', 1, 0, 0, 0),
(12, 'Transportation', 0, 0, 0, 0),
(13, 'Images', 0, 0, 0, 0),
(14, 'Videos', 0, 0, 0, 0),
(15, 'Cute', 1, 0, 0, 0),
(16, 'Sexy', 1, 0, 0, 0),
(17, 'Women', 0, 0, 0, 0),
(18, 'Men', 0, 0, 0, 0),
(19, 'Site Discussion', 1, 0, 0, 0),
(20, 'Needs Tagged', 1, 0, 0, 0),
(21, 'Otaku', 1, 0, 0, 1),
(22, 'Random', 1, 0, 0, 1),
(23, 'Pokémon', 0, 0, 0, 0),
(24, 'Touhou', 0, 0, 0, 0),
(25, 'Comics', 0, 0, 0, 0),
(26, 'Comics & Cartoons', 1, 0, 0, 0),
(27, 'Film & Television', 1, 0, 0, 0),
(28, 'Film', 0, 0, 0, 0),
(29, 'Weapons & Military', 1, 0, 0, 0),
(30, 'Auto & Transport', 1, 0, 0, 0),
(31, 'Animals', 1, 0, 0, 0),
(32, 'Sports & Fitness', 1, 0, 0, 0),
(33, 'Math & Science', 1, 0, 0, 0),
(34, 'Math', 0, 0, 0, 0),
(35, 'Photography', 1, 0, 0, 0),
(36, 'Pornography', 1, 0, 0, 1),
(37, 'Advice', 1, 0, 0, 0),
(38, 'LGBT', 1, 0, 0, 0),
(39, 'Hentai', 1, 0, 0, 1),
(41, 'Fitness', 0, 0, 0, 0),
(42, 'Health', 1, 0, 0, 0),
(43, 'International', 1, 0, 0, 0),
(44, 'Politics', 1, 0, 0, 0),
(45, 'Foreign Language', 1, 0, 0, 0),
(46, 'Drugs', 1, 0, 0, 0),
(47, 'Alcohol', 0, 0, 0, 0),
(48, 'Japan', 0, 0, 0, 0),
(49, 'Korea', 0, 0, 0, 0),
(51, 'All Tags', 1, 1, 0, 0),
(52, 'Music', 1, 0, 0, 0),
(53, 'Ecchi', 0, 0, 0, 0),
(54, 'Yuri', 0, 0, 0, 0),
(55, 'Yaoi', 0, 0, 0, 0),
(56, 'Gay', 0, 0, 0, 0),
(57, 'Manga', 0, 0, 0, 0),
(58, 'Wallpaper', 0, 0, 0, 0),
(59, 'Mecha', 0, 0, 0, 0),
(60, 'Nature', 0, 0, 0, 0),
(61, 'Traditional Games', 0, 0, 0, 0),
(62, 'Esports', 0, 0, 0, 0),
(63, 'Toys', 0, 0, 0, 0),
(64, 'Finance', 0, 0, 0, 0),
(65, 'Papercraft', 0, 0, 0, 0),
(66, 'Fashion', 1, 0, 0, 0),
(67, 'Graphic Design', 0, 0, 0, 0),
(68, 'DIY', 1, 0, 0, 0),
(69, 'GIF', 0, 0, 0, 0),
(70, 'Torrents', 0, 0, 0, 0),
(71, 'Paranormal', 1, 0, 0, 0),
(72, 'My Little Pony', 0, 0, 0, 0),
(73, 'Request', 1, 0, 0, 0),
(74, 'Operating Systems', 0, 0, 0, 0),
(75, 'Military', 0, 0, 0, 0),
(76, 'Celebrities', 0, 0, 0, 0),
(77, 'Programming', 0, 0, 0, 0),
(78, 'Vocaloid', 0, 0, 0, 0),
(79, 'VIP', 0, 0, 0, 0),
(80, 'Conventions', 1, 0, 0, 0),
(81, 'Social Justice', 0, 0, 0, 0),
(82, 'Religion', 1, 0, 0, 0),
(83, 'Let''s Play', 0, 0, 0, 0),
(85, 'Moderator', 1, 0, 1, 0),
(86, 'Informational', 1, 0, 0, 0),
(87, 'Socially Retarded', 1, 0, 0, 0),
(88, 'Travel', 1, 0, 0, 0);
