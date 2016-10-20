-- phpMyAdmin SQL Dump
-- version 4.5.4.1deb2ubuntu2
-- http://www.phpmyadmin.net
--
-- Host: localhost
-- Generation Time: Oct 19, 2016 at 08:13 PM
-- Server version: 5.7.15-0ubuntu0.16.04.1
-- PHP Version: 7.0.8-0ubuntu0.16.04.3

SET SQL_MODE = "NO_AUTO_VALUE_ON_ZERO";
SET time_zone = "+00:00";


/*!40101 SET @OLD_CHARACTER_SET_CLIENT=@@CHARACTER_SET_CLIENT */;
/*!40101 SET @OLD_CHARACTER_SET_RESULTS=@@CHARACTER_SET_RESULTS */;
/*!40101 SET @OLD_COLLATION_CONNECTION=@@COLLATION_CONNECTION */;
/*!40101 SET NAMES utf8mb4 */;

--
-- Database: `tssund93_forums`
--

-- --------------------------------------------------------

--
-- Table structure for table `posts`
--

CREATE TABLE IF NOT EXISTS `posts` (
  `PostID` int(20) NOT NULL AUTO_INCREMENT,
  `ThreadID` int(11) NOT NULL,
  `ModName` varchar(20) NOT NULL,
  `PostContent` longtext NOT NULL,
  `PostEditContent` longtext NOT NULL,
  `PostTime` datetime NOT NULL,
  `PostIP` varchar(50) NOT NULL,
  `PostRevealedOP` tinyint(1) NOT NULL,
  `Bump` tinyint(1) NOT NULL,
  `PostDeleted` tinyint(1) NOT NULL,
  `PostDeletedBy` varchar(20) NOT NULL,
  `PostDeleteTime` datetime NOT NULL,
  PRIMARY KEY (`PostID`),
  KEY `PostID` (`PostID`,`ThreadID`),
  KEY `PostTime` (`PostTime`),
  KEY `PostID_2` (`PostID`),
  KEY `ThreadID` (`ThreadID`),
  KEY `Bump` (`Bump`)
) ENGINE=InnoDB DEFAULT CHARSET=latin1;

/*!40101 SET CHARACTER_SET_CLIENT=@OLD_CHARACTER_SET_CLIENT */;
/*!40101 SET CHARACTER_SET_RESULTS=@OLD_CHARACTER_SET_RESULTS */;
/*!40101 SET COLLATION_CONNECTION=@OLD_COLLATION_CONNECTION */;
