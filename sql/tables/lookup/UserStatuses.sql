-- phpMyAdmin SQL Dump
-- version 4.5.4.1deb2ubuntu2
-- http://www.phpmyadmin.net
--
-- Host: localhost
-- Generation Time: Oct 18, 2016 at 10:00 PM
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
-- Table structure for table `UserStatuses`
--

CREATE TABLE `UserStatuses` (
  `UserStatusID` int(11) NOT NULL,
  `UserStatusDesc` varchar(20) COLLATE utf8_unicode_ci NOT NULL,
  `UserStatusRank` int(11) NOT NULL
) ENGINE=InnoDB DEFAULT CHARSET=utf8 COLLATE=utf8_unicode_ci;

--
-- Dumping data for table `UserStatuses`
--

INSERT INTO `UserStatuses` (`UserStatusID`, `UserStatusDesc`, `UserStatusRank`) VALUES
(1, 'Admin', 1),
(2, 'Moderator', 2);

--
-- Indexes for dumped tables
--

--
-- Indexes for table `UserStatuses`
--
ALTER TABLE `UserStatuses`
  ADD PRIMARY KEY (`UserStatusID`),
  ADD UNIQUE KEY `UserStatusDesc` (`UserStatusDesc`);

--
-- AUTO_INCREMENT for dumped tables
--

--
-- AUTO_INCREMENT for table `UserStatuses`
--
ALTER TABLE `UserStatuses`
  MODIFY `UserStatusID` int(11) NOT NULL AUTO_INCREMENT, AUTO_INCREMENT=3;
/*!40101 SET CHARACTER_SET_CLIENT=@OLD_CHARACTER_SET_CLIENT */;
/*!40101 SET CHARACTER_SET_RESULTS=@OLD_CHARACTER_SET_RESULTS */;
/*!40101 SET COLLATION_CONNECTION=@OLD_COLLATION_CONNECTION */;
