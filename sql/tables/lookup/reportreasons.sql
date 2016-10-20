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
-- Table structure for table `reportreasons`
--

DROP TABLE IF EXISTS `reportreasons`;
CREATE TABLE `reportreasons` (
  `ReportReasonID` int(11) NOT NULL,
  `ReportReasonText` varchar(255) NOT NULL,
  `IsActive` tinyint(1) NOT NULL
) ENGINE=InnoDB DEFAULT CHARSET=latin1;

--
-- Dumping data for table `reportreasons`
--

INSERT INTO `reportreasons` (`ReportReasonID`, `ReportReasonText`, `IsActive`) VALUES
(1, 'Illegal content', 1),
(2, 'Spamming', 1),
(3, 'Poster underage', 1),
(4, 'Misuse of tags/off-topic', 1),
(5, 'Pornography missing tag', 1);

--
-- Indexes for dumped tables
--

--
-- Indexes for table `reportreasons`
--
ALTER TABLE `reportreasons`
  ADD PRIMARY KEY (`ReportReasonID`);

--
-- AUTO_INCREMENT for dumped tables
--

--
-- AUTO_INCREMENT for table `reportreasons`
--
ALTER TABLE `reportreasons`
  MODIFY `ReportReasonID` int(11) NOT NULL AUTO_INCREMENT, AUTO_INCREMENT=6;
/*!40101 SET CHARACTER_SET_CLIENT=@OLD_CHARACTER_SET_CLIENT */;
/*!40101 SET CHARACTER_SET_RESULTS=@OLD_CHARACTER_SET_RESULTS */;
/*!40101 SET COLLATION_CONNECTION=@OLD_COLLATION_CONNECTION */;
