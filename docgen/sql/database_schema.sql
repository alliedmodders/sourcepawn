SET SQL_MODE = "NO_AUTO_VALUE_ON_ZERO";
SET time_zone = "+00:00";

create table if not exists `spdoc_include` (
  `id` int unsigned not null auto_increment primary key,
  `name` varchar(64) not null,
  `doc` text not null,
  `content` text not null,
  unique key `name_key` (`name`)
) ENGINE=InnoDB CHARSET=utf8mb4 AUTO_INCREMENT=1 ;

CREATE TABLE IF NOT EXISTS `spdoc_function` (
  `id` int unsigned not null auto_increment primary key,
  `include_id` int unsigned not null,
  `parent_type` varchar(12),
  `parent_id` int unsigned,
  `kind` varchar(16),
  `name` varchar(64) not null,
  `signature` text not null,
  `brief` text not null,
  `data` text not null
) ENGINE=InnoDB CHARSET=utf8mb4 AUTO_INCREMENT=1 ;
ALTER TABLE `spdoc_function` ADD UNIQUE( `include_id`, `parent_type`, `parent_id`, `name`); 

create table if not exists `spdoc_class` (
  `id` int unsigned not null auto_increment primary key,
  `include_id` int unsigned not null,
  `name` varchar(64) not null,
  `brief` text not null,
  `data` text not null,
  unique key `class_key` (`include_id`, `name`)
) ENGINE=InnoDB CHARSET=utf8mb4 AUTO_INCREMENT=1 ;

create table if not exists `spdoc_enum` (
  `id` int unsigned not null auto_increment primary key,
  `include_id` int unsigned not null,
  `name` varchar(64) not null,
  `brief` text not null,
  `data` text not null,
  unique key `class_key` (`include_id`, `name`)
) ENGINE=InnoDB CHARSET=utf8mb4 AUTO_INCREMENT=1 ;

create table if not exists `spdoc_constant` (
  `id` int unsigned not null auto_increment primary key,
  `include_id` int unsigned not null,
  `parent_type` varchar(32),
  `parent_id` int unsigned not null,
  `name` varchar(64) not null,
  `brief` text not null,
  `data` text not null,
  unique key `class_key` (`include_id`, `name`)
) ENGINE=InnoDB CHARSET=utf8mb4 AUTO_INCREMENT=1 ;

-- In the future, stuff should be added to this table as much as possible,
-- such that we can fully reconstruct types off spdoc_type.
create table if not exists `spdoc_type` (
  `id` int unsigned not null auto_increment primary key,
  `include_id` int unsigned not null,
  `parent_type` varchar(12),
  `parent_id` int unsigned,
  `kind` varchar(12) not null, 
  `name` varchar(64) not null,
  `brief` text not null,
  `data` text not null
) ENGINE=InnoDB CHARSET=utf8mb4 AUTO_INCREMENT=1;
ALTER TABLE `spdoc_type` ADD UNIQUE( `include_id`, `parent_type`, `parent_id`, `name`); 

create table if not exists `spdoc_property` (
  `id` int unsigned not null auto_increment primary key,
  `include_id` int unsigned not null,
  `class_id` int unsigned not null,
  `name` varchar(64) not null,
  `type` varchar(128) not null,
  `getter` tinyint(1) not null,
  `setter` tinyint(1) not null,
  `brief` text not null,
  `data` text not null,
  unique key `fun_key` (`include_id`, `class_id`, `name`)
) ENGINE=InnoDB CHARSET=utf8mb4 AUTO_INCREMENT=1 ;
