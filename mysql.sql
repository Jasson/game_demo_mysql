drop database if exists game;
create database game;
use game;

DROP TABLE IF EXISTS `map`;
CREATE TABLE `map` (
  `id` BIGINT(20) NOT NULL AUTO_INCREMENT, 
  `x` BIGINT(20) DEFAULT 0,
  `y` BIGINT(20) DEFAULT 0,
  `author_id` varchar(250) DEFAULT NULL,
  PRIMARY KEY (`id`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8;
CREATE INDEX i_map_author_id ON map(author_id(36));
CREATE UNIQUE INDEX i_map_x_y ON map(x, y);
CREATE UNIQUE INDEX i_map_x_y_author_id ON map(x, y, author_id(36));


DROP TABLE IF EXISTS `city`;
CREATE TABLE `city` (
  `id` BIGINT(20) NOT NULL AUTO_INCREMENT, 
  `x` BIGINT(20) DEFAULT 0,
  `y` BIGINT(20) DEFAULT 0,
  `foods` DECIMAL(10,2) default 0,
  `product_food_rate` DECIMAL(10,2) DEFAULT 0.27,
  `golds` DECIMAL(10,2) default 0,
  `gold_tax` DECIMAL(10,2) DEFAULT 0.2,
  `people` BIGINT(20) DEFAULT 100,
  `soldiers` BIGINT(20) default 0,
  `name` varchar(250) DEFAULT "",
  `author_id` varchar(250) DEFAULT "",
  `is_captial` varchar(250) DEFAULT "0",
  `is_food_crisis` varchar(250) DEFAULT "0",
  `tax_tag`  varchar(250) DEFAULT "",
  PRIMARY KEY (`id`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8;
CREATE INDEX i_city_author_id ON city(author_id(36));
CREATE UNIQUE INDEX i_city_x_y ON city(x, y);
CREATE UNIQUE INDEX i_city_x_y_author_id ON city(x, y, author_id(36));


  --  1:piker2:archer 3:cavalry
  --  3:complete 4:forward 5:back 6:battle 
DROP TABLE IF EXISTS `soldier`;
CREATE TABLE `soldier` (
  `id` BIGINT(20) NOT NULL AUTO_INCREMENT, 
  `city_id` BIGINT(20),
  `x` BIGINT(20) DEFAULT 0,
  `y` BIGINT(20) DEFAULT 0,
  `author_id` varchar(250) DEFAULT NULL,
  `type`    varchar(250),   
  `state`   varchar(250),   
  `soldier_sum`     BIGINT(20),
  `time`    varchar(250),
  PRIMARY KEY (`id`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8;

CREATE INDEX i_soldier_author_id ON soldier(author_id(36));
CREATE INDEX i_soldier_x_y_id_author_id ON soldier(x, y, author_id);
CREATE INDEX i_soldier_x_y_id_author_id_type ON soldier(x, y, author_id, type);
CREATE UNIQUE INDEX i_soldier_x_y_id_author_id_type_state ON soldier(x, y, author_id, type, state);
CREATE INDEX i_soldier_state ON soldier(state);


DROP TABLE IF EXISTS `soldier_queue`;
CREATE TABLE `soldier_queue` (
  `id` BIGINT(20) NOT NULL AUTO_INCREMENT, 
  `city_id` BIGINT(20),
  `x` BIGINT(20) DEFAULT 0,
  `y` BIGINT(20) DEFAULT 0,
  `author_id` varchar(250) DEFAULT NULL,
  `type`    varchar(250),
  `soldier_sum`     BIGINT(20),
  `queue_id` varchar(250),
  `state`   varchar(250), -- %%  1:wait 2:training 
  `time`    varchar(250),
  PRIMARY KEY (`id`)
  ) ENGINE=InnoDB DEFAULT CHARSET=utf8;

CREATE INDEX i_soldier_queue_author_id ON soldier_queue(author_id(36));
CREATE INDEX i_soldier_queue_x_y_id_author_id ON soldier_queue(x, y, author_id);
CREATE INDEX i_soldier_queue_x_y_id_author_id_type ON soldier_queue(x, y, author_id, type);
CREATE UNIQUE INDEX i_soldier_queue_x_y_id_author_id_type_queue_id_state ON soldier_queue(x, y, author_id, type, queue_id, state);




