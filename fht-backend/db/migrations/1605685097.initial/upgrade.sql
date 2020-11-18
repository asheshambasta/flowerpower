CREATE TYPE maintenance_type AS ENUM ('Pruning', 'Ferilizing', 'Repotting');
CREATE TYPE maintenance_freq AS ENUM ('Week', 'Month', 'Year');

CREATE TABLE plant 
       ( plant_id bigint NOT NULL PRIMARY KEY 
       , plant_name text NOT NULL 
       , description text NULL DEFAULT NULL 
       , image text NULL DEFAULT NULL 
       , time_planted TIMESTAMP WITH TIME ZONE NULL DEFAULT NULL 
       , maintenance_types maintenance_type[] NOT NULL DEFAULT '{}'
       , maintenance_freqs maintenance_freq[] NOT NULL DEFAULT '{}'
       );

GRANT INSERT, SELECT, DELETE, UPDATE ON plant TO flowerpower;
