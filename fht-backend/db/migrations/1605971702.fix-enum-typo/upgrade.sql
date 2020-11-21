-- FUGLY, but works for the first cut.

DELETE FROM plant WHERE TRUE;

ALTER TABLE plant DROP COLUMN maintenance_types; 
DROP TYPE maintenance_type; 

CREATE TYPE maintenance_type AS ENUM ('Pruning', 'Fertilizing', 'Repotting'); 
ALTER TABLE plant ADD COLUMN maintenance_types maintenance_type[] DEFAULT '{}';  

