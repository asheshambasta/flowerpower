CREATE TABLE plant_maintenance_log 
	( plant_id BIGINT NOT NULL 
	, maintenance maintenance_type NOT NULL 
	, time_performed TIMESTAMP WITH TIME ZONE NOT NULL
	, CONSTRAINT plant_maintenance_fk FOREIGN KEY (plant_id) REFERENCES
		plant(plant_id) ON DELETE CASCADE ON UPDATE CASCADE
	, CONSTRAINT plant_maintenance_unique UNIQUE (plant_id, maintenance, time_performed)
	); 
    
GRANT INSERT, SELECT, DELETE, UPDATE on plant_maintenance_log to flowerpower;

-- Editing: fht-backend/db/migrations/1606558487.logstable/upgrade.sql
-- Leave as is for not creating the file
