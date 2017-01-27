/******************** Purge the database  ************************/
DROP TABLE IF EXISTS "episodes" CASCADE;
DROP TABLE IF EXISTS "demographic" CASCADE;
DROP TABLE IF EXISTS "physiology" CASCADE;

DROP SEQUENCE IF EXISTS "episode_seq";
CREATE SEQUENCE "episode_seq" INCREMENT BY 1;
DROP SEQUENCE IF EXISTS "demographic_seq";
CREATE SEQUENCE "demographic_seq" INCREMENT BY 1;
DROP SEQUENCE IF EXISTS "physiology_seq";
CREATE SEQUENCE "physiology_seq" INCREMENT BY 1;

--ALTER TABLE "episodes" DROP CONSTRAINT IF EXISTS pkepisodes;
/******************** Add Table: "episodes" ************************/
/* Build Table Structure */
CREATE TABLE "episodes"
(
  "Id" INTEGER DEFAULT nextval('episode_seq'::regclass) NOT NULL,
  nhs_number CHAR(200) NULL,
  pas_number CHAR(200) NULL,
  site_id CHAR(200) NULL,
  local_id CHAR(200) NULL
);

/* Add Primary Key */
ALTER TABLE "episodes" ADD CONSTRAINT pkepisodes
PRIMARY KEY ("Id");

/******************** Add Table: "demographic" ************************/
/* Build Table Structure */
CREATE TABLE "demographic"
(
  "Id" INTEGER DEFAULT nextval('demographic_seq'::regclass) NOT NULL,
  item CHAR(100) NOT NULL,
  value CHAR(100) NULL,
  episode_id INTEGER NOT NULL
);

/* Add Primary Key */
ALTER TABLE "demographic" ADD CONSTRAINT pkdemographic
PRIMARY KEY ("Id");

/******************** Add Table: "physiology" ************************/
/* Build Table Structure */
CREATE TABLE "physiology"
(
  "Id" INTEGER DEFAULT nextval('physiology_seq'::regclass) NOT NULL,
  item CHAR(100) NOT NULL,
  value CHAR(100) NULL,
  episode_id INTEGER NOT NULL,
  time_stamp TIMESTAMP NOT NULL
);

/* Add Primary Key */
ALTER TABLE "physiology" ADD CONSTRAINT pkphysiology
PRIMARY KEY ("Id");



/************ Add Foreign Keys ***************/

/* Add Foreign Key: fk_demographic_episodes */
ALTER TABLE "demographic" ADD CONSTRAINT fk_demographic_episodes
FOREIGN KEY (episode_id) REFERENCES "episodes" ("Id")
ON UPDATE NO ACTION ON DELETE NO ACTION;
