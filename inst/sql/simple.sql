DROP TABLE IF EXISTS "demographic" CASCADE;
DROP TABLE IF EXISTS "measurement" CASCADE;

DROP SEQUENCE IF EXISTS "episode_seq";
CREATE SEQUENCE "episode_seq" INCREMENT BY 1;
DROP SEQUENCE IF EXISTS "demographic_seq";
CREATE SEQUENCE "demographic_seq" INCREMENT BY 1;
DROP SEQUENCE IF EXISTS "measurement_seq";
CREATE SEQUENCE "measurement_seq" INCREMENT BY 1;

CREATE TABLE "measurement"
(
  "Id" INTEGER DEFAULT nextval('measurement_seq'::regclass) NOT NULL,
  item CHAR(100) NOT NULL,
  value CHAR(100) NULL,
  episode_id INTEGER NOT NULL,
  time_stamp TIMESTAMP NOT NULL
);


/******************** Add Table: "demographic" ************************/
CREATE TABLE "demographic"
(
  "Id" INTEGER DEFAULT nextval('demographic_seq'::regclass) NOT NULL,
  item CHAR(100) NOT NULL,
  value CHAR(100) NULL,
  episode_id INTEGER NOT NULL
);


/*CREATE TABLE "identifiers"
(
  "Id" INTEGER DEFAULT nextval('episode_seq'::regclass) NOT NULL,

);*/
