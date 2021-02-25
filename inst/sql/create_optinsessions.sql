-- !preview conn=DBI::dbConnect(RPostgres::Postgres(), host="127.0.0.1", dbname = "api", user = "www-data")

CREATE TABLE IF NOT EXISTS optinsessions (
  status INT NOT NULL,
  ts_gmt TIMESTAMP NOT NULL,
  session_id VARCHAR ( 255 ) UNIQUE,
  file_name VARCHAR ( 50 ) UNIQUE,
  get_path VARCHAR ( 255 ) UNIQUE
);





