CREATE SCHEMA IF NOT EXISTS public;
SET search_path TO public;

CREATE TABLE tokens
( token varchar PRIMARY KEY
, layers text[] default '{}'::text[]
, created_at TIMESTAMP WITH TIME ZONE NOT NULL DEFAULT clock_timestamp()
, modified_at TIMESTAMP WITH TIME ZONE NOT NULL DEFAULT clock_timestamp()
);

CREATE INDEX token_idx ON tokens (token);

CREATE OR REPLACE FUNCTION update_row_modified_at_function()
RETURNS TRIGGER
AS
  $$
  BEGIN
    -- ASSUMES the table has a column named exactly "modified_at".
    -- Fetch date-time of actual current moment from clock,
    -- rather than start of statement or start of transaction.
    NEW."modified_at" := clock_timestamp();
    RETURN NEW;
  END;
  $$
LANGUAGE 'plpgsql';

CREATE TRIGGER tokens_update_modified_at BEFORE UPDATE
   ON tokens
   FOR EACH ROW
   EXECUTE PROCEDURE update_row_modified_at_function();