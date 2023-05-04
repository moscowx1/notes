--liquibase formatted sql
--changeset santra:1683206267-2
ALTER TABLE "note"
ADD COLUMN "title" VARCHAR NOT NULL;
--rollback ALTER TABLE "note"DROP COLUMN "title";