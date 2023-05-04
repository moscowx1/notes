-- liquibase formatted sql
-- changeset santra:1683128760620-1
CREATE TABLE "user" (
  "id" BIGINT GENERATED BY DEFAULT AS IDENTITY (START WITH 16) NOT NULL,
  "login" VARCHAR NOT NULL,
  "salt" BYTEA NOT NULL,
  "password" BYTEA NOT NULL,
  "created_at" TIMESTAMP WITH TIME ZONE DEFAULT NOW() NOT NULL,
  CONSTRAINT "user_pkey" PRIMARY KEY ("id")
);
CREATE TABLE "note" (
  "id" BIGINT GENERATED BY DEFAULT AS IDENTITY NOT NULL,
  "content" VARCHAR NOT NULL,
  "author" BIGINT NOT NULL,
  CONSTRAINT "note_pkey" PRIMARY KEY ("id")
);

ALTER TABLE "user"
ADD CONSTRAINT "unique_login" UNIQUE ("login");

ALTER TABLE "note"
ADD CONSTRAINT "note_author_fkey" FOREIGN KEY ("author") REFERENCES "user" ("id") ON UPDATE RESTRICT ON DELETE RESTRICT;