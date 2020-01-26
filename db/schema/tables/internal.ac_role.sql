CREATE TABLE IF NOT EXISTS internal.ac_role (
  role_id           UUID                NOT NULL
    DEFAULT uuid_generate_v1mc(),
  role_name         TEXT                NOT NULL
    CHECK (length(role_name) < 512),
  role_description  TEXT,

  PRIMARY KEY (role_id),
  UNIQUE (role_name)
);

COMMENT ON TABLE internal.ac_role IS 'A named collection of permissions';
