CREATE TABLE IF NOT EXISTS internal.ac_permission (
  permission_id      UUID              NOT NULL
    DEFAULT uuid_generate_v1mc(),
  permission_code    TEXT              NOT NULL
    CHECK (length(permission_code) < 512),

  PRIMARY KEY (permission_id),
  UNIQUE (permission_code)
);

COMMENT ON TABLE internal.ac_permission IS 'A permission';
