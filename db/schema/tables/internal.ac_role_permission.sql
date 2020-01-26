CREATE TABLE IF NOT EXISTS internal.ac_role_permission (
  role_id             UUID           NOT NULL
    REFERENCES internal.ac_role (role_id)
    ON DELETE CASCADE,
  permission_id       UUID           NOT NULL
    REFERENCES internal.ac_permission (permission_id)
    ON DELETE CASCADE,

  PRIMARY KEY (role_id, permission_id)
);

COMMENT ON TABLE internal.ac_role_permission IS 'Mapping of role to permission';
