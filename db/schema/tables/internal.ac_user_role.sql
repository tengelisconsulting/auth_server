CREATE TABLE IF NOT EXISTS internal.ac_user_role (
  user_id             UUID           NOT NULL
    REFERENCES internal.ac_user (user_id),
  role_id             UUID           NOT NULL
    REFERENCES internal.ac_role (role_id),

  PRIMARY KEY (user_id, role_id)
);

COMMENT ON TABLE internal.ac_user_role IS 'Mapping of user to role';
