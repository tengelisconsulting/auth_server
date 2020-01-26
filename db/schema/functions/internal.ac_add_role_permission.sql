DROP FUNCTION IF EXISTS internal.ac_add_role_permission;
CREATE FUNCTION internal.ac_add_role_permission(
  p_role_name         TEXT,
  p_permission_code   TEXT
)
RETURNS INTEGER
AS $$
DECLARE
  v_role_id             UUID;
  v_permission_id       UUID;
BEGIN
  SELECT role_id
    INTO v_role_id
    FROM internal.ac_role
   WHERE role_name = p_role_name
  ;
  SELECT permission_id
    INTO v_permission_id
    FROM internal.ac_permission
   WHERE permission_code = p_permission_code
  ;
  INSERT INTO internal.ac_role_permission (
                role_id,
                permission_id
              )
       VALUES (
                v_role_id,
                v_permission_id
              )
  ;
  RETURN 1;
END;
$$
LANGUAGE plpgsql
;
