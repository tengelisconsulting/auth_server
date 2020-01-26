DROP FUNCTION IF EXISTS internal.ac_add_role;
CREATE FUNCTION internal.ac_add_role(
  IN p_role_name          TEXT,
  IN p_role_description   TEXT          DEFAULT NULL
)
RETURNS UUID
AS $$
DECLARE
  v_role_id         UUID        := uuid_generate_v1mc();
BEGIN
  INSERT INTO internal.ac_role (
                role_id,
                role_name,
                role_description
              )
       VALUES (
                v_role_id,
                p_role_name,
                p_role_description
              )
  ;
  RETURN v_role_id;
END;
$$
LANGUAGE plpgsql
;
