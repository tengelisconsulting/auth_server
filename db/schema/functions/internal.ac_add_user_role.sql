DROP FUNCTION IF EXISTS internal.ac_add_user_role;
CREATE FUNCTION internal.ac_add_user_role(
  IN   p_username      TEXT,
  IN   p_role_name     TEXT
)
RETURNS INTEGER
AS $$
DECLARE
  v_user_id     UUID;
  v_role_id     UUID;
BEGIN
  SELECT user_id
    INTO v_user_id
    FROM internal.ac_user
   WHERE username = p_username
  ;
  SELECT role_id
    INTO v_role_id
    FROM internal.ac_role
   WHERE role_name = p_role_name
  ;

  INSERT INTO internal.ac_user_role (
                user_id,
                role_id
              )
       VALUES (
                v_user_id,
                v_role_id
              )
  ;
  RETURN 1;
END;
$$
LANGUAGE plpgsql
;
