DROP FUNCTION IF EXISTS internal.ac_add_user;
CREATE FUNCTION internal.ac_add_user(
  IN   p_username      TEXT,
  IN   p_password      TEXT
)
RETURNS UUID
AS $$
DECLARE
  v_user_id     UUID    := uuid_generate_v1mc();
BEGIN
  INSERT INTO internal.ac_user (
                user_id,
                username,
                password_hash
              )
       VALUES (
                v_user_id,
                p_username,
                crypt(p_password, gen_salt('bf'))
              )
  ;
  RETURN v_user_id;
END;
$$
LANGUAGE plpgsql
;
