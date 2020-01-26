DROP FUNCTION IF EXISTS api.check_username_password;
CREATE FUNCTION api.check_username_password(
  IN p_username    TEXT,
  IN p_password    TEXT
)
RETURNS TEXT
AS $$
DECLARE
  v_pw_hash       internal.ac_user.password_hash%TYPE;
  v_user_id       internal.ac_user.user_id%TYPE;
BEGIN
  BEGIN
    SELECT password_hash,
           user_id
      INTO v_pw_hash,
           v_user_id
      FROM internal.ac_user
     WHERE username = p_username
    ;
    EXCEPTION
      WHEN NO_DATA_FOUND THEN
       RETURN '';
  END;
  IF v_pw_hash = crypt(
                   p_password,
                   v_pw_hash
                 ) THEN
    RETURN v_user_id::TEXT;
  END IF;
  RETURN '';
END;
$$
LANGUAGE plpgsql
STABLE
;
