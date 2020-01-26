DO $$
BEGIN
  DELETE FROM internal.ac_permission;
  INSERT INTO internal.ac_permission (
                permission_code
              )
       VALUES ( 'app_owner' ),
              ( 'test_permission' )
  ;
END
$$;
