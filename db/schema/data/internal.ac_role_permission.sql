DO $$
BEGIN
  DELETE FROM internal.ac_role_permission;
  PERFORM internal.ac_add_role_permission('ADMIN', 'app_owner');
END
$$;
