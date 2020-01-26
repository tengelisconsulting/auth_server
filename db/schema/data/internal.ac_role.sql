DO $$
BEGIN
  BEGIN
    PERFORM internal.ac_add_role('ADMIN', 'app admin');
    EXCEPTION
      WHEN unique_violation THEN
        NULL;
  END;
  BEGIN
    PERFORM internal.ac_add_role('BASE_USER', 'base user');
    EXCEPTION
      WHEN unique_violation THEN
        NULL;
  END;
END
$$;
