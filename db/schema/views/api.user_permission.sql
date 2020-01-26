DROP VIEW IF EXISTS api.user_permission;
CREATE VIEW api.user_permission
AS
  SELECT u.user_id,
         u.username,
         p.permission_id,
         p.permission_code
    FROM internal.ac_user u
    JOIN internal.ac_user_role ur
      ON u.user_id = ur.user_id
    JOIN internal.ac_role_permission rp
      ON rp.role_id = ur.role_id
    JOIN internal.ac_permission p
      ON p.permission_id = rp.permission_id
;
