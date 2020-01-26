DROP VIEW IF EXISTS api.user_permission_list;
CREATE VIEW api.user_permission_list
AS
    SELECT up.user_id,
           array_agg(up.permission_code) permissions
      FROM api.user_permission up
  GROUP BY up.user_id
;
