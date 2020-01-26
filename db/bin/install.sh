#!/bin/sh


if [ "${DEF_BASE_DIR}" = "" ]; then
    echo "supply DEF_BASE_DIR"
    exit 1
fi

execute_all_sql_at_root() {
    for f in $(find "${1}" -name '*.sql'); do
        echo "${f}"
        psql -h ${PGHOST} -U ${PGUSER} -f "${f}"
    done

}

c() {
    psql -h ${PGHOST} -U ${PGUSER} -f "${DEF_BASE_DIR}/${1}"
}


execute_all_sql_at_root ${DEF_BASE_DIR}/extensions
execute_all_sql_at_root ${DEF_BASE_DIR}/schemas
execute_all_sql_at_root ${DEF_BASE_DIR}/tables
execute_all_sql_at_root ${DEF_BASE_DIR}/views
execute_all_sql_at_root ${DEF_BASE_DIR}/functions
execute_all_sql_at_root ${DEF_BASE_DIR}/data
