# By default, use db containing only publicly available resources
ALL_LKPS_MAPS_DB <- "./all_lkps_maps.db"

# User can supply their own db by mounting a folder called `all_lkps_maps`
# containing `all_lkps_maps.db`, to the container root directory
USER_SUPPLIED_DB_PATH <- "/all_lkps_maps/all_lkps_maps.db"
if (file.exists(USER_SUPPLIED_DB_PATH)) {
  ALL_LKPS_MAPS_DB <- USER_SUPPLIED_DB_PATH
}

# Run app
codemapper::RunCodelistBuilder(all_lkps_maps = ALL_LKPS_MAPS_DB)
