library(codemapper)
db_path <- "all_lkps_maps.db"

all_lkps_maps_to_db(db_path = db_path)

RunCodelistBuilder(all_lkps_maps = db_path,
                   options = NULL)
