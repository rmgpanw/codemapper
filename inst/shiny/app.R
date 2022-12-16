library(codemapper)
library(snomedizer)

snomedizer::snomedizer_options_set(
  endpoint = "https://snowstorm.ihtsdotools.org/snowstorm/snomed-ct",
  branch = "MAIN/2021-07-31"
)


RunCodelistBuilder(all_lkps_maps = "all_lkps_maps.db",
                   options = NULL)
