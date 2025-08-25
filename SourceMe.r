while (!exists("Done")) {
    try(source("NORA3-Data.r"))
    unlink(list.files(Dir.Data, pattern = "TEMP_", full.names = TRUE))
    closeAllConnections()
    Sys.sleep(60*60) # sleep for an hour - likely, thredds is having an issue: https://status.met.no/
}



# data_in <- readRDS("/div/no-backup-nac/PATHFINDER/EMULATOR-DATA/Data_1km_df.rds")

# head(data_in)

# locs2015 <- which(substr(data_in$YEAR_MONTH, 1, 4) == "2015")


# data_2015 <- data_in[locs2015, ]

# head(data_2015)

# library(dplyr)

# summary_df <- data_2015 %>%
#   group_by(CELL) %>%
#   summarise(
#     mean_mean = mean(mean, na.rm = TRUE),
#     LONGITUDE = first(LONGITUDE),
#     LATITUDE = first(LATITUDE),
#     ELEVATION = first(ELEVATION),
#     AGB_ESA = first(AGB_ESA)
#   )

# head(summary_df)

# write.csv(summary_df, file = "/div/no-backup-nac/PATHFINDER/EMULATOR-DATA/FSCVE_2015.csv")
