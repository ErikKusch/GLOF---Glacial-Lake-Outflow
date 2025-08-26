while (!exists("Done")) {
    try(source("NORA3-Data.r"))
    unlink(list.files(Dir.Data, pattern = "TEMP_", full.names = TRUE))
    closeAllConnections()
    Sys.sleep(60) # sleep for an hour - likely, thredds is having an issue: https://status.met.no/
}