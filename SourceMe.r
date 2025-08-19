while (!exists("Done")) {
    try(source("NORA3-Data.r"))
    unlink(file.path(Dir.Data, pattern = "TEMP_"))
    closeAllConnections()
}
