while (!exists("Done")) {
    try(source("NACGLOF.r"))
    unlink(file.path(Dir.Data, pattern = "TEMP_"))
    closeAllConnections()
}
