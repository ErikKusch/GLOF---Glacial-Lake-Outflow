while(!exists("Done")){
    try(source("NACGLOF.r"))
    closeAllConnections()
}


# library(doSNOW)
# library(parallel)

# checkme <- function(Cores){

# cl <- parallel::makeCluster(2)
# doSNOW::registerDoSNOW(cl)
# stopCluster(cl)
# print("Stopping")
# return(1+1)
# }


