#' ####################################################################### #
#' PROJECT: [Primary Research - Glacial Lake Outflow (GLOF)]
#' CONTENTS:
#'  - NORA3 data retrieval
#'  DEPENDENCIES:
#'  - "glof_after1979.xlsx" in Data subdirectory
#' AUTHOR: [Erik Kusch]
#' ####################################################################### #

# PREAMBLE ================================================================
## Packages ---------------------------------------------------------------
install.load.package <- function(x) {
    if (!require(x, character.only = TRUE)) {
        install.packages(x, repos = "http://cran.us.r-project.org")
    }
    require(x, character.only = TRUE)
}
### CRAN PACKAGES ----
package_vec <- c(
    "readxl", # for excel sheet loading
    "stringdist" # for checking if a variable has already been dealth with
)
sapply(package_vec, install.load.package)

### NON-CRAN ----
if ("ClimHub" %in% rownames(installed.packages()) == FALSE) { # ClimHub check
    devtools::install_github("https://github.com/ErikKusch/ClimHub", force = TRUE)
}
library(ClimHub)

## Directories ------------------------------------------------------------
Dir.Base <- getwd()
Dir.Data <- file.path(Dir.Base, "Data")

# DATA ====================================================================
## GLOF Event Information -------------------------------------------------
# locs_df <- read_xlsx(file.path(Dir, "glof_after1979.xlsx"))
locs_df <- read.csv(file.path(Dir.Data, "glof_after1979_withReprojection.csv"))
colnames(locs_df)[c(12, 13)] <- c("Lon", "Lat")

## Target Variables -------------------------------------------------------
Vars <- c("Precipitation Amount Acc (Height = 0)", "Surface Downwelling Shortwave Flux In Air (Height = 0)", "TS (Surface temperature)", "T2M (2m_Temperature)", "H (Averaged_Sensible_Heat_Flux)", "LE (Averaged_Total_Latent_Heat_Flux)", "GFLUX (Averaged_Ground_Heat_Flux)", "Air Temperature 0m (Height = 0)", "Air Temperature 2m (Height = 2)", "Relative Humidity 2m (Height = 2)", "Rainfall Amount (Height = 0)", "Snowfall Amount (Height = 0)", "Wind Speed (Height = 10)", "Snowfall Amount Acc (Height = 0)")
names(Vars) <- Meta.Variables("NORA3")$datafile[match(Vars, Meta.Variables("NORA3")$name)]
Vars <- Vars[order(names(Vars), decreasing = TRUE)]

## Data Download ----------------------------------------------------------
Times <- locs_df$Periods.of.interest
for (Loc in 1:nrow(locs_df)) {
    print(paste("####### ", Loc, "/", nrow(locs_df)))
    for (VarPos in 1:length(Vars)) {
        Variable <- Vars[VarPos]
        print(paste("----- ", Variable))
        # Split into two dates
        Duration <- strsplit(Times[Loc], "-", fixed = TRUE)[[1]]
        # Convert each part to Date objects
        dates <- as.Date(Duration, format = "%d.%m.%Y")
        # Format into ISO without separators
        new_str <- paste(format(dates, "%Y%m%d"), collapse = "-")

        Name <- paste(locs_df$Glacier.name[Loc], "-", Variable)

        FinalFile <- file.path(Dir, paste0(paste(locs_df$Glacier.name[Loc], new_str, sep = "_"), ".csv"))
        if (file.exists(FinalFile)) {
            CSVcontent <- read.csv(FinalFile)[, -1]
            colnames(CSVcontent)

            clean_string <- function(x) {
                x |>
                    tolower() |>
                    gsub("[[:punct:]]", " ", x = _) |> # remove punctuation
                    gsub("\\s+", " ", x = _) |> # collapse multiple spaces
                    trimws()
            }

            similarity <- function(a, b) {
                a_clean <- clean_string(a)
                b_clean <- clean_string(b)
                dist <- stringdist(a_clean, b_clean, method = "lv") # Levenshtein distance
                max_len <- max(nchar(a_clean), nchar(b_clean))
                (1 - dist / max_len) * 100
            }

            if (any(sapply(colnames(CSVcontent), similarity, b = Variable) > 90)) {
                message("Already prepared")
                next()
            }
        }
        # print(paste(Name, new_str))

        Start <- paste(as.Date(Duration[1], format = c("%d.%m.%Y")), "00")
        Stop <- paste(as.Date(Duration[2], format = c("%d.%m.%Y")), "18")

        MetNo_rast <- Download.NORA3(
            Variable = Variable,
            DateStart = Start,
            DateStop = Stop,
            Leadtime = 9,
            Dir = Dir,
            Cores = 1, # 1
            FileName = "Temporary.nc",
            WriteFile = FALSE
        )

        Meta_vec <- terra::metags(MetNo_rast)

        # reproj <- locs_df[,c(1,6,7)]
        # colnames(reproj) <- c("loc", "Lat", "Lon")
        # locs_df <- cbind(locs_df, sf::st_coordinates(Spatial.Reproject(Spatial.MakePoints(reproj), MetNo_rast)))
        # write.csv(locs_df, file = file.path(Dir.Data, "glof_after1979_withReprojection.csv"))
        # read.csv(file.path(Dir.Data, "glof_after1979_withReprojection.csv"))


        # PointLoc <- Spatial.MakePoints(locs_df[Loc, ])

        # PointBuffer <- Spatial.Buffer(PointLoc, Buffer = 3e4)

        # PointBuffer <- Spatial.Reproject(PointBuffer, MetNo_rast)

        # MetNo_rast <- Spatial.CropMask(MetNo_rast, PointBuffer)

        # MetNo_rast <- Spatial.Reproject(MetNo_rast, PointLoc)

        # ## extract data to csv and save raw file
        # MetNo_rast <- WriteRead.NC(
        #     NC = MetNo_rast,
        #     Variable = Variable,
        #     Unit = unique(terra::units(MetNo_rast)),
        #     FName = FinalFile,
        #     Attrs = Meta_vec,
        #     Write = TRUE,
        #     Compression = 9
        # )

        extracted <- data.frame(
            Time = terra::time(MetNo_rast),
            Val = t(terra::extract(x = MetNo_rast, y = locs_df[Loc, c("Lon", "Lat")])[1, -1])[, 1]
        )
        rownames(extracted) <- c()
        colnames(extracted)[2] <- Variable

        if (file.exists(FinalFile)) {
            CSVcontent <- read.csv(FinalFile)[, -1]
            SAVEcontent <- cbind(CSVcontent, extracted[, 2])
            colnames(SAVEcontent)[ncol(SAVEcontent)] <- Variable
            write.csv(SAVEcontent, file = FinalFile)
        } else {
            write.csv(extracted, file = FinalFile)
        }

        if (VarPos == length(Vars)) {
            unlink(list.files(Dir.Data, pattern = paste0(names(Vars)[VarPos], ".nc"), full.names = TRUE))
        } else {
            if (names(Vars)[VarPos] != names(Vars)[VarPos + 1]) {
                print(names(Vars)[VarPos])
                print(names(Vars)[VarPos + 1])
                print("Deleting")
                unlink(list.files(Dir.Data, pattern = paste0(names(Vars)[VarPos], ".nc"), full.names = TRUE))
            }
        }
    }
}

Done <- "I am done"
