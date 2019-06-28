### ###
### initialisation
### ###

warning("Set directory to folder SysBioSigHeterogeneity")
dir <- getwd() ## if not change dir

source(paste(dir, "scripts/scripts_libraries.R", sep = "/"))
sapply(list.files(path = paste(dir, "R/"),
                  full.names = TRUE), source) -> r
rm(r)

