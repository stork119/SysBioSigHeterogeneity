### ###
### initialisation
### ###

warning("Set directory to folder SysBioSigHeterogeneity")
dir <- getwd() ## if not change dir

source(paste(dir, "scripts/scripts_libraries.R", sep = "/"))
sapply(list.files(path = paste(dir, "R", sep = "/"),
                  full.names = TRUE), source) -> r
rm(r)

