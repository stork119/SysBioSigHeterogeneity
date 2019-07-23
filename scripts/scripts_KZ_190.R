### ###
### JAK inhibitors KZ 190
### ###

source("scripts/scripts_initialisation.R")
source("scripts/scripts_libraries.R")
#library(SysBioSigHeterogeneity)

#### preparation of data ####
experiment.id <- "KZ_190"

output.dir <- paste("resources", "output", "JAK", experiment.id, sep = "/")
dir.create(output.dir, recursive = TRUE, showWarnings = FALSE)

inhibitor.input.dir <- paste("resources", "input", "JAK", sep = "/")

data.raw <- (read.table(paste(inhibitor.input.dir, experiment.id, "ShrinkedNuclei.csv", sep="/"),
                    header=TRUE, sep=",") %>%
  SysBioSigTheme::normalize_data())$data %>%
  data.table::data.table()
data.filtered <- subset(data.raw, !data.raw$well.name %in%
                          c("A02", "B02", "B03",
                            "B10", "B13", "C10", "C12", "C13", "D02", "D09", "D12", "E06", "E13",
                            "F01", "F12", "A17", "A18", "B17", "B18", "C17", "C18"#,
                            #"D17", "D18"
                            ))
data.all <- data.filtered

#
# data.filtered.ps1 <- subset(data.filtered, data.filtered$antibody.1.1=="1:100")
# data.filtered.pS3 <- subset(data.filtered, data.filtered$antibody.1.2=="1:200")

#### choice of data set and parameters  ####
### !!!
quantile.prob = 0.1 ### choose probability

### !!! choose data

# ## ps3
# title.main <- "ps3"
# data.all <- data.filtered.pS3
# response_ <-  "Intensity_MeanIntensity_Alexa555" ### column with response
#
# ## ps1
# title.main <- "ps1"
# data.all <- data.filtered.ps1
# response_ <-  "Intensity_MeanIntensity_Alexa488"

### !!! other parameters
signal_ <- "stimulation.1.2"

### !!! inhibitors
inhibitors.list <- c("inhibitor.3.1","inhibitor.3.2",  "inhibitor.3.3")
inhibitors.names <- c("JAK1", "JAK2", "TYK2")
### columns used for grouping data samples e.g. signal
distincting.colnames <- c(signal_, inhibitors.list)

color.limits <- c(0,1)

#### Finding distinct quantiles ####
response.df <-
  data.frame(
    response.type = c("pS1", "pS3"),
    response = c("Intensity_MeanIntensity_Alexa488", "Intensity_MeanIntensity_Alexa555"),
    stringsAsFactors = FALSE)
response.type.list <- response.df$response.type

data.control <- data.all %>% dplyr::filter(stimulation.1.2 == 0)
data.saturation <- data.all %>%
  dplyr::filter(stimulation.1.2 == max(stimulation.1.2)) %>%
  dplyr::filter(inhibitor.1.1 == 0 ) %>%
  dplyr::filter(inhibitor.1.2 == 0 )  %>%
  dplyr::filter(inhibitor.1.3 == 0 )
### !!! choose data
foreach(response.i = 1:nrow(response.df)) %do% {
  response.type_ <- response.df[response.i,]$response.type
  response_ <-   response.df[response.i,]$response

  color.limits <- c(0,1)


  ### Finding distinct quantiles ###
  bounds.df <-
    ComputeDistinctQuantiles(
      data.control = data.control,
      data.saturation = data.saturation,
      response = response_,
      signal = signal_,
      quantile.prob = quantile.prob
    )

  ### CountResponseFractions ###
  data.tiles <-
    CountResponseFractions(
      data = data.all,
      bounds.df = bounds.df,
      response = response_,
      signal = signal_,
      distincting.colnames = distincting.colnames
    ) %>%
    dplyr::mutate(response.type = response.type_)
  return(data.tiles)
} %>%
  do.call(what = rbind,
          args = .)  %>%
  reshape2::dcast(
    formula =
      paste(
        paste(c(distincting.colnames,"type"), collapse = "+"),
        "~", "response.type", sep = ""),
    value.var = "prob") ->
  data.tiles
saveRDS(file =  paste(output.dir, paste0("ResponseFractions.rds"), sep = "/"),
        object = data.tiles)
#### plotting ####
title.main <- ""
expand.grid(x = 1:length(inhibitors.list),
            y = 1:length(inhibitors.list),
            response.type = c("pS1", "pS3", "pS1 - pS3"),
            stringsAsFactors = FALSE) %>%
  dplyr::filter(x < y) ->
  combinations.df

foreach(combination.i = 1:nrow(combinations.df)) %do% {
  axes.x.i <- combinations.df[combination.i,]$x
  axes.y.i <- combinations.df[combination.i,]$y

  axes.x <- inhibitors.list[axes.x.i]
  axes.y <- inhibitors.list[axes.y.i]
  axes.x.name <- inhibitors.names[axes.x.i]
  axes.y.name <- inhibitors.names[axes.y.i]

  response.type_ <- combinations.df[combination.i,]$response.type
  if(!(response.type_ %in% response.type.list)){
    color.limits <- c(-1,1)
  }
  title_ <-
    paste(response.type_, ":",
          paste(inhibitors.names[-which(inhibitors.list %in% c(axes.x,axes.y))],
                "= 0"))
  ggplot(data =
           data.tiles %>%
           dplyr::filter(stimulation.1.2 != 0) %>%
           dplyr::filter_(
             paste(inhibitors.list[-which(inhibitors.list %in% c(axes.x,axes.y))],
                   " == 0")),
         mapping = aes_string(
           x = paste("factor(", axes.x, ")"),
           y = paste("factor(", axes.y, ")"),
           fill = response.type_,
           label = paste("round(", response.type_, ", 2)")
         )) +
    geom_tile(color = "white") +
    viridis::scale_fill_viridis(
      name = "Cells fraction",
      limits = color.limits) +
    ggtitle(title_)+
    xlab(axes.x.name)+
    ylab(axes.y.name)+
    SysBioSigTheme::theme_sysbiosig()+
    geom_text(size = 4) +
    facet_grid("~type")
} ->
  g.inhibitors.list

g.inhibitor <-
  cowplot::plot_grid(
    plotlist = g.inhibitors.list,
    ncol = 1)

ggsave(filename = paste(output.dir, paste0("tiles_fractions.pdf"), sep = "/"),
       plot = g.inhibitor,
       width = 8,
       height = 24)
