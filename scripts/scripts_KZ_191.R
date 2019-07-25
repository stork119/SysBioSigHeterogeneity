### ###
### script_KZ_191
### ###


# source("scripts/scripts_initialisation.R")
source("scripts/scripts_libraries.R")

#### preparation of data ####
### !!!
experiment.id <- "KZ_191"

output.dir <- paste("resources", "output", "JAK", experiment.id, sep = "/")
dir.create(output.dir, recursive = TRUE, showWarnings = FALSE)
inhibitor.input.dir <- paste("resources", "input", "JAK", sep = "/")
data.raw <- (read.table(paste(inhibitor.input.dir, experiment.id, "ShrinkedNuclei.csv", sep="/"),
                        header=TRUE, sep=",") %>%
               SysBioSigTheme::normalize_data())$data %>%
  data.table::data.table()
data.filtered <- data.raw
data.all <- data.filtered
#### choice of data set and parameters  ####
### !!!
quantile.prob = 0.1 ### choose probability

### !!! other parameters
signal_ <- "stimulation.1.2"

### !!! choose x scale
# rescale.fun = "factor"
# rescale.fun.args = list()

rescale.fun = function(x, base){log(x = x, base = base)}
rescale.fun.args = list(base = exp(1))

### !!! inhibitors
inhibitors.list <- c("inhibitor.1.1","inhibitor.1.2",  "inhibitor.1.3")
inhibitors.names <- c("JAK1", "JAK2", "TYK2")
### columns used for grouping data samples e.g. signal
distincting.colnames <- c(signal_, inhibitors.list)

### !!!
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
    SysBioSigHeterogeneity::ComputeDistinctQuantiles(
      data.control = data.control,
      data.saturation = data.saturation,
      response = response_,
      signal = signal_,
      quantile.prob = quantile.prob
    )

  ### CountResponseFractions ###
  data.tiles <-
    SysBioSigHeterogeneity::CountResponseFractions(
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
plot.list <- list()
plot.grid.list <- list()
inhibitor.i <- 1
for(inhibitor.i in 1:length(inhibitors.list)){
  inhibitor_ <- inhibitors.list[inhibitor.i]
  inhibitor.name <- inhibitors.names[inhibitor.i]
  inhibitors.filter <-
    paste("c(",
    paste(
      inhibitors.list[-which(inhibitors.list %in% inhibitor_)],
      "== 0", collapse = "& "),
    ")", sep = "")

  plot.list[[inhibitor_]] <- list()
  plot.list[[inhibitor_]][["pS1"]] <-
    SysBioSigHeterogeneity::plotCompletePartialLine(
      data =
        data.tiles %>%
        dplyr::filter_(paste(signal_, "== max(", signal_,")")) %>%
        dplyr::filter_(inhibitors.filter),
      title = paste(inhibitor.name, "pS1"),
      x_ =  inhibitor_,
      xlab_ = inhibitor.name,
      y_ = "pS1",
      rescale.fun = rescale.fun,
      rescale.fun.args = rescale.fun.args
    )

  plot.list[[inhibitor_]][["pS3"]] <-
    SysBioSigHeterogeneity::plotCompletePartialLine(
      data =
        data.tiles %>%
        dplyr::filter_(paste(signal_, "== max(", signal_,")")) %>%
        dplyr::filter_(inhibitors.filter),
      title = paste(inhibitor.name, "pS3"),
      x_ = inhibitor_,
      xlab_ = inhibitor.name,
      y_ = "pS3",
      rescale.fun = rescale.fun,
      rescale.fun.args = rescale.fun.args
    )
  plot.list[[inhibitor_]][["pS1-pS3"]] <-
    SysBioSigHeterogeneity::plotCompletePartialLine(
    data =
      data.tiles %>%
      dplyr::filter_(paste(signal_, "== max(", signal_,")")) %>%
      dplyr::filter_(inhibitors.filter),
    title_ = paste(inhibitor.name, "pS1-pS3"),
    x_ = inhibitor_,
    xlab_ = inhibitor.name,
    y_ = "pS1 - pS3",
    ylim_ = c(-1,1),
    rescale.fun = rescale.fun,
    rescale.fun.args = rescale.fun.args
  )
  plot.grid.list[[inhibitor_]] <-
    cowplot::plot_grid(plotlist = plot.list[[inhibitor_]], nrow = 1)
}
plot.grid <-
  cowplot::plot_grid(plotlist = plot.grid.list, ncol = 1)

ggsave(filename = paste(output.dir, paste0("one_dimensional_fractions.pdf"), sep = "/"),
       plot = plot.grid,
       width = 12,
       height = 12)
