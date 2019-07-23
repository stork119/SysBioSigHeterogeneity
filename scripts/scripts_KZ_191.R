### ###
### script_KZ_191
### ###


source("scripts/scripts_initialisation.R")
source("scripts/scripts_libraries.R")
#library(SysBioSigHeterogeneity)

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
plot.list <- list()
plot.grid.list <- list()
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
    plotCompletePartialLine(
      data =
        data.tiles %>%
        dplyr::filter_(paste(signal_, "== max(", signal_,")")) %>%
        dplyr::filter_(inhibitors.filter),
      title = paste(inhibitor.name, "pS1"),
      x_ = paste("factor(", inhibitor_, ")")
    )

  plot.list[[inhibitor_]][["pS3"]] <-
    plotCompletePartialLine(
      data =
        data.tiles %>%
        dplyr::filter_(paste(signal_, "== max(", signal_,")")) %>%
        dplyr::filter_(inhibitors.filter),
      title = paste(inhibitor.name, "pS3"),
      x_ = paste("factor(", inhibitor_, ")"),
      y_ = "pS3"
    )
  plot.list[[inhibitor_]][["pS1-pS3"]] <-
    plotCompletePartialLine(
    data =
      data.tiles %>%
      dplyr::filter_(paste(signal_, "== max(", signal_,")")) %>%
      dplyr::filter_(inhibitors.filter),
    title_ = paste(inhibitor.name, "pS1-pS3"),
    x_ = paste("factor(", inhibitor_, ")"),
    y_ = "pS1 - pS3",
    ylim_ = c(-1,1)
  )
  plot.grid.list[[inhibitor_]] <-
    cowplot::plot_grid(plotlist = plot.list[[inhibitor_]], nrow = 1)
}
plot.grid <-
  cowplot::plot_grid(plotlist = plot.grid.list, ncol = 1)

ggsave(filename = paste(output.dir, paste0("one_dimensional_fractions.pdf"), sep = "/"),
       plot = plot.grid,
       width = 8,
       height = 12)



#
# expand.grid(x = 1:length(inhibitors.list),
#             y = 1:length(inhibitors.list)) %>%
#   dplyr::filter(x < y) ->
#   combinations.df
#
# foreach(combination.i = 1:nrow(combinations.df)) %do% {
#   axes.x.i <- combinations.df[combination.i,]$x
#   axes.y.i <- combinations.df[combination.i,]$y
#
#   axes.x <- inhibitors.list[axes.x.i]
#   axes.y <- inhibitors.list[axes.y.i]
#   axes.x.name <- inhibitors.names[axes.x.i]
#   axes.y.name <- inhibitors.names[axes.y.i]
#
#   title_ <-
#     paste(title.main, ":",
#           paste(inhibitors.names[-which(inhibitors.list %in% c(axes.x,axes.y))],
#                 "= 0"))
#
#
#   ggplot(data =
#            data.tiles %>%
#            dplyr::filter(stimulation.1.2 != 0) %>%
#            dplyr::filter_(
#              paste(inhibitors.list[-which(inhibitors.list %in% c(axes.x,axes.y))],
#                    " == 0")),
#          mapping = aes_string(
#            x = paste("factor(", axes.x, ")"),
#            y = paste("factor(", axes.y, ")"),
#            fill = "prob",
#            label = "round(prob, 2)"
#          )) +
#     geom_tile(color = "white") +
#     viridis::scale_fill_viridis(
#       name = "Cells fraction",
#       limits = color.limits) +
#     ggtitle(title_)+
#     xlab(axes.x.name)+
#     ylab(axes.y.name)+
#     SysBioSigTheme::theme_sysbiosig()+
#     geom_text(size = 4) +
#     facet_grid("~type")
#
# } ->
#   g.inhibitors.list
#
# g.inhibitor <-
#   cowplot::plot_grid(
#     plotlist = g.inhibitors.list,
#     ncol = 1)
#
# ggsave(filename = paste(output.dir, paste0(title.main, "_fractions.pdf"), sep = "/"),
#        plot = g.inhibitor,
#        width = 8,
#        height = 12)
