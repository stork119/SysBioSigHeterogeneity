### ###
### JAK inhibitors KZ 190 KOSTKA
### ###

source("scripts/scripts_libraries.R")

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
  response.type_ = combinations.df[combination.i,]$response.type
  if(!(response.type_ %in% response.type.list)){
    color.limits.tmp <- c(-1,1)
    scale_colors_fun <-
      viridis::scale_fill_viridis(
        name = "Cells fraction",
        limits = color.limits.tmp, option = "A")
  } else {
    scale_colors_fun <-
      viridis::scale_fill_viridis(
        name = "Cells fraction",
        limits = color.limits.tmp)
    color.limits.tmp <- color.limits
  }

  SysBioSigHeterogeneity::plotCompletePartialCube(
    data =
      data.tiles %>%
      dplyr::filter(stimulation.1.2 != 0) %>%
      dplyr::filter_(
        paste(inhibitors.list[-which(inhibitors.list %in%
                                       c(inhibitors.list[axes.x.i],
                                         inhibitors.list[axes.y.i]))],
              " == 0")),
    axes.x = inhibitors.list[axes.x.i],
    axes.y = inhibitors.list[axes.y.i],
    axes.x.name = inhibitors.names[axes.x.i],
    axes.y.name = inhibitors.names[axes.y.i],
    response.type_ = combinations.df[combination.i,]$response.type,
    color.limits = color.limits.tmp,
    scale_colors_fun = scale_colors_fun,
    title_ =
      paste(response.type_, ":",
            paste(inhibitors.names[-which(inhibitors.list %in% c(inhibitors.list[axes.x.i],
                                                                 inhibitors.list[axes.y.i]))],
                  "= 0"))
  )
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

#### curves lines ####
expand.grid(
  inhibitor.one = 1:length(inhibitors.list),
  inhibitor.two = 1:length(inhibitors.list),
  stringsAsFactors = FALSE
) %>% dplyr::filter(
  inhibitor.one < inhibitor.two
) -> inhibitor.pairs.df

foreach(inhibitor.pair.i = 1:nrow(inhibitor.pairs.df)) %do% {
  inhibitor.one <- inhibitors.list[inhibitor.pairs.df[inhibitor.pair.i,]$inhibitor.one]
  inhibitor.two <- inhibitors.list[inhibitor.pairs.df[inhibitor.pair.i,]$inhibitor.two]
  inhibitor.three.name <- inhibitors.names[-as.numeric(inhibitor.pairs.df[inhibitor.pair.i,])]
  inhibitor.three <- inhibitors.list[-as.numeric(inhibitor.pairs.df[inhibitor.pair.i,])]
  data.tiles %>%
    dplyr::filter_(
      paste(inhibitor.one, "==", 0, "&",
            inhibitor.two, "==", 0, "&",
            signal_, "==", "max(", signal_,")")
    ) %>%
    dplyr::mutate(group = paste(inhibitor.three.name)) %>%
    dplyr::mutate_(inhibitor = inhibitor.three)
} %>%
  do.call(
    what = rbind,
    args = .
  ) ->
  data.tiles.individual

foreach(inhibitor.pair.i = 1:nrow(inhibitor.pairs.df)) %do% {
  inhibitor.one <- inhibitors.list[inhibitor.pairs.df[inhibitor.pair.i,]$inhibitor.one]
  inhibitor.two <- inhibitors.list[inhibitor.pairs.df[inhibitor.pair.i,]$inhibitor.two]
  inhibitor.three <- inhibitors.list[-as.numeric(inhibitor.pairs.df[inhibitor.pair.i,])]
  data.tiles %>%
    dplyr::filter_(
      paste(paste(inhibitor.three, "==", 0, collapse = " & "), "&",
            inhibitor.two, "==", inhibitor.one, "&",
            signal_, "==", "max(", signal_,")")
    )  %>%
    dplyr::mutate(group =
                    paste(
                      inhibitors.names[inhibitor.pairs.df[inhibitor.pair.i,]$inhibitor.one],
                      inhibitors.names[inhibitor.pairs.df[inhibitor.pair.i,]$inhibitor.two]

    )) %>%
    dplyr::mutate_(inhibitor = inhibitor.one)
}  %>%
  do.call(
    what = rbind,
    args = .
  ) ->
  data.tiles.pairs


data.tiles %>%
  dplyr::filter_(
    paste(signal_, "==", "max(", signal_,")")
  ) %>%
  dplyr::filter_(
    paste((inhibitor.pairs.df %>%
      dplyr::mutate(command =
                      paste(inhibitors.list[inhibitor.one],
                            inhibitors.list[inhibitor.two],
                            sep = "==")))[["command"]],
      collapse = "&")
  )  %>%
  dplyr::mutate(group =
                  paste(
                    inhibitors.names,
                    collapse = " "
                  )) %>%
  dplyr::mutate_(inhibitor = inhibitors.list[1])->
  data.tiles.group

do.call(
  what = rbind,
  args = list(data.tiles.individual,
              data.tiles.pairs,
              data.tiles.group)
) -> data.tiles.curves



g.curve.pS <-
  plotCompletePartialLine(
  data =
    data.tiles.curves %>%
    dplyr::select(type, pS1, pS3, group, inhibitor) %>%
    #dplyr::mutate(pS1pS3 = pS1 - pS3) %>%
    reshape2::melt(
      id.vars = c("inhibitor", "type", "group")
    ),
  title_ = "",
  x_ = "inhibitor",
  y_ = "value",
           group_ = "group",
           ylim_ = c(0,1),
           xlab_ = "inhibitor",
  rescale.fun = "log") +
  facet_grid(variable~type)

g.curve.pSdiff <-
  plotCompletePartialLine(
    data =
      data.tiles.curves %>%
      dplyr::select(type, pS1, pS3, group, inhibitor) %>%
      dplyr::mutate(pS1pS3 = pS1 - pS3) %>%
      dplyr::select(-pS1, -pS3) %>%
      reshape2::melt(
        id.vars = c("inhibitor", "type", "group")
      ),
    title_ = "",
    x_ = "inhibitor",
    y_ = "value",
    group_ = "group",
    ylim_ = c(-0.5,0.5),
    xlab_ = "inhibitor",
    rescale.fun = "log") +
  facet_grid(variable~type)

