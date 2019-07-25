#' rescaleSignalsValues.factor
#'
rescaleSignalsValues.factor <-
  function(
    levels,
    ...){
    function(x){factor(x,
                       levels = levels)}
  }

#' rescaleSignalsValues.numeric
#'
rescaleSignalsValues.numeric <-
  function(...){
    function(x){x}
  }

#' rescaleSignalsValues.logarithmic
#'
rescaleSignalsValues.logarithmic <-
  function(base = exp(1),
           ...){
    function(x){log(x = x, base = base)}
  }

#' rescaleSignalsValues
#'
#' @param rescale.fun parameter, that defines a function used for rescaling signals in plots.
#' There are three built-in functions, that can be chosen:
#' (1) \code{'factor'} - signals treated as factors (default) with levels defined in list \code{rescale.fun.args},
#' (2) \code{'numeric'},
#' (3) \code{logarithmic} - with base defined in \code{rescale.fun.args} - default: \code{e = exp(1)}.
#' Function must be defined as a lambda construct \code{function(x, ...){...}}.
#' @param rescale.fun.args list of the arguments to defaults \code{rescale.fun}
#'
rescaleSignalsValues <-
  function(
    signal.list,
    rescale.fun = "factor",
    rescale.fun.args = list(),
    signal.rescale.remove = c(NA, NaN, -Inf, Inf),
    ...
  ){
    if(!is.list(rescale.fun.args)){
      stop("rescale.fun.args must be list of arguments to rescale.fun")
    }
    if(is.character(rescale.fun)){
      rescale.option <- "factor"
      tryCatch(
        expr = {
          rescale.option <- match.arg(arg = rescale.fun,
                                      choices = c("numeric", "factor", "logarithmic", "log"))
        },
        error =
          function(e){
            warning(paste("Rescaling option rescale.fun =",
                          paste("'", rescale.fun, "'", sep = ""),
                          "is not defined. Default will be used used"))
          }
      )
      if(rescale.option == "factor"){
        if(!("levels" %in% names(rescale.fun.args))){
          rescale.fun.args$levels <- signal.list
        }
        rescale.fun.call <- rescaleSignalsValues.factor
      } else if (rescale.option %in% c("log", "logarithmic")){
        rescale.fun.call <-
          rescaleSignalsValues.logarithmic
      } else {
        rescale.fun.call <-
          rescaleSignalsValues.logarithmic
      }
      rescale.fun <-
        do.call(
          what = rescale.fun.call,
          args = rescale.fun.args
        )
    }

    data.frame(signal =
                 signal.list) %>%
      dplyr::mutate(signal.rescale =
                      rescale.fun(x = signal)) ->
      signal.rescale.df

    signal.remove <-
      (signal.rescale.df %>%
         dplyr::filter(signal.rescale %in%
                         signal.rescale.remove))[["signal"]]

    signal.rescale.df %>%
      dplyr::filter(!(signal %in% signal.remove))  ->
      signal.rescale.df
    if(length(signal.remove) > 0){
      if("base" %in% names(rescale.fun.args)){
        signal.mins <- rescale.fun(
          x = c(rescale.fun.args$base,
                rescale.fun.args$base*rescale.fun.args$base
          ))
      } else {
        signal.mins <-
          (signal.rescale.df %>%
             dplyr::arrange(signal))$signal.rescale[c(1,2)]
      }

      signal.remove.rescale.values <-
        (seq(from = signal.mins[1],
             by =  -(signal.mins[2] - signal.mins[1]),
             length.out = length(signal.remove) + 1))[-1]
      signal.rescale.df %>%
        rbind(
          data.frame(
            signal = signal.remove,
            signal.rescale = signal.remove.rescale.values
          )
        ) %>%
        dplyr::arrange(signal) ->
        signal.rescale.df
    }
    return(signal.rescale.df)
  }

#' rescaleSignalsValues.DataFrame
#'
#' @param data data
#' @param col.to.rescale character, define column that must be rescaled
#' @param col.rescaled character, define name of the rescaled
#' @inheritDotParams rescaleSignalsValues
#' @export
rescaleSignalsValues.DataFrame <-
  function(
    data,
    col.to.rescale,
    col.rescaled   = "signal_rescaled",
    ...){

    signal.rescale.df <-
      rescaleSignalsValues(
        signal.list =
          (data %>%
             dplyr::arrange_(col.to.rescale) %>%
             dplyr::distinct_(col.to.rescale))[[col.to.rescale]],
        ...)

    signal.rescale.df %>%
      dplyr::rename_(
        .dots = setNames(object = colnames(signal.rescale.df),
                         nm = c(col.to.rescale, col.rescaled)))  %>%

      return()
  }
