#' calculateInhibitionCurve
#' @description Function returns standard inhibition curve for mean or median
#' @param data data.frame with colnames \code{response}, \code{signal}
#' @param experiments.df data.frame that cosists of columns \code{response.cols} - response column name, e.g. Intensity_MeanIntensity_Alexa488, \code{response.protein} - response desription, e.g., pSTAT1#' @param signal character, specify name of the column that represents the input signal
#' @param fun.summarise function, e.g., \code{mean}, \code{median}
#' @param quote.control quote, used for filtering control data, default \code{quote.control = quo(inhibitor.1.0 == 0 & stimulation.1.1 == 0)}
#' @param quote.saturation quote, used for filtering control data, default \code{quote.saturation = quo(inhibitor.1.1 == 0 & stimulation.1.1 != 0)}
#' @export
calculateInhibitionCurve <-
  function(
    data,
    experiments.df,
    distincting.colnames,
    fun.summarise = mean,
    quote.control = quo(inhibitor.1.0 == 0 & stimulation.1.1 == 0),
    quote.saturation = quo(inhibitor.1.1 == 0 & stimulation.1.1 != 0),
    ...
  ){

    SysBioSigHeterogeneity::DataFiltering(
      data = data,
      response = response,
      ...
    ) ->
      data

    summary_vars <-syms(experiments.df$response.cols)

    # Wrap the summary variables with mean()
    summary_vars <- purrr::map(summary_vars, function(var) {
      expr(fun.summarise(!!var, na.rm = TRUE))
    })

    # Prefix the names with `avg_`
    names(summary_vars) <- experiments.df$response.protein

    data %>%
      dplyr::group_by(
        !!!syms(x = distincting.colnames)
      ) %>%
      dplyr::summarise(
        !!!summary_vars
      ) %>%
      dplyr::ungroup() ->
      data.sum

    ### rescaling to 0-1 ###
    data.control <-
      data.sum %>%
      dplyr::filter(
        !!quote.control
      )
    sym.response.expr <-
      purrr::map(experiments.df$response.protein,
                 function(var) {
                   var.sym <- sym(var)
                   expr(!!var.sym - data.control[[!!var]])
                 })
    names(sym.response.expr) <- experiments.df$response.protein
    data.sum %>%
      dplyr::mutate(
        !!!sym.response.expr
      ) ->
      data.sum

    data.saturation <-
      data.sum %>%
      dplyr::filter(
        !!quote.saturation
      )

    sym.response.expr <-
      purrr::map(experiments.df$response.protein,
                 function(var) {
                   var.sym <- sym(var)
                   expr(1 - !!var.sym/data.saturation[[!!var]])
                 })
    names(sym.response.expr) <- experiments.df$response.protein

    data.sum %>%
      dplyr::mutate(
        !!!sym.response.expr
      ) ->
      data.sum
  }
