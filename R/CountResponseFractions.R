#' CountResponseFractions
#' @description CountResponseFractions
#' @param data data.frame with colnames \code{response}, \code{signal} and
#' @param bounds.df result of function \code{ComputeDistinctQuantiles}
#' @param response chartacter, that specify name of the column that represents
#' the output response
#' @param signal character, specify name of the column that represents the input signal
#' @param distincting.colnames columns that are used for grouping data
#' @export
CountResponseFractions <-
  function(
    data,
    bounds.df,
    response,
    signal,
    distincting.colnames,
    ...
  ) {
  SysBioSigHeterogeneity::DataFiltering(
      data = data,
      response = response,
      ...
    ) ->
      data

bounds.df %>%
  dplyr::arrange(bound) ->
      bounds.df



  for(bound.i in 1:nrow(bounds.df)) {
    data %>%
      # dplyr::mutate(
      #   !!bounds.df[bound.i,]$type :=
      #
      #         paste(response, "<=", bounds.df[bound.i,]$bound)
      #     )
      dplyr::mutate_(
        .dots =
          setNames(
            object =
              paste(response, "<=", bounds.df[bound.i,]$bound),
            nm = bounds.df[bound.i,]$type
        ))  ->
      data
  }
  data %>%
    dplyr::group_by(.dots = distincting.colnames
                    #data  %>% select(contains("inhibitor")) %>% colnames()
                    ) %>%
    dplyr::summarise_(.dots =
                        setNames(object = paste("sum(", bounds.df$type, ")/n()"),
                                 nm = bounds.df$type)) %>%
    reshape2::melt(
      id.vars = distincting.colnames,
      variable.name = "type",
      value.name =    "prob",
      factorsAsStrings = TRUE
    ) %>%
    dplyr::left_join(
      bounds.df,
      by = "type"
    ) %>%
  return()

}
