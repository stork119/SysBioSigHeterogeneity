#' DataFiltering
#' @description CountResponseFractions
#' @param data data.frame with colnames \code{response}
#' @param response chartacter, that specify name of the column that represents the output response
#' @param response.min numeric, lower bound of \code{response}, default \code{response.min = 10^(0)}
#' @param response.max numeric, lower bound of \code{response}, default \code{response.max = 10^(4)}
#' @return filtered data
#' @export
DataFiltering <-
  function(
    data,
    response,
    response.min = 10^(0),
    response.max = 10^(4)
  ){
    data %>%
      dplyr::filter(
        !!!quos(!!sym(response) > response.min,
                !!sym(response) < response.max
                )
      )
  }


