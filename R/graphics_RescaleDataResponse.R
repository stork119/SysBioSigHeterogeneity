#' rescaleDataToITRC.Params
#' @param data data to rescale
rescaleDataResponse.Params <-
  function(
    data,
    scale.left = "Intensity_MeanIntensity_Alexa488",
    scale.right = "Intensity_MeanIntensity_Alexa555",
    data_left_min = 0,
    data_left_max = Inf,
    data_right_min = 0,
    data_right_max = Inf,
    quantile.prob = 0.95,
    ...
  ){

    data_left_min <- min(data_left_min,
                         data[[scale.left]])
    data_left_max <- min(data_left_max,
                         quantile(x = data[[scale.left]],
                                  probs = quantile.prob))

    data_right_min <- min(data_right_min,
                          data[[scale.right]])
    data_right_max <- min(data_right_max,
                          quantile(x = data[[scale.right]],
                                   probs = quantile.prob))

    a <- (data_left_min - data_left_max)/(data_right_min - data_right_max)
    return(list(a = a,
                b = data_left_max - a*data_right_max))
  }



#' rescaleDataToITRC.DataFrame
#' @param data data to rescale
#' @export
rescaleDataResponse.DataFrame <-
  function(
    data,
    scale.left = "Intensity_MeanIntensity_Alexa488",
    scale.left.name  = "pSTAT1",
    scale.right = "Intensity_MeanIntensity_Alexa555",
    scale.right.name  = "pSTAT3",
    a = NULL,
    b = NULL,
    ...
  ){

    if(is.null(a) | is.null(b)){
      rescale.params <-
        rescaleData.Params(
          data = data,
          scale.left = scale.left,
          scale.right = scale.right,
          ...
        )

      a <- rescale.params$a
      b <- rescale.params$b
    }

    #if(is.null(variable.rescaled)){
    variable.rescaled <- scale.right
    variable.to.rescale <- scale.right
    #}

    response.df <-
      data.frame(
        colnames = c(scale.left,
                     scale.right),
        pSTAT = c(scale.left.name,
                  scale.right.name),
        stringsAsFactors = FALSE)

    id.colnames <- colnames(data)[-which(colnames(data) %in% response.df$colnames)]

    data %>%
      dplyr::mutate_(
        .dots =
          setNames(object = paste("`", variable.to.rescale, "`", "*a + b", sep = ""),
                   nm = variable.rescaled
          ))  %>%
      reshape2::melt(
        id.vars = id.colnames,
        variable.name = "colnames",
        value.name = "intensity",
        stringsAsFactors = FALSE
      ) %>%
      dplyr::left_join(
        response.df,
        by = "colnames"
      ) %>%
      data.table::data.table()->
      data.rescaled

    return(
      list(
        a = a,
        b = b,
        data.rescaled = data.rescaled
      )
    )
  }
