#' ComputeDistinctQuantiles
#' @description CountResponseFractions
#' @param data.control data.frame with colnames \code{response}, \code{signal} - used as lower bound
#' @param data.saturation data.frame with colnames \code{response}, \code{signal} - used as upper bound
#' @param bounds.df result of function \code{ComputeDistinctQuantiles}
#' @param response chartacter, that specify name of the column that represents
#' the output response
#' @param signal character, specify name of the column that represents the input signal
#' @param distincting.colnames columns that are used for grouping data
#' @return bounds.df -- data.frame that consist name of the bound and its value
#' @export
ComputeDistinctQuantiles <-
  function(
    data.control,
    data.saturation,
    response,
    signal,
    quantile.prob,
    bounds.names = c("complete", "partial"),
    bootstrap = TRUE,
    bootstrap.number = 8,
    bootstrap.sample_size = 1000,
    bootstrap.test.sample = TRUE,
    bootstrap.test.number = 4,
    parallel_cores = 1,
    cc_maxit = 100,
    lr_maxit = 1000,
    MaxNWts = 5000,
    ...
  ){

    signal.min <- (data.control %>% dplyr::distinct_(signal))[[signal]]
    signal.max <- (data.saturation %>% dplyr::distinct_(signal))[[signal]]

    df.res <-
      GetLogRegParameters(
        data =
          (data.control %>% dplyr::select_(signal, response))  %>%
          rbind(data.saturation %>% dplyr::select_(signal, response)),
        response = response,
        signal = signal,
        signal.list = c(signal.min, signal.max))


    lr_model <- nnet::multinom(formula = df.res$formula_string,
                               data = df.res$data,
                               na.action = na.omit,
                               maxit = lr_maxit,
                               MaxNWts = MaxNWts)#,  model = FALSE )


    lr.fit <-
      predict(object  = lr_model,
              newdata = df.res$data)

    df.res$data$class <- as.numeric(as.character(lr.fit))

    df.res$data %>%
      dplyr::rename(level = X) %>%
      dplyr::rename_(.dots = setNames(
        object =df.res$response,
        nm = response)) %>%
      dplyr::filter(level == class) ->
      df.res$data

    fraction.min.bound <-
      quantile((df.res$data %>%
                  dplyr::filter(level %in% c(signal.min)))[[response]],
               probs = 1 - quantile.prob)[[1]]

    fraction.max.bound <-
      quantile((df.res$data %>%
                  dplyr::filter(level %in% c(signal.max)))[[response]],
               probs = quantile.prob)[[1]]

    fractions.bounds <- list(fraction.min.bound = fraction.min.bound,
                fraction.max.bound = fraction.max.bound
                )

    bounds.df <-
      data.frame(
        type = c("complete", "partial"),
        bound = c(fractions.bounds$fraction.min.bound,
                  fractions.bounds$fraction.max.bound),
        stringsAsFactors = FALSE
      )
    return(bounds.df)
  }
