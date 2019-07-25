getScaleXContinuous <-
  function(
    xlab,
    signals.rescale.df,
    col.rescaled,
    col.to.rescale
  ) {
    ggplot2::scale_x_continuous(
      name = xlab,
      breaks = signals.rescale.df[[col.rescaled]],
      labels = signals.rescale.df[[col.to.rescale]],
      limits = c(min(signals.rescale.df[[col.rescaled]]), max(signals.rescale.df[[col.rescaled]]))
    )
  }


getScaleXDiscrete <-
  function(
    xlab,
    signals.rescale.df,
    col.rescaled,
    col.to.rescale
  ) {
    ggplot2::scale_x_discrete(
      name = xlab,
      breaks = as.character(signals.rescale.df[[col.rescaled]]),
      labels = as.character(signals.rescale.df[[col.to.rescale]]),
      limits = as.character(signals.rescale.df[[col.rescaled]]))
  }
