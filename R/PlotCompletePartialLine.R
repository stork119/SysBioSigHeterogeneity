#' plotCompletePartialCube
#' @export
plotCompletePartialLine <-
  function(data,
           title_ = "",
           x_,
           y_ = "prob",
           group_ = "type",
           color_ = group_,
           ylim_ = c(0,1),
           xlab_ = "inhibitor",
           ylab_ = NULL,
           scale_colors_fun = viridis::scale_color_viridis(discrete = TRUE, end = 0.9),
           size.line = 1,
           ...){

        col.rescaled <- "signal_rescaled"
    col.to.rescale <- x_
    signal.rescale.df <-
      rescaleSignalsValues.DataFrame(
        data = data,
        col.to.rescale = col.to.rescale,
        col.rescaled = col.rescaled,
        ...
      )
    data %>%
      dplyr::left_join(
        signal.rescale.df,
        by = x_
      ) -> data
    g.plot <-
      ggplot2::ggplot(
      data = data,
      mapping =
        ggplot2::aes_string(x = "signal_rescaled",
                   y = y_,
                   group = group_,
                   color = color_)) +
      ggplot2::geom_line(size = size.line)  +
      SysBioSigTheme::theme_sysbiosig() +
      scale_colors_fun +
      ggplot2::coord_cartesian(ylim = ylim_) +
      ggplot2::ggtitle(title_)

      if(!is.null(ylab_)){
        g.plot <-
          g.plot + ylab(ylab_)
      }

    if(!is.factor(signal.rescale.df[[col.rescaled]])){
      g.plot +
        getScaleXContinuous(
          xlab = xlab_,
          signals.rescale.df = signal.rescale.df,
          col.rescaled = col.rescaled,
          col.to.rescale = col.to.rescale
        ) -> g.plot
    } else {
      g.plot +
        getScaleXDiscrete(
          xlab = xlab_,
          signals.rescale.df = signal.rescale.df,
          col.rescaled = col.rescaled,
          col.to.rescale = col.to.rescale
        )->
        g.plot
    }
   return(g.plot)
  }
