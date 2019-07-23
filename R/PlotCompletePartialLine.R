plotCompletePartialLine <-
  function(data,
           title_ = "",
           x_,
           y_ = "prob",
           group_ = "type",
           color_ = group_,
           ylim_ = c(0,1),
           ...){
    return(
      ggplot(
        data = data,
        mapping =
          aes_string(x = x_,
                     y = y_,
                     group = group_,
                     color = color_)) +
        geom_line() +
        SysBioSigTheme::theme_sysbiosig() +
        viridis::scale_color_viridis(discrete = TRUE, end = 0.9) +
        coord_cartesian(ylim = ylim_) +
      ggtitle(title_))
  }
