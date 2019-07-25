#' plotCompletePartialCube
#' @export
plotCompletePartialCube <-
  function(data,
           axes.x,
           axes.y,
           axes.x.name,
           axes.y.name,
           response.type_,
           color.limits,
           title_ = "",
           scale_colors_fun = NULL,
           signal.list = NULL,
           ...){
    if(is.null(scale_colors_fun)){
      scale_colors_fun <-
        viridis::scale_fill_viridis(
          name = "Cells fraction",
          limits = color.limits)
    }
    if(is.null(signal.list)){
      signal.list <- as.character(sort(c(unique(c(data[[axes.x]],data[[axes.y]])))))
    }
    ggplot2::ggplot(data = data,
           mapping =
             ggplot2::aes_string(
             x = paste("factor(", axes.x, ")"),
             y = paste("factor(", axes.y, ")"),
             fill = response.type_,
             label = paste("round(", response.type_, ", 2)")
           )) +
      ggplot2::geom_tile(color = "white") +
      ggplot2::ggtitle(title_)+
      ggplot2::xlab(axes.x.name)+
      ggplot2::ylab(axes.y.name)+
      SysBioSigTheme::theme_sysbiosig() +
      scale_colors_fun +
      ggplot2::scale_x_discrete(limits = signal.list) +
      ggplot2::scale_y_discrete(limits = signal.list) +
      ggplot2::geom_text(size = 4) +
      ggplot2::facet_grid("~type")
  }
