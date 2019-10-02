#' getScaleY.Comparison
#' @export
getScaleY.Comparison <-
  function(
    scaleY.a,
    scaleY.b,
    ylimits,
    ybreaks = NULL,
    ylab,
    ylab.right,
    digits = 2,
    length.out = 5,
    ...
  ){

    if(is.null(ybreaks)){
      ybreaks <-
        seq(from = floor(ylimits[1]),
            to =   floor(ylimits[2]),
            length.out = length.out)
    }
    a <- scaleY.a
    b <- scaleY.b
    if(scaleY.a != 0){
      ggplot2::scale_y_continuous(
        sec.axis =
          eval(substitute(expr =
                            ggplot2::sec_axis(trans = ~ (.-bb)/aa,
                                              name = ylab.right,
                                              breaks = (ybreaks-b)/a,
                                              labels = round((ybreaks-b)/a,
                                                             digits = digits)),
                          #rep("", times= length(round((graphics.args.waves$ybreaks-b)/a)))),
                          list(aa = a,
                               bb = b))),
        name = ylab,
        breaks = ybreaks,
        limits = ylimits)
    } else {
      ggplot2::scale_y_continuous(
        sec.axis =
          eval(substitute(expr =
                            ggplot2::sec_axis(trans = ~ (.-bb),
                                              name = ylab.right,
                                              breaks = (ybreaks-b),
                                              labels = round((ybreaks-b),
                                                             digits = digits)),
                          #rep("", times= length(round((graphics.args.waves$ybreaks-b)/a)))),
                          list(aa = a,
                               bb = b))),
        name = ylab,
        breaks = ybreaks,
        limits = ylimits)
    }
  }
