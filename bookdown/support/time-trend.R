geom_lineWithStroke <- function(
  mapping = NULL,
  data = NULL,
  stat = "identity",
  position = "identity",
  na.rm = FALSE,
  orientation = NA,
  show.legend = NA,
  inherit.aes = TRUE,
  # set up default makes your function easy to use
  stroke = "white",
  prop = 0.9,
  size = 2, 
  ...){
  list(
    geom_line(
      data=data,
      color=stroke,
      size = size,
      mapping = mapping
    ),
    geom_line( # the last geom will be on the top
      mapping = mapping,
      data = data,
      stat = stat,
      position = position,
      na.rm = na.rm,
      orientation = orientation,
      show.legend = show.legend,
      inherit.aes = inherit.aes,
      size = size*prop,
      ...)
  )
}