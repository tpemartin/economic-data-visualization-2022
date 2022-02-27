colors <- c(low = "#d3e3f3", high = "#003870")
colors <- tolower(colors)
lab_in <- farver::decode_colour(colors, alpha = TRUE, to = "lab", 
  na_value = "transparent")
x_in <- seq(0, 1, length.out = length(colors))
l_interp <- stats::approxfun(x_in, lab_in[, 1])
u_interp <- stats::approxfun(x_in, lab_in[, 2])
v_interp <- stats::approxfun(x_in, lab_in[, 3])

colors <- 
farver::decode_colour(colors, alpha = TRUE, to = "hsl", 
  na_value = "transparent")
colors
