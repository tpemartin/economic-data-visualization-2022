myGoodPlot <- function(data4plot) {
  myplot <- {
    ggplot() +
      geom_point(
        data = data4plot$point$data,
        mapping = aes(
          x = x, y = y
        ),
        fill = "#2fc1d3",
        shape = 21,
        color = "black",
        stroke = 1,
        size = 12
      ) +
      geom_text(
        data = data4plot$text$data,
        mapping = aes(
          x = x, y = y, label = label
        ),
        hjust = 0,
        size = 12
      )
  }
  myplot
}
plot_hightlightCountries <- function(data) {
  myplot <- {
    ggplot(data = data,
    ) +
      geom_point(
        mapping = aes(
          x = x, y = y
        ),
        fill = "#2fc1d3",
        shape = 21,
        color = "black",
        stroke = 1,
        size = 12
      ) +
      geom_text(
        # data = data4plot$text$data,
        mapping = aes(
          x = x, y = y, label = label
        ),
        hjust = 0,
        size = 12
      )
  }
  myplot
}
plot_countryPoints <- function(hightlightedCountries, unhighlightedCountries) {
  plot_hightlightCountries(hightlightedCountries) +
    geom_point(
      data=unhighlightedCountries,
      aes(
        x=x, y=y
      ),
      size = 12,
      color= "#a4dce6"
    ) 
}
hightlightXYequal <- function(plot_obsessionWithBorders) {
  plot_obsessionWithBorders +
    geom_abline(
      aes(
        slope=1,
        intercept=0
      ),
      color = "#549ab6",
      linetype = 2,
      size=2
    )
}
plot_allCountries <- function(highlightedCountries, nonhighlightedCountries) {
  plot_hightlightCountries(highlightedCountries) +
    geom_point(
      data = nonhighlightedCountries,
      aes(
        x=x,
        y=y
      ),
      size = 12,
      color= "#a4dce6"
    )
}
plot_allCountries <- function(highlightedCountries, nonhighlightedCountries) {
  plot_hightlightCountries(highlightedCountries) +
    geom_point(
      data = nonhighlightedCountries,
      aes(
        x=x,
        y=y
      ),
      size = 12,
      color= "#a4dce6"
    )
}

