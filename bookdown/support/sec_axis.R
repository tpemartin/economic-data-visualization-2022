
# helpers -----------------------------------------------------------------


scale_mapping_y <- function(mapping)
{
  mapping$y |> rlang::quo_get_expr() -> y_source_expr
  rlang::quo(
    scale_fn(!!y_source_expr)
  ) -> new_quo
  rlang::quo_set_env(
    new_quo,
    env=rlang::quo_get_env(mapping$y)
  ) -> mapping$y
  mapping
}

# {
#   limits_left = range(breaks_left)
#   limits_right =range(breaks_right)
#   transfer_fn <- function(x) scales::rescale(x, to=limits_right, from=limits_left)
#   scale_fn <- function(y) scales:rescale(y, to=limits_left, from=limits_right)
# }

geom_line_right <- function(..., mapping, name_left=waiver(), name_right=waiver(), labels_left=waiver(), labels_right=waiver(), breaks_left=waiver(), breaks_right=waiver(), expand=waiver()){
  list(
    {
      limits_left = range(breaks_left)
      limits_right =range(breaks_right)
      .GlobalEnv$transfer_fn <- function(x) scales::rescale(x, to=limits_right, from=limits_left)
      .GlobalEnv$scale_fn <- function(y) scales::rescale(y, to=limits_left, from=limits_right)
      
      
      mapping_scaled = scale_mapping_y(mapping)
      geom_right_scaled <- {
        geom_line(
          mapping=mapping_scaled, 
          ...
        )
      }  
    },
    {
      scale_y_continuous(
        name=name_left,
        limits = limits_left,
        breaks = breaks_left,
        labels = labels_left,
        sec.axis = sec_axis(
          trans=transfer_fn, # map: first axis breaks to second axis breaks
          name=name_right,
          labels=labels_right # secondary axis labels
        ),
        expand = expand
      )
    })
}


# test --------------------------------------------------------------------



breaks_left = seq(-10, 10, by=5)
breaks_right=9:13
name_left = NULL
name_right= NULL
labels_right=c("//", as.character(10:13))
mapping = aes(
  y=unemployment_scaled
)
color="#77230f"

ggplot5$plot1 +
  geom_line_right(
    mapping=aes(
      y=unemployment
    ),
    color="#77230f",
    breaks_left=breaks_left, breaks_right=breaks_right,
    name_left=name_left, name_right=name_right,
    labels_right = labels_right
  )

