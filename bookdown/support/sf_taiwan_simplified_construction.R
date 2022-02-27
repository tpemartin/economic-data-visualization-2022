osmdata::getbb("Taiwan") -> bbox
mp <- econDV2::Map()
get_admin_level <- function(num){
  bbox |>
    mp$osm$request_data(
      c(
        "admin_level"=as.character(num)
      )
    )
}

sf_taiwan6 <- sf_taiwan

econDV2::Object(sf_taiwan)

sf_taiwan$a4 <- get_admin_level(4) #sf_taiwan4 # 直轄市
sf_taiwan$a5 <- get_admin_level(5) #sf_taiwan5 # 區（直轄市）
sf_taiwan$a6 <- get_admin_level(6) #sf_taiwan6 # 縣轄市
sf_taiwan$a7 <- get_admin_level(7) #sf_taiwan7
sf_taiwan$a8 <- get_admin_level(8) #sf_taiwan8 # 鎮
# sf_taiwan$a9 <- get_admin_level(9) #sf_taiwan9 # 里
sf_taiwan$save() 

.df <- sf_taiwan$a4$osm_multipolygons# |> View()

select_osm_cols <- function(.df){
  # View(.df)
  cols <- c("osm_id", "name", "ISO3166.2","admin_level", "geometry")
  .df |> 
    dplyr::select(dplyr::everything(cols)) 
}
select_osm_cols2 <- function(.df){
  # View(.df)
  cols <- c("osm_id", "name", "ISO3166.2","admin_level", "is_in.city", "geometry")
  .df |> 
    dplyr::select(dplyr::everything(cols)) 
}
simplify <- function(.sf){
  sf::as_Spatial(.sf) |>
    rmapshaper::ms_simplify() -> .simple
  .simple |>
    sf::st_as_sf() -> .simple 
  return(.simple)
}

sf_taiwan$a4$osm_multipolygons |>
  osmdata::unname_osmdata_sf()
sf_taiwan$a4$osm_multipolygons |> class()
sf_taiwan$a4$osm_multipolygons |> sf::st_geometry()

sf_taiwan$a4$osm_multipolygons |> 
    sf::as_Spatial() |>
    rmapshaper::ms_simplify() -> .test

View(.test)
class(.test)

purrr::map(
  paste0("a",4:8),
  ~{
    # .x="a5"
    sf_taiwan[[.x]]$osm_multipolygons |> mp$sf$simplify()
  }
) -> sf_simplified
sf_taiwan$simplified <- setNames(
  sf_simplified, paste0("a",4:8)
)
sf_taiwan$simplified$a4 |> ggplot()+geom_sf()
library(magrittr)
sf_taiwan$a4$osm_multipolygons |>
  dplyr::filter(
    !stringr::str_detect(name, '省')
  ) |>
  select(
    name, geometry
  ) |>
  mutate(
    map_id=name
  ) -> .df
sf_taiwan$a6$osm_multipolygons |>
  select(
    name, geometry
  ) |>
  mutate(
    map_id=name
  ) -> .df2 
dplyr::bind_rows(
  .df, .df2
) -> sf_taiwan$縣市
econDV2::Object(sf_taiwan_simplified)
sf_taiwan$縣市 |> mp$sf$simplify() ->
  sf_taiwan_simplified$縣市


# 鄉鎮區 ---------------------------------------------------------------------

for(.x in paste0("a", 5:8)){
  sf_taiwan[[.x]]$osm_multipolygons |> mp$sf$simplify() ->
    sf_taiwan$simplified[[.x]]
}
sf_taiwan$simplified$a5 |> ggplot()+geom_sf()
sf_taiwan$simplified$a7 |> ggplot()+geom_sf()
sf_taiwan$simplified$a8 |> ggplot()+geom_sf()

sf_taiwan$simplified$a5 |> View()
sf_taiwan$simplified$a5$name

for()
sf_taiwan$縣市[1, ]
{
  whichHasNoCity <- 
    which(sf_taiwan$a5$osm_multipolygons$is_in.city=="")
  sf_taiwan$a5$osm_multipolygons[whichHasNoCity, ]-> .dfnocity
  sf_taiwan$縣市 |>
    sf::st_contains(.dfnocity) -> containsWhich
  for(.x in seq_along(containsWhich)){
    if(length(containsWhich[[.x]])==0) next
    .dfnocity[containsWhich[[.x]], ]$is_in.city <- sf_taiwan$縣市$name[[.x]]
  }
  sf_taiwan$a5$osm_multipolygons[whichHasNoCity, ] <-  .dfnocity
  sf_taiwan$a5$osm_multipolygons
}

sf_taiwan$a5$osm_multipolygons |>
  select(name, is_in.city, geometry) |>
  mutate(
    map_id=paste0(is_in.city, name)
  ) -> .df1

sf_taiwan$simplified$a7$name
{
  whichHasNoCity <- 
    which(sf_taiwan$a7$osm_multipolygons$is_in.city=="")
  sf_taiwan$a7$osm_multipolygons[whichHasNoCity, ]-> .dfnocity
  sf_taiwan$縣市 |>
    sf::st_contains(.dfnocity) -> containsWhich
  for(.x in seq_along(containsWhich)){
    if(length(containsWhich[[.x]])==0) next
    .dfnocity[containsWhich[[.x]], ]$is_in.city <- sf_taiwan$縣市$name[[.x]]
  }
  sf_taiwan$a7$osm_multipolygons[whichHasNoCity, ] <-  .dfnocity
  sf_taiwan$a7$osm_multipolygons 
}

sf_taiwan$a7$osm_multipolygons |>
  select(name, is_in.city, geometry) |>
  mutate(
    map_id=paste0(is_in.city, name)
  ) -> .df2

sf_taiwan$simplified$a8$is_in.city
{
  whichHasNoCity <- 
    which(sf_taiwan$a8$osm_multipolygons$is_in.city=="")
  sf_taiwan$a8$osm_multipolygons[whichHasNoCity, ]-> .dfnocity
  sf_taiwan$縣市 |>
    sf::st_contains(.dfnocity) -> containsWhich
  for(.x in seq_along(containsWhich)){
    if(length(containsWhich[[.x]])==0) next
    .dfnocity[containsWhich[[.x]], ]$is_in.city <- sf_taiwan$縣市$name[[.x]]
  }
  sf_taiwan$a8$osm_multipolygons[whichHasNoCity, ] <-  .dfnocity
  sf_taiwan$a8$osm_multipolygons 
}

sf_taiwan$a8$osm_multipolygons |>
  select(name, is_in.city, geometry) |>
  mutate(
    map_id=paste0(is_in.city, name)
  ) -> .df3
.df3

dplyr::bind_rows(
  .df1, .df2, .df3
) -> sf_taiwan$鄉鎮區
sf_taiwan$鄉鎮區 |> mp$sf$simplify() ->
  sf_taiwan_simplified$鄉鎮區

sf_taiwan$crop_mainland <- function(.sf){
  xmin <- 119
  xmax <- 122
  ymin <- 21
  ymax <- 25.6
  .sf |>
    sf::st_crop(
      c(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax)
    )
}

sf_taiwan_simplified$台灣本島 <- purrr::map(
  sf_taiwan_simplified[c("縣市", "鄉鎮區")],
  ~{
    .x |> sf_taiwan$crop_mainland()
  }
)
names(sf_taiwan_simplified$台灣本島) <- c("縣市", "鄉鎮區")
sf_taiwan_simplified$`台灣本島`$縣市 |> ggplot()+geom_sf()
sf_taiwan_simplified$`台灣本島`$鄉鎮區 |> ggplot()+geom_sf()


# 尖山，五峰 -------------------------------------------------------------------

bbox2 <- c(left=120.7432,
  bottom=24.3265,
  right=121.8212,
  top=24.8659) 
get_hsinchucounties <- function(name){
  bbox2 |>
    mp$osm$request_data(
      features = c(
        "name"=name
      )
    )
}
two_counties <- c("尖石鄉","五峰鄉")
purrr::map(
  two_counties,
  get_hsinchucounties
) -> sf_taiwan$hsinchu
names(sf_taiwan$hsinchu) <- two_counties
sf_taiwan$create_polygon_from_linestring_geometry <- function(sfc) {
  sfc |> 
    sf::st_geometry() |>
    purrr::reduce(sf::st_union) -> g1
  # g1 |> ggplot()+geom_sf()
  # g1 |> sf::st_line_merge() |> ggplot()+geom_sf()
  g1 |> sf::st_line_merge() -> g2
  g2 |> sf::st_cast("MULTIPOLYGON") #|> ggplot()+geom_sf()
}
sf_taiwan$hsinchu$`尖石鄉`$osm_lines |>
  sf_taiwan$create_polygon_from_linestring_geometry() -> .sf1
sf_taiwan$hsinchu$五峰鄉$osm_lines |>
  sf_taiwan$create_polygon_from_linestring_geometry() -> .sf2
sf::st_sfc(list(.sf1, .sf2)) -> .sfc

data.frame(
  name=two_counties
) |>
  mutate(
    is_in.city="新竹縣",
    map_id=paste0(is_in.city, name)
  ) |>
  sf::st_set_geometry(.sfc) -> .sfhsinchu

dplyr::bind_rows(sf_taiwan$鄉鎮區, .sfhsinchu) -> 
  sf_taiwan$鄉鎮區
sf_taiwan$鄉鎮區 |> mp$sf$simplify() ->
  sf_taiwan_simplified$鄉鎮區

sf_taiwan_simplified$鄉鎮區 |>
  sf_taiwan$crop_mainland() ->
  sf_taiwan_simplified$台灣本島$鄉鎮區

# drug data ---------------------------------------------------------------

drug <- list()
drug$source[[1]] <- 
  "https://docs.google.com/spreadsheets/d/17ID43N3zeXqCvbUrc_MbpgE6dH7BjLm8BHv8DUcpZZ4/edit?usp=sharing"
drug$data <- 
  googlesheets4::read_sheet(
    drug$source[[1]]
  )
