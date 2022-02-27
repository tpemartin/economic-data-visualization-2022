sf_taiwan |> names()

sf_taiwan[c("縣市_simplified", "區鎮_simplified_patched")] -> sf_taiwan_simplified
sf_taiwan_simplified$區鎮_simplified_patched |>
  sf_taiwan$crop_mainland() -> sf_taiwan_simplified$台灣本島$鄉鎮區
sf_taiwan_simplified$縣市_simplified |>
  sf_taiwan$crop_mainland() ->
  sf_taiwan_simplified$台灣本島$縣市

names(sf_taiwan_simplified)[c(1:2)] <- c("縣市", "鄉鎮區")

sf_taiwan_simplified |> 
  saveRDS("data/sf_taiwan_simplified.Rds")
sf_taiwan_simplified$縣市
{
  sf_taiwan_simplified$台灣本島$縣市 |> 
  mutate(
    map_id=name
  ) |>
  relocate(map_id) -> sf_taiwan_simplified$台灣本島$縣市
sf_taiwan_simplified$台灣本島$鄉鎮區 |>
  mutate(
    map_id=paste0(is_in.city,name)
  ) |> 
  relocate(map_id) -> sf_taiwan_simplified$台灣本島$鄉鎮區
sf_taiwan_simplified$縣市 |> 
  mutate(
    map_id=name
  ) |>
  relocate(map_id) -> sf_taiwan_simplified$縣市
sf_taiwan_simplified$鄉鎮區 |>
  mutate(
    map_id=paste0(is_in.city,name)
  ) |> 
  relocate(map_id) -> sf_taiwan_simplified$鄉鎮區
}
sf_taiwan_simplified |> saveRDS("data/sf_taiwan_simplified.Rds")

geom_sf_taiwan <- function(data, map_id, ...,
  type=c("縣市", "鄉鎮區"),
  background.fill= "#c8c5be",
  background.color= "#c8c5be",
  background.size= 0
  ) {

  type = match.arg(type)
  .sf=sf_taiwan_simplified[["台灣本島"]][[type]]

  which2pick <- which(.sf$map_id %in% data[[map_id]])
  .sf[which2pick, ] |>
    dplyr::left_join(
      data,
      by=c("map_id"=map_id)
    ) -> .sf_choropleth
  
  list(
    geom_sf(
      data=.sf,
      fill=background.fill,
      color=background.color,
      size=background.size
    ),
    geom_sf(
      data=.sf_choropleth,
      ...
    )
  )
}

ggplot() +
  geom_sf_taiwan(
    data=gg_drug$data,
    map_id="發生地點",
    mapping=aes(fill=案件次數),
    type="鄉鎮區"
  ) 