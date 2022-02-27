copy_url_paste_gps <- function(){
  clipr::read_clip() |> get_gps() |> clipr::write_clip()
}

get_gps <- function(url) {
  url |>
     stringr::str_extract(
       "(?<=/@)[^/@]+(?=/)"
     ) |>
    stringr::str_split(",") -> gps
  
  gps[[1]][1:2] |> as.numeric()
}

