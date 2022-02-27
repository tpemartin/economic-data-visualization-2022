get_vaccineDF_aggregate <- function(vaccination){
  
  city = vaccination$result$city
  bornRange = vaccination[["result"]][["bornRange"]]
  
  list_vaccines_byAgeGroup <-
    get_list_vaccines_byAgeGroup(
      bornRange = bornRange,
      city = city
    )
  
  list_vaccineDFs_byAgeGroup <- 
    get_vaccineDFs_byAgeGroup(list_vaccines_byAgeGroup)
  
  list_vaccineDFs_byAgeGroup |>
    purrr::reduce(
      dplyr::bind_rows
    ) |>
    relocate(age, county) -> vaccineDF
  
  vaccineDF |>
    group_by(age, county) |>
    rowwise() |>
    mutate(
      sum=sum(
        c_across(), na.rm=T
      )
    ) |>
    ungroup() -> vaccineDF_aggregate
  
  vaccineDF_aggregate
}
get_vaccineDFs_byAgeGroup <- function(list_vaccines_byAgeGroup) {
  purrr::map(
    seq_along(list_vaccines_byAgeGroup),
    ~{
      list_vaccines_byAgeGroup[.x] |>
        get_df_ageGroupX()
    }
  )
}


get_df_ageGroupX <- function(ageGroupX) {
  ageGroupname <- names(ageGroupX)
  ageGroupX[[1]] |>
    purrr::reduce(
      dplyr::bind_rows
    ) -> df_ageGroupX
  df_ageGroupX$age = ageGroupname
  df_ageGroupX
}
get_list_vaccines_byAgeGroup <- function(bornRange, city) {
  purrr::map(
    bornRange,
    ~get_list_vaccineDF(.x, city)
  )
}
get_list_vaccineDF <- function(ageX, city) {
  purrr::map(
    seq_along(ageX),
    ~ {
      ageXx <- ageX[[.x]]
      if (is.null(ageXx)) {
        NULL
      } else {
        ageXx |>
          tidyr::pivot_wider(
            names_from = "vaccine",
            values_from = "amount"
          ) |>
          mutate(
            county = city[[.x]]
          )
      }
    }
  )
}