#' Functional Urban Areas
#'
get_fua <- function(dataset, language = "en", city, props = FALSE){
  args <- list(cities = city)

  check_internet()
  base_url_modified <- sprintf(paste0(base_url, "%s/%s"), language, dataset)
  resp <- httr::GET(base_url_modified, query = plyr::compact(args))

  check_status(resp)

  resp
}

parse_response <- function(resp){
  rjson <- jsonlite::fromJSON(rawToChar(resp$content))

  year <- rjson$dimension$time$category$label
  data <- rjson$value
  null_data <- rjson$status
  category <- rjson$dimension$indic_ur$category$label
  fua_code <- rjson$dimension$cities$category$label
  fua <- rjson$dimension$cities$category$label

  num_codes <- rjson$size[1]
  num_cities <- rjson$size[2]
  num_years <- rjson$size[3]

  tibble(
    fua_code = rep(purrr::map(names(fua_code), function(x){
      rep(x, num_years)
    }) %>%
      purrr::reduce(c), num_codes),
    fua = rep(purrr::map(fua, function(x){
      rep(x, num_years)
    }) %>%
      purrr::reduce(c), num_codes),
    year = rep(purrr::map_int(names(year), function(x){as.integer(year[[x]])}),
               num_codes * num_cities),
    value = purrr::map_chr(as.character(sort(as.integer(c(names(data),
                                                          names(null_data))))),
                           function(x){c(data, null_data)[[x]]}),
    code = purrr::map(names(category),function(x){
                        rep(x, num_years * num_cities)}
                      ) %>%
      purrr::reduce(c),
    variable = purrr::map(names(category), function(x){
      rep(category[[x]], num_years * num_cities)
      }) %>%
      purrr::reduce(c)

  )

}



