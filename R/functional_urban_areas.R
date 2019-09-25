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
  year <- jsonlite::fromJSON(rawToChar(resp$content))$dimension$time$category$label
  data <- jsonlite::fromJSON(rawToChar(rep$content))$value
  null_data <- jsonlite::fromJSON(rawToChar(rep$content))$status
  category <- jsonlite::fromJSON(rawToChar(rep$content))$dimension$indic_ur$category$label

  tibble(
    year = rep(purrr::map_int(names(year), function(x){as.integer(year[[x]])}),
               length(c(data, null_data))/length(year)),
    value = purrr::map_chr(as.character(sort(as.integer(c(names(data),
                                                          names(null_data))))),
                           function(x){c(data, null_data)[[x]]}),
    code = purrr::map(names(category),
                      function(x){rep(x, 30)}) %>% purrr::reduce(c),
    dataset = purrr::map(names(category),
                         function(x){rep(category[[x]], 30)}) %>% purrr::reduce(c)

  )
}



