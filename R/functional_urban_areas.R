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
