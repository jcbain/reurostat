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


tmp <- jsonlite::fromJSON(rawToChar(rep$content))$dimension$time$category$label
tmp2 <- jsonlite::fromJSON(rawToChar(rep$content))$dimension$time$category$index
tmp3 <- jsonlite::fromJSON(rawToChar(rep$content))$value
tmp4 <- jsonlite::fromJSON(rawToChar(rep$content))$status

purrr::map_int(names(tmp2), function(x){as.integer(tmp2[[x]])})
purrr::map_int(names(tmp3), function(x){as.integer(x)%%length(tmp)})
purrr::map_int(names(tmp4), function(x){as.integer(x)%%length(tmp)})
sort(purrr::map_int(c(names(tmp4), names(tmp3)), function(x){as.integer(x)}))
purrr::map_chr(sort(as.integer(c(names(tmp3), names(tmp4)))), function(x){do.call(c, list(tmp3, tmp4))[[x]]})
purrr::map_chr(as.character(sort(as.integer(c(names(tmp3), names(tmp4))))), function(x){c(tmp3, tmp4)[[x]]})
