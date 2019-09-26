base_url <- "http://ec.europa.eu/eurostat/wdds/rest/data/v2.1/json/"

#' @importFrom attempt stop_if_not
#' @importFrom curl has_internet
check_internet <- function(){
  stop_if_not(.x = has_internet(), msg = "Please check your internet connection")
}

#' @importFrom httr status_code
check_status <- function(resp){
  parsed <- jsonlite::fromJSON(httr::content(resp, "text"), simplifyVector = FALSE)
  stop_if_not(.x = status_code(resp),
              .p = ~ .x == 200,
              msg = sprintf("eurostat API request failed [%s]\n%s",
                            status_code(resp),
                            parsed$error$label))
}
