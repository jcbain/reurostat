#' Download City Statistics datasets.
#'
#' Description from eurostat (see link for reference).
#' The Functional Urban Area consists of a city and its commuting zone. (This
#' was formerly known as larger urban zone (LUZ)).
#'
#' Download data pertaining to Functional Urban Areas, cities and greater cities
#' from eurostat.
#'
#' @importFrom magrittr "%>%"
#'
#' @param dataset The dataset code pertaining to the variable of interest.
#' @param cities The functional urban area code or a vector of codes for the
#'   functional urban areas of interest.
#' @param language The language of the data.
#' @param props An option to include proportional data along with absolute
#'   values.
#' @return A tibble of with the attributes `urb_code`, `city`, `year`, `value`,
#'   `var_code` and `variable`.
#' @seealso \url{https://ec.europa.eu/eurostat/web/cities/spatial-units}
#' @export
#' @examples \dontrun{
#' library(reurostat)
#'
#' brus_antw <- get_city_stat(dataset = "urb_lpop1", cities = c("BE002L2", "BE001L2"))
#' }
get_city_stats <- function(dataset, cities, language = "en", props = FALSE){
  args <- purrr::map(cities, function(x){x}) %>%
    stats::setNames(rep("cities", length(cities)))

  check_internet()
  base_url_modified <- sprintf(paste0(base_url, "%s/%s"), language, dataset)
  resp <- httr::GET(base_url_modified, query = plyr::compact(args))

  check_status(resp)

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
  n_rows <- num_codes * num_cities * num_years

  value <- purrr::map(c(0:(n_rows - 1)), function(x){
    if (rlang::is_empty(data[[as.character(x)]])){
      v <- NA
    } else {
      v <- data[[as.character(x)]]
    }
    v
  }) %>%
    purrr::reduce(c)

  flags <- purrr::map(c(0:(n_rows - 1)), function(x){
      extrac_flags <- stringr::str_extract_all(null_data[[as.character(x)]], "[:alpha:]")

    if (rlang::is_empty(null_data[[as.character(x)]])){
      v <- NA
    } else if(identical(extrac_flags[[1]], character(0))){
      v <- NA
    } else {
      v <- stringr::str_c(extrac_flags[[1]], collapse = "")
    }
    v
  }) %>%
    purrr::reduce(c)

  dplyr::tibble(
    urb_code = rep(purrr::map(names(fua_code), function(x){
      rep(x, num_years)
    }) %>%
      purrr::reduce(c), num_codes),
    city = rep(purrr::map(fua, function(x){
      rep(x, num_years)
    }) %>%
      purrr::reduce(c), num_codes),
    year = rep(purrr::map_int(names(year), function(x){as.integer(year[[x]])}),
               num_codes * num_cities),
    value = as.numeric(value),
    flag = flags,
    var_code = purrr::map(names(category),function(x){
      rep(x, num_years * num_cities)}
    ) %>%
      purrr::reduce(c),
    variable = purrr::map(names(category), function(x){
      rep(category[[x]], num_years * num_cities)
    }) %>%
      purrr::reduce(c)
  )
}



