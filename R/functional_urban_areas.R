#' Download Functional Urban Areas datasets.
#'
#' Description from eurostat (see link for reference).
#' The Functional Urban Area consists of a city and its commuting zone. (This
#' was formerly known as larger urban zone (LUZ)).
#'
#' Download data pertaining to Functional Urban Areas from eurostat. Cities
#' here refers to the major ubran area in which the Functional Urban Area
#' belongs to.
#'
#' @importFrom magrittr "%>%"
#'
#' @param dataset The dataset code pertaining to the variable of interest.
#' @param cities The functional urban area code or a vector of codes for the
#'   functional urban areas of interest.
#' @param language The language of the data.
#' @param props An option to include proportional data along with absolute
#'   values.
#' @return A tibble of with the attributes `fua_code`, `fua`, `year`, `value`,
#'   `code` and `variable`.
#' @seealso \url{https://ec.europa.eu/eurostat/web/cities/spatial-units}
#' @export
#' @examples \dontrun{
#' library(reurostat)
#'
#' brus_antw <- get_fua(dataset = "urb_lpop1", cities = c("BE002L2", "BE001L2")
#' }
get_fua <- function(dataset, cities, language = "en", props = FALSE){
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

  value <- purrr::map_chr(as.character(sort(as.integer(c(names(data),
                                                         names(null_data))))),
                          function(x){
                            c(data, null_data)[[x]]
                          })
  value[value == ":"] <- NA

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
    value = as.numeric(value),
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
