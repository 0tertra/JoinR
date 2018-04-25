#' List devices
#'
#' @param apiKey key to get from join
#'
#' @return devices
#' @export
#' @import
#' httr
#' jsonlite
#'
#' @examples
listDevices <- function(apiKey) {
  joinUrl <- 'https://joinjoaomgcd.appspot.com'
  path <- '/_ah/api/registration/v1/listDevices'
  query <- list(apikey = apiKey)
  url <- modify_url(joinUrl, path = path, query = query)

  response <- GET(url)

  if (http_type(response) != "application/json") {
    stop("API did not return json", call. = FALSE)
  }

  parsed <- fromJSON(content(response, "text"), simplifyVector = TRUE)

  if (http_error(response)) {
    stop(
      sprintf(
        "Join API request failed [%s]\n%s\n<%s>",
        status_code(resp),
        parsed$message,
        parsed$documentation_url
      ),
      call. = FALSE
    )
  }

  if (!parsed$success) {
    stop(
      sprintf(
        "Join API request failed \n %s",
        parsed$errorMessage
      ),
      call. = FALSE
    )
  }

  devices <- parsed$records
}
