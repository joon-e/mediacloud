#' Construct request URL
#'
#' Construct the URL for the API call
#'
#' @param endpoint MediaCloud API endpoint (without /api/v2)
#' @param path Additional path to add to the endpoint
#' @param parameters A named list with query parameters
#' @param key MediaCloud API key
#'
#' @return A string with the call URL
#'
#' @keywords internal
create_mc_url <- function(endpoint,
                          path = NULL,
                          parameters = NULL,
                          key = Sys.getenv("MEDIACLOUD_API_KEY")) {
    # Parse URL
    url <- httr::parse_url("https://api.mediacloud.org/api/v2")

    # Add endpoint
    url$path <- paste(url$path, endpoint, sep = "/")

    # Add additional path
    if (!is.null(path)) {
        url$path <- paste(url$path, path, sep = "/")
    }

    # Add API Key
    parameters$key <- key

    # Add additional parameters
    if (!is.null(parameters)) {
        url$query <- parameters
    }

    # Build URL
    url <- httr::build_url(url)
    stringr::str_replace_all(url, c("%3A" = ":",
                                    "%2B" = "+"))
}


#' Call API
#'
#' Call API, check for status code and return content
#'
#' @param url A URL string built with create_mc_url()
#'
#' @return JSON content object
call_mc_api <- function(url) {

    # GET
    res <- httr::GET(url)

    # Check
    if (httr::status_code(res) != 200) {
        err_msg <- httr::content(res)$error
        stop(glue::glue("HTTP Code {httr::status_code(res)}: {err_msg}"),
             call. = FALSE)
    }

    # Return
    httr::content(res)
}

#' Solr query
#'
#' Build Solr query from named arguments
#'
#' @param ... Named arguments to build Solr query from
#'
#' @return String with Solr query
#'
#' @keywords internal
build_solr_query <- function(...) {
    query_terms <- list(...)

    # Remove NULLs
    query_terms <- query_terms[lengths(query_terms) != 0]

    # Collapse query function
    collapse_query <- function(value, key, concat = "OR") {

        collapse <- paste(" ", concat, " ", sep = "")

        if (length(value) > 1) {
            value <- stringr::str_c("(", stringr::str_c(value, collapse = collapse), ")")
        }

        stringr::str_c(key, value, sep = ":")
    }

    # Construct query
    query_terms %>%
        purrr::imap(collapse_query) %>%
        stringr::str_c(collapse = "+AND+")
}

#' Check rate limit
#'
#' Check your current rate limit
#'
#' @export
#'
#' @param key MediaCloud API key. Will be read from environment
#'   variable 'MEDIACLOUD_API_KEY' if set to `NULL` (default).
#'
#' @return A tibble with information about the current rate limits.
check_rate_limit <- function(key = NULL) {

    # Define endpoint
    ep <- "auth/profile"

    # Read Key
    if (is.null(key)) key <- Sys.getenv("MEDIACLOUD_API_KEY")

    # Call API
    url <- create_mc_url(ep, key = key)
    res <- call_mc_api(url)

    tibble::enframe(res$limits$weekly) %>%
        tidyr::hoist(value, "limit", "used") %>%
        dplyr::mutate(remaining = limit - used) %>%
        dplyr::rename(type = name, weekly_limit = limit)

}


