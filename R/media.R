#' Search media
#'
#' Search for media outlets by name and/or tag.
#'
#' @export
#'
#' @param name Media name to search for.
#' @param tag MediaCloud tag to search for.
#' @param n Maximum number of results to return.
#' @param key MediaCloud API key. Will be read from environment
#'   variable 'MEDIACLOUD_API_KEY' if set to `NULL` (default).
#' @param tibble Logical indicating whether result should be returned as
#'   a tibble. Default to `TRUE`. If set to `FALSE`, the unedited content
#'   of the HTTP response will be returned instead.
#'
#' @examples
#' \dontrun{
#' search_media(name = "New York")
#' search_media(tag = "Germany___National")
#' }
#'
#' @return A tibble containing information about found media outlets.
search_media <- function(name = NULL, tag = NULL, n = 20, key = NULL, tibble = TRUE) {

    # Check
    stopifnot(n > 0, n <= 100)

    # Define endpoint
    ep <- "media/list"

    # Read Key
    if (is.null(key)) key <- Sys.getenv("MEDIACLOUD_API_KEY")

    # Parameters
    search_params <- list(name = name,
                          tag_name = tag,
                          rows = n)

    # Create URL
    url <- create_mc_url(ep, parameters = search_params, key = key)

    # Call
    media <- call_mc_api(url)

    # Transform to tibble
    if (tibble) {

        # Separately extract tags as list
        tags <- purrr::map(media, magrittr::extract, "media_source_tags") %>%
            purrr::flatten()

        # Define extracted fields
        fields <- c("media_id", "name", "url", "start_date")

        # To tibble
        media <- media %>%
            purrr::map_dfr(magrittr::extract, fields) %>%
            dplyr::mutate(tags = tags,
                          start_date = as.POSIXct(start_date))

    }

    # Return
    return(media)
}
