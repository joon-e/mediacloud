#' Get single story
#'
#' Get information about a single MediaCloud story.
#'
#' @export
#'
#' @param stories_id The MediaCloud story id
#' @param key MediaCloud API key. Will be read from environment
#'   variable 'MEDIACLOUD_API_KEY' if set to `NULL` (default).
#' @param tibble Logical indicating whether result should be returned as
#'   a tibble. Default to `TRUE`. If set to `FALSE`, the unedited content
#'   of the HTTP response will be returned instead.
#'
#' @examples
#'
#' \dontrun{
#' get_single_story(27456565)
#' }
#'
#' @return A tibble containing information about the story.
get_single_story <- function(stories_id, key = NULL, tibble = TRUE) {

    # Define endpoint
    ep <- "stories_public/single"

    # Read Key
    if (is.null(key)) key <- Sys.getenv("MEDIACLOUD_API_KEY")

    # Create URL
    url <- create_mc_url(ep, path = stories_id, key = key)

    # Call
    story <- call_mc_api(url)

    # Transform to tibble
    if (tibble) {

        # Separately extract tags as list
        tags <- purrr::map(story, magrittr::extract, "story_tags") %>%
            purrr::flatten()

        # Define extracted fields
        fields <- c("story_id", "media_id", "publish_date", "title", "url",
                    "processed_story_id", "media_name", "collect_date")

        # To tibble
        story <- story %>%
            purrr::map_dfr(magrittr::extract, fields) %>%
            dplyr::mutate(tags = tags,
                          publish_date = as.POSIXct(publish_date),
                          collect_date = as.POSIXct(collect_date))

    }

    # Return
    return(story)
}

#' Search stories
#'
#' Search for stories with various parameters. Multiple parameters
#' will be connected with AND in the call.
#'
#' @export
#'
#' @param text Optional character vector for full text search passed to the
#'   \href{https://mediacloud.org/support/query-guide/}{Solr query}.
#'   If character vector contains more than one element, elements will
#'   be connected with OR.
#' @param title Optional character vector for title search passed to the
#'   \href{https://mediacloud.org/support/query-guide/}{Solr query}.
#'   If character vector contains more than one element, elements will
#'   be connected with OR.
#' @param media_id Optional media ids (see code{\link{search_media()}}) passed to the
#'   \href{https://mediacloud.org/support/query-guide/}{Solr query}.
#'   If vector contains more than one element, elements will
#'   be connected with OR.
#' @param stories_id Optional stories ids passed to the
#'   \href{https://mediacloud.org/support/query-guide/}{Solr query}.
#'   If vector contains more than one element, elements will
#'   be connected with OR.
#' @param after_date Limit results to stories published after this date. Should
#'   be a date string that can be interpreted as a `POSIXct` object, e.g.,
#'   '2021-01-01' or '2021-12-24 09:00:00'. Note that '00:00:00' will be
#'   added if only passing a date.
#' @param before_date Limit results to stories published before this date. Should
#'   be a date string that can be interpreted as a `POSIXct` object, e.g.,
#'   '2021-01-01' or '2021-12-24 09:00:00'. Note that '00:00:00' will be
#'   added if only passing a date.
#' @param n Number of stories to search. Should be <= 1000.
#' @param last_processed_stories_id Limit results to stories with a
#'   `processed_stories_id` greater than this value. Useful for paginating
#'   over results.
#' @inheritParams get_single_story
#'
#' @examples
#' \dontrun{
#' search_stories(text = c("football", "soccer"))
#' search_stories(text = "football NOT soccer")
#' search_stories(title = "football", media_id = c(1, 2))
#' search_stories("football", after_date = "2020-01-01", before_date = "2020-01-02", media_id = 1)
#' }
#'
#' @return A tibble containing information about the stories.
search_stories <- function(text = NULL, title = NULL, media_id = NULL, stories_id = NULL,
                           after_date = NULL, before_date = NULL,
                           n = 20, last_processed_stories_id = NULL,
                           key = NULL, tibble = TRUE) {

    # Define endpoint
    ep <- "stories_public/list"

    # Create query
    q <- build_solr_query(text = text, title = title,
                          media_id = media_id, stories_id = stories_id)

    # Create filter query
    if (all(is.null(after_date), is.null(before_date))) {
        fq <- NULL
    } else {

        # Format after_date
        if (is.null(after_date)) {
            after_date <- paste(lubridate::format_ISO8601(as.POSIXct("1970-01-01")), "Z", sep = "")
        } else {
            after_date <- paste(lubridate::format_ISO8601(as.POSIXct(after_date)), "Z", sep = "")
        }

        # Format before_date
        if (is.null(before_date)) {
            before_date <- paste(lubridate::format_ISO8601(as.POSIXct(Sys.Date())), "Z", sep = "")
        } else {
            before_date <- paste(lubridate::format_ISO8601(as.POSIXct(before_date)), "Z", sep = "")
        }

        fq <- glue::glue("publish_date:[{after_date} TO {before_date}]")
    }

    # Read Key
    if (is.null(key)) key <- Sys.getenv("MEDIACLOUD_API_KEY")

    # Create URL
    url <- create_mc_url(ep,
                         parameters = list(
                             q = q,
                             fq = fq,
                             rows = n,
                             last_processed_stories_id = last_processed_stories_id),
                         key = key)

    # Call
    stories <- call_mc_api(url)

    # Transform to tibble
    if (tibble) {

        # Separately extract tags as list
        tags <- purrr::map(stories, magrittr::extract, "story_tags") %>%
            purrr::flatten()

        # Define extracted fields
        fields <- c("stories_id", "media_id", "publish_date", "title", "url",
                    "processed_stories_id", "media_name", "collect_date")

        # To tibble
        stories <- stories %>%
            purrr::map_dfr(magrittr::extract, fields) %>%
            dplyr::mutate(tags = tags,
                          publish_date = as.POSIXct(publish_date),
                          collect_date = as.POSIXct(collect_date))

    }

    # Return
    return(stories)
}


#' Get word matrices
#'
#' Get word matrices for stories.
#'
#' @export
#'
#' @inheritParams search_stories
#' @param stopword_length if set to 'tiny', 'short', or 'long',
#'   eliminate stop word list of that length
#'
#' @examples
#' \dontrun{
#' get_word_matrices(stories_id = c(1484325770, 24835747, 24840330))
#' get_word_matrices("football", after_date = "2020-01-01", before_date = "2020-01-02", media_id = 1)
#' }
#'
#' @return A tibble with a
#'   \href{https://cran.r-project.org/web/packages/tidytext/vignettes/tidytext.html}{Tidytext-style}
#'   word matrix with one word per row and columns indicating
#'   the `stories_id`, the `word_count`, the `word_stem`, and the most common
#'   `full_word` associated with said stem. Use \code{\link[tidytext]{cast_dfm}}
#'   to transform into a  \href{https://quanteda.io/}{Quanteda} DFM.
get_word_matrices <- function(text = NULL, title = NULL,
                              media_id = NULL, stories_id = NULL,
                              after_date = NULL, before_date = NULL,
                              n = 20, stopword_length = NULL,
                              key = NULL, tibble = TRUE) {

    # Define endpoint
    ep <- "stories_public/word_matrix"

    # Create query
    q <- build_solr_query(text = text, media_id = media_id, stories_id = stories_id)

    # Create filter query
    if (all(is.null(after_date), is.null(before_date))) {
        fq <- NULL
    } else {

        # Format after_date
        if (is.null(after_date)) {
            after_date <- paste(lubridate::format_ISO8601(as.POSIXct("1970-01-01")), "Z", sep = "")
        } else {
            after_date <- paste(lubridate::format_ISO8601(as.POSIXct(after_date)), "Z", sep = "")
        }

        # Format before_date
        if (is.null(before_date)) {
            before_date <- paste(lubridate::format_ISO8601(as.POSIXct(Sys.Date())), "Z", sep = "")
        } else {
            before_date <- paste(lubridate::format_ISO8601(as.POSIXct(before_date)), "Z", sep = "")
        }

        fq <- glue::glue("publish_date:[{after_date} TO {before_date}]")
    }

    # Read Key
    if (is.null(key)) key <- Sys.getenv("MEDIACLOUD_API_KEY")

    # Create URL
    url <- create_mc_url(ep,
                         parameters = list(
                             q = q,
                             fq = fq,
                             rows = n,
                             stopword_length = stopword_length),
                         key = key)

    # Call
    wm <- call_mc_api(url)

    # Transform to tibble
    if (tibble) {

        # Separate
        word_list <- wm$word_list
        word_matrix <- wm$word_matrix

        # Word list
        word_list <- word_list %>%
            tibble::enframe(name = "word_counts_id", value = "word_forms") %>%
            tidyr::hoist(word_forms, word_stem = 1, full_word = 2) %>%
            dplyr::mutate(word_counts_id = word_counts_id - 1) # R starts to count at 1

        # Word matrix
        word_matrix <- word_matrix %>%
            tibble::enframe(name = "stories_id", value = "word_counts") %>%
            tidyr::unnest_longer(word_counts) %>%
            dplyr::mutate(word_counts_id = as.integer(word_counts_id))

        # To tibble
        wm <- word_matrix %>%
            dplyr::left_join(word_list, by = "word_counts_id") %>%
            dplyr::select(-word_counts_id)

    }

    # Return
    return(wm)
}
