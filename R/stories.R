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
                          publish_date = as.POSIXct(.data$publish_date),
                          collect_date = as.POSIXct(.data$collect_date))

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
#' @param media_id Optional media ids (see \code{\link{search_media}}) passed to the
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
                          publish_date = as.POSIXct(.data$publish_date),
                          collect_date = as.POSIXct(.data$collect_date))

    }

    # Return
    return(stories)
}

#' Count stories and tags
#'
#' Search for stories with various parameters. Multiple parameters
#' will be connected with AND in the call.
#' `count_stories` gets the total number of stories, possibly by date. `count_tags` gets the total number of stories by \href{https://mediacloud.org/support/list-of-tags}{tags}.   
#' @inheritParams search_stories
#' @param split logical, either or not to split the count by `split_period`, default to TRUE
#' @param split_period character, split the couny by this granularity, possible values are "day", "week", "month", and "year"
#' @return depending on the parameter `tibble`, the return object can be a tibble (`tibble` is TRUE) or a list (`tibble` is FALSE).
#' @export
#' @examples
#' \dontrun{
#' ## search for daily count of articles mentioning "klimawandel" in German media
#' de_media <- search_media(tag = "Germany___National")
#' res_kw <- count_stories(text = "klimawandel", after_date = "2021-01-01",
#' before_date = "2021-12-22", media_id = de_media$media_id,
#' split = TRUE, split_period = "day")
#' ## search for popular named entities in articles mentioning "covid" in German media
#' count_tags(text = "covid", media_id = de_media$media_id, n = 100,
#' after_date = "2020-01-01", tag_sets_id = "2389")
#' }
count_stories <- function(text = NULL, title = NULL, media_id = NULL,
                          after_date = NULL, before_date = NULL,
                          split = TRUE, split_period = "day",
                          key = NULL, tibble = TRUE) {
    if (isTRUE(split) & !split_period %in% c("day", "week", "month", "year")) {
        stop("Unknown `split_period` value: possible values are \"day\", \"week\", \"month\", and \"year\".", call. = FALSE)
    }
    ep <- "stories_public/count"
    q <- build_solr_query(text = text, title = title,
                          media_id = media_id)
    if (all(is.null(after_date), is.null(before_date))) {
        fq <- NULL
    } else {
        if (is.null(after_date)) {
        after_date <- paste(lubridate::format_ISO8601(as.POSIXct("1970-01-01")), "Z", sep = "")
    } else {
        after_date <- paste(lubridate::format_ISO8601(as.POSIXct(after_date)), "Z", sep = "")
    }
    if (is.null(before_date)) {
        before_date <- paste(lubridate::format_ISO8601(as.POSIXct(Sys.Date())), "Z", sep = "")
    } else {
        before_date <- paste(lubridate::format_ISO8601(as.POSIXct(before_date)), "Z", sep = "")
    }
        fq <- glue::glue("publish_date:[{after_date} TO {before_date}]")
    }
    if (is.null(key)) key <- Sys.getenv("MEDIACLOUD_API_KEY")
    params <- list(q = q, fq = fq, key = key)
    if (isTRUE(split)) {
        params[["split"]] <- "1"
        params[["split_period"]] <- split_period
    }
    url <- create_mc_url(ep,
                     parameters = params)
    stories <- call_mc_api(url)
    if (isTRUE(tibble)) {
        if (split) {
            stories <- stories$counts %>%
                purrr::map_dfr(magrittr::extract, c("date", "count")) %>%
                dplyr::mutate(date = as.POSIXct(.data$date))
        } else {
            stories <- tibble::tibble("count" = stories$count)
        }        
    }
    return(stories)
}

#' @param n numeric, maximum number of tags to return
#' @param tag_sets_id character, if not NULL, only tags belonging to this tag sets is returned. For example, the tag set id of "2389" is tag set of people as identified by the CLIFF named-entity annotator.
#' @rdname count_stories
#' @export
count_tags <- function(text = NULL, title = NULL, media_id = NULL,
                       after_date = NULL, before_date = NULL,
                       n = 1000, tag_sets_id = NULL, key = NULL,
                       tibble = TRUE) {
    ep <- "stories_public/tag_count"
    q <- build_solr_query(text = text, title = title,
                          media_id = media_id)
    if (all(is.null(after_date), is.null(before_date))) {
        fq <- NULL
    } else {
        if (is.null(after_date)) {
        after_date <- paste(lubridate::format_ISO8601(as.POSIXct("1970-01-01")), "Z", sep = "")
    } else {
        after_date <- paste(lubridate::format_ISO8601(as.POSIXct(after_date)), "Z", sep = "")
    }
    if (is.null(before_date)) {
        before_date <- paste(lubridate::format_ISO8601(as.POSIXct(Sys.Date())), "Z", sep = "")
    } else {
        before_date <- paste(lubridate::format_ISO8601(as.POSIXct(before_date)), "Z", sep = "")
    }
        fq <- glue::glue("publish_date:[{after_date} TO {before_date}]")
    }
    if (is.null(key)) key <- Sys.getenv("MEDIACLOUD_API_KEY")
    params <- list(q = q, fq = fq, key = key, limit = n, tag_sets_id = tag_sets_id)
    url <- create_mc_url(ep,
                     parameters = params)
    stories <- call_mc_api(url)
    if (isTRUE(tibble)) {
        stories <- stories %>% purrr::map_dfr(magrittr::extract, c("tags_id", "count", "tag", "tag_set_label", "tag_set_name", "tag_sets_id")) %>% dplyr::arrange(dplyr::desc(.data$count))
    }
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
            tidyr::hoist(.data$word_forms, word_stem = 1, full_word = 2) %>%
            dplyr::mutate(word_counts_id = .data$word_counts_id - 1) # R starts to count at 1

        # Word matrix
        word_matrix <- word_matrix %>%
            tibble::enframe(name = "stories_id", value = "word_counts") %>%
            tidyr::unnest_longer(.data$word_counts) %>%
            dplyr::mutate(word_counts_id = as.integer(.data$word_counts_id))

        # To tibble
        wm <- word_matrix %>%
            dplyr::left_join(word_list, by = "word_counts_id") %>%
            dplyr::select(-.data$word_counts_id)

    }

    # Return
    return(wm)
}
