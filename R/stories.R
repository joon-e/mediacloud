#' Get single story
get_single_story <- function(story_id, key = NULL, tibble = TRUE) {

    # Define endpoint
    ep <- "stories_public/single"

    # Read Key
    if (is.null(key)) key <- Sys.getenv("MEDIACLOUD_API_KEY")

    # Create URL
    url <- create_mc_url(ep, path = story_id, key = key)

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
search_stories <- function(text = NULL, title = NULL, media_id = NULL, stories_id = NULL,
                           after_date = NULL, before_date = NULL,
                           n = 20, last_processed_stories_id = NULL,
                           key = NULL, tibble = TRUE) {

    # Define endpoint
    ep <- "stories_public/list"

    # Create query
    q <- parse_solr_query(text = text, title = title,
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
get_word_matrices <- function(text = NULL, title = NULL,
                              media_id = NULL, stories_id = NULL,
                              after_date = NULL, before_date = NULL,
                              n = 20, stopword_length = NULL,
                              key = NULL, tibble = TRUE) {

    # Define endpoint
    ep <- "stories_public/word_matrix"

    # Create query
    q <- parse_solr_query(text = text, media_id = media_id, stories_id = stories_id)

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
