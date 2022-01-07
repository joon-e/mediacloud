function (redact) {
    gsub_response(redact, Sys.getenv("MEDIACLOUD_API_KEY"), "1337")
}
