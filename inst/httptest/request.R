function (request) {
    gsub_request(request, Sys.getenv("MEDIACLOUD_API_KEY"), "1337")
}
