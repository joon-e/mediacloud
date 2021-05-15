
<!-- README.md is generated from README.Rmd. Please edit that file -->

# mediacloud

<!-- badges: start -->

<!-- badges: end -->

R wrapper package for the [MediaCloud](https://mediacloud.org/) API.

## Installation

You can install the development version of mediacloud from GitHub with:

``` r
#install.packages("remotes")
remotes::install_github("joon-e/mediacloud")
```

## Usage

``` r
library(mediacloud)
```

### Authentication

Register for a MediaCloud account
[here](https://explorer.mediacloud.org/#/user/signup). The API key can
be passed directly to functions with the `key` argument. If no key is
provided, then the package will look for one in the environment variable
`MEDIACLOUD_API_KEY`. Thus, the easiest way to authenticate is to store
your key using `Sys.setenv()`:

``` r
Sys.setenv(MEDIACLOUD_API_KEY = "YOUR_KEY_GOES_HERE")
```

### Search media

Search for media outlets with `search_media()`:

``` r
search_media(tag = "Germany___National", n = 10)
#> # A tibble: 10 x 5
#>    media_id name            url                    start_date          tags     
#>       <int> <chr>           <chr>                  <dttm>              <named l>
#>  1    19831 Spiegel         http://www.spiegel.de  2013-06-10 00:00:00 <list [1~
#>  2    20001 taz.de          http://www.taz.de      2017-04-03 00:00:00 <list [1~
#>  3    21558 neues-deutschl~ http://www.neues-deut~ 2017-04-03 00:00:00 <list [1~
#>  4    21854 jungewelt.de    http://www.jungewelt.~ 2018-06-04 00:00:00 <list [2~
#>  5    21917 berlinerumscha~ http://www.berlinerum~ 2014-12-29 00:00:00 <list [7~
#>  6    22009 bild.de         http://www.bild.de     2013-06-10 00:00:00 <list [2~
#>  7    23037 manager-magazi~ http://www.manager-ma~ 2017-04-10 00:00:00 <list [9~
#>  8    23538 n-tv.de         http://www.n-tv.de     2017-04-03 00:00:00 <list [9~
#>  9    38697 zeit            http://www.zeit.de/in~ 2013-03-11 00:00:00 <list [2~
#> 10    39206 Tagespiegel     http://www.tagesspieg~ 2013-03-18 00:00:00 <list [2~
```

This is mainly useful for matching media outlets with their MediaCloud
`media_id`.

### Search stories

Search for stories with `search_stories()`:

``` r
stories <- search_stories(title = "dogecoin", media_id = c(19831, 38697), after_date = "2021-05-01")
stories
#> # A tibble: 3 x 9
#>   stories_id media_id publish_date        title      url        processed_stori~
#>        <int>    <int> <dttm>              <chr>      <chr>                 <dbl>
#> 1 1922328226    19831 2021-05-05 13:35:13 Dogecoin:~ https://w~       2328683297
#> 2 1925893908    38697 2021-05-09 07:10:42 Dogecoin:~ https://w~       2331981923
#> 3 1926994811    19831 2021-05-10 12:44:34 Elon Musk~ https://w~       2333054504
#> # ... with 3 more variables: media_name <chr>, collect_date <dttm>,
#> #   tags <named list>
```

The function provides a simplified interface for writing the [Solr
queries](https://mediacloud.org/support/query-guide/) that MediaCloud
parses to search for stories (`q` and `fq` parameters in the API call).
This includes the following optional arguments:

  - `text` and `title`: Character vector passed to full text search and
    title-only search, respectively. If the character vector contains
    more than one element, they will be connected with `OR` in the call.
  - `media_id`: Limit to stories from media outlets with these
    `media_id`
  - `after_date` and `before_date`: Limit to stories published
    after/before these dates. Should be a date string that can be
    interpreted as a `POSIXct` object, e.g., `"2021-01-01"` or
    `"2021-12-24 09:00:00"`. Note that `00:00:00` will be added if only
    passing a date, but no time, and boundaries are inclusive, so
    setting `after_date` to `"2021-01-01"` will include stories
    published at `2021-01-01 00:00:00` and later.

Use the argument `n` to control the maximum number of results returned
with one call (`<= 1000`). Note that the returned object also includes
the `processed_stories_id`, which can be passed to the argument
`last_processed_stories_id` to paginate over results.

### Get word matrices

Get [Tidytext](https://juliasilge.github.io/tidytext/)-style word
matrices associated with those stories with `get_word_matrices()`. This
uses the same arguments as `search_stories()`, but is most useful to
obtain word matrices for stories found with `search_stories()`:

``` r
wm <- get_word_matrices(stories_id = stories$stories_id)
wm
#> # A tibble: 290 x 4
#>    stories_id word_counts word_stem       full_word      
#>    <chr>            <int> <chr>           <chr>          
#>  1 1922328226           1 ein             eine           
#>  2 1922328226           1 parodi          parodie        
#>  3 1922328226           1 derzeit         derzeit        
#>  4 1922328226           1 las             las            
#>  5 1922328226           1 kryptowährungen kryptowährungen
#>  6 1922328226           1 sotschi         sotschi        
#>  7 1922328226           2 euro            euro           
#>  8 1922328226           1 überzeugt       überzeugt      
#>  9 1922328226           1 szene           szene          
#> 10 1922328226           1 passiert        passiert       
#> # ... with 280 more rows
```

The word matrices can be tranformed to Quanteda-style DFMs using
`tidytext::cast_dfm()`:

``` r
tidytext::cast_dfm(wm, stories_id, word_stem, word_counts)
#> Document-feature matrix of: 3 documents, 266 features (63.66% sparse) and 0 docvars.
#>             features
#> docs         ein parodi derzeit las kryptowährungen sotschi euro überzeugt
#>   1922328226   1      1       1   1               1       1    2         1
#>   1925893908   0      0       0   0               0       0    0         0
#>   1926994811   0      0       0   0               1       0    0         0
#>             features
#> docs         szene passiert
#>   1922328226     1        1
#>   1925893908     0        0
#>   1926994811     0        0
#> [ reached max_nfeat ... 256 more features ]
```
