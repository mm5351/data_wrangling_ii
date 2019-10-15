reading\_data\_from\_the\_web
================
Martha Mulugeta
10/10/2019

\#\#Get some data read in NSDUH
data

``` r
url = "http://samhda.s3-us-gov-west-1.amazonaws.com/s3fs-public/field-uploads/2k15StateFiles/NSDUHsaeShortTermCHG2015.htm"

drug_use_xml = read_html(url) 
  
table_list = drug_use_xml %>% html_nodes(css = "table")

table_marj =   
  table_list[[1]] %>% 
    html_table() %>% 
    slice(-1) %>% 
    as_tibble()
```

slice to take out first row \[\[\]\] for which table to extract

\#\#Get harry potter data

``` r
hpsaga_html = 
  read_html("https://www.imdb.com/list/ls000630791/")
```

``` r
hp_movie_names = 
  hpsaga_html %>% 
  html_nodes(".lister-item-header a") %>% 
  html_text()

hp_movie_runtime = 
  hpsaga_html %>% 
  html_nodes(".runtime") %>% 
  html_text()

hp_movie_money = 
  hpsaga_html %>%
  html_nodes(".text-small:nth-child(7) span:nth-child(5)") %>%
  html_text()

hp_df =
  tibble(
    title = hp_movie_names,
    runtime = hp_movie_runtime,
    money = hp_movie_money
  )
```

\#\#Get
napolean

``` r
url = "https://www.amazon.com/product-reviews/B00005JNBQ/ref=cm_cr_arp_d_viewopt_rvwer?ie=UTF8&reviewerType=avp_only_reviews&sortBy=recent&pageNumber=1"

dynamite_html = read_html(url)

review_titles = 
  dynamite_html %>%
  html_nodes(".a-text-bold span") %>%
  html_text()

review_stars = 
  dynamite_html %>%
  html_nodes("#cm_cr-review_list .review-rating") %>%
  html_text()

review_text = 
  dynamite_html %>%
  html_nodes(".review-text-content span") %>%
  html_text()

reviews = tibble(
  title = review_titles,
  stars = review_stars,
  text = review_text
)
```

To get reviews on other pages, change “pageNumber=1” at end of URL to
another number

\#\#Using an API

``` r
nyc_water_df = 
  GET("https://data.cityofnewyork.us/resource/waf7-5gvc.csv") %>% 
  content("parsed")
```

    ## Parsed with column specification:
    ## cols(
    ##   year = col_double(),
    ##   new_york_city_population = col_double(),
    ##   nyc_consumption_million_gallons_per_day = col_double(),
    ##   per_capita_gallons_per_person_per_day = col_double()
    ## )

GET is one of the four core API calls, go to this website and get the
information

\#\#JSON format is messier than the CSV version (depicted above)
Requires additional parsing

``` r
nyc_water_df = 
  GET("https://data.cityofnewyork.us/resource/waf7-5gvc.json") %>% 
  content("text") %>% 
  jsonlite::fromJSON() %>% 
  as_tibble()
```

``` r
brfss_data = 
  GET("https://data.cdc.gov/api/views/acme-vg9e/rows.csv?accessType=DOWNLOAD") %>% 
  content("parsed")
```

    ## Parsed with column specification:
    ## cols(
    ##   .default = col_character(),
    ##   Year = col_double(),
    ##   Sample_Size = col_double(),
    ##   Data_value = col_double(),
    ##   Confidence_limit_Low = col_double(),
    ##   Confidence_limit_High = col_double(),
    ##   Display_order = col_double(),
    ##   LocationID = col_logical()
    ## )

    ## See spec(...) for full column specifications.

\#\#Pokemon

``` r
poke = 
  GET("http://pokeapi.co/api/v2/pokemon/1") %>%
  content()

poke$name
```

    ## [1] "bulbasaur"

``` r
poke$height
```

    ## [1] 7

``` r
poke$abilities
```

    ## [[1]]
    ## [[1]]$ability
    ## [[1]]$ability$name
    ## [1] "chlorophyll"
    ## 
    ## [[1]]$ability$url
    ## [1] "https://pokeapi.co/api/v2/ability/34/"
    ## 
    ## 
    ## [[1]]$is_hidden
    ## [1] TRUE
    ## 
    ## [[1]]$slot
    ## [1] 3
    ## 
    ## 
    ## [[2]]
    ## [[2]]$ability
    ## [[2]]$ability$name
    ## [1] "overgrow"
    ## 
    ## [[2]]$ability$url
    ## [1] "https://pokeapi.co/api/v2/ability/65/"
    ## 
    ## 
    ## [[2]]$is_hidden
    ## [1] FALSE
    ## 
    ## [[2]]$slot
    ## [1] 1
