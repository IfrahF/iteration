Writing Functions
================
Ifrah Fayyaz
11/4/2021

``` r
library(tidyverse)
library(rvest)
```

## z-scores

``` r
x_vec = rnorm(25, mean = 5, sd = 4)

(x_vec - mean(x_vec)) / sd(x_vec)
```

    ##  [1] -0.09058551 -0.01423400 -1.33938247  0.52523610 -0.14877165  1.66091298
    ##  [7]  0.38505729 -0.99555557  0.61202172 -0.30922629  0.81681204  1.45365263
    ## [13] -1.78763770 -1.53063266 -0.90855650  0.33578473 -0.46440887  1.89019174
    ## [19] -0.38541227  0.95536792 -0.54293375 -0.20854363  0.53564956  0.93960673
    ## [25] -1.38441258

``` r
z_scores = function(x) {
  z = (x - mean(x)) / sd(x)
  return(z)
}

z_scores(x = x_vec)
```

    ##  [1] -0.09058551 -0.01423400 -1.33938247  0.52523610 -0.14877165  1.66091298
    ##  [7]  0.38505729 -0.99555557  0.61202172 -0.30922629  0.81681204  1.45365263
    ## [13] -1.78763770 -1.53063266 -0.90855650  0.33578473 -0.46440887  1.89019174
    ## [19] -0.38541227  0.95536792 -0.54293375 -0.20854363  0.53564956  0.93960673
    ## [25] -1.38441258

``` r
y_vec = rnorm(40, mean = 12, sd = 0.3)

z_scores(y_vec)
```

    ##  [1]  1.37502095 -1.47587749 -1.20448392 -1.04965009 -1.32992223  0.25621953
    ##  [7] -0.43037281  0.18121968 -0.71998784  1.38073806  1.06357453 -0.31394532
    ## [13]  1.14089975 -0.12214876 -0.62777530 -0.74014626  0.19665409  0.49115038
    ## [19]  0.02876051  0.62495301 -0.10344522  1.26623310  0.03740323  0.96885429
    ## [25] -0.37832466  1.35303078 -0.82631009 -0.82643303 -1.50606541  1.15899221
    ## [31]  1.06184949  2.07132214 -0.07245522 -1.90044859  1.47097133 -0.82094583
    ## [37] -1.09519230 -0.45863254 -0.75052698  0.62524280

``` r
mean_and_sd = function(x) {
  if (!is.numeric(x)) {
    stop("x needs to be numeric")
  }
  if (length(x) < 3) {
    stop("x should have at least 3 numbers")
  }
  mean_x = mean(x)
  sd_x = sd(x)
  
  output_df = 
    tibble(
      mean = mean_x,
      sd = sd_x
    )
  return(output_df)
}

mean_and_sd(x_vec)
```

    ## # A tibble: 1 × 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1  7.54  3.27

``` r
mean_and_sd(y_vec)
```

    ## # A tibble: 1 × 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1  12.0 0.249

## Different sample sizes, mean and sd

``` r
sim_data =
  tibble(
    x = rnorm(30, mean = 2, sd = 3)
  )

sim_data %>%
  summarize(
    mean = mean(x),
    sd = sd(x)
  )
```

    ## # A tibble: 1 × 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1  2.24  2.82

## Lets write a function that simulates data, computes the mean and sd

``` r
sim_mean_sd = function(n, mu, sigma) {
  # do checks on inputs
  
      sim_data =
      tibble(
        x = rnorm(n = n, mean = mu, sd = sigma)
      )
    
    sim_data %>%
      summarize(
        mean = mean(x),
        sd = sd(x)
      )
}

sim_mean_sd(30, 5, 2)
```

    ## # A tibble: 1 × 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1  5.26  2.09

``` r
url = "https://www.amazon.com/product-reviews/B00005JNBQ/ref=cm_cr_arp_d_viewopt_rvwer?ie=UTF8&reviewerType=avp_only_reviews&sortBy=recent&pageNumber=1"


get_page_reviews = function(pageurl) {
    page_html = read_html(pageurl)
    review_titles = 
      page_html %>%
      html_nodes(".a-text-bold span") %>%
      html_text()
    
    review_stars = 
      page_html %>%
      html_nodes("#cm_cr-review_list .review-rating") %>%
      html_text() %>%
      str_extract("^\\d") %>%
      as.numeric()
    
    review_text = 
      page_html %>%
      html_nodes(".review-text-content span") %>%
      html_text() %>% 
      str_replace_all("\n", "") %>% 
      str_trim()
    
    reviews = 
      tibble(
        title = review_titles,
        stars = review_stars,
        text = review_text
      )
  return(reviews)
}

base_url = "https://www.amazon.com/product-reviews/B00005JNBQ/ref=cm_cr_arp_d_viewopt_rvwer?ie=UTF8&reviewerType=avp_only_reviews&sortBy=recent&pageNumber="

urls = str_c(base_url, 1:5)

bind_rows(
  get_page_reviews(urls[1]),
  get_page_reviews(urls[2]),
  get_page_reviews(urls[3]),
  get_page_reviews(urls[4]),
  get_page_reviews(urls[5])
)
```

    ## # A tibble: 50 × 3
    ##    title                                                 stars text             
    ##    <chr>                                                 <dbl> <chr>            
    ##  1 it was                                                    5 "mad good yo"    
    ##  2 Fun!                                                      4 "Fun and enterta…
    ##  3 Vintage                                                   5 "Easy to order. …
    ##  4 too many commercials                                      1 "5 minutes into …
    ##  5 this film is so good!                                     5 "VOTE FOR PEDRO!"
    ##  6 Good movie                                                5 "Weird story, go…
    ##  7 I Just everyone to know this....                          5 "VOTE FOR PEDRO …
    ##  8 the cobweb in his hair during the bike ramp scene lol     5 "5 stars for bei…
    ##  9 Best quirky movie ever                                    5 "You all know th…
    ## 10 Classic Film                                              5 "Had to order th…
    ## # … with 40 more rows
