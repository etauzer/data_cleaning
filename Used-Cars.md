Data Cleaning Project Used Cars in California
================

The purpose of this project is for me to get experience with data
cleaning using both base R and tidyverse (dplyr) libraries along with
regex patterns that will be used to extract missing values from other
variables. The dataset is on used car listing from Craigslist. The
dataset is pretty messy with a lot of duplicate observation with
different values given (e.g. the same car may be posted multiple times
with different prices or with the lister omitting certain information
for specific variables). Listers may also place relevant information in
the description instead of placing it in the designated column -
something that we can try to fix using regex patterns to remedy.

# Libraries and Data Importing

``` r
library(tidyverse)
```

    ## -- Attaching packages --------------------------------------- tidyverse 1.3.1 --

    ## v ggplot2 3.3.5     v purrr   0.3.4
    ## v tibble  3.1.5     v dplyr   1.0.7
    ## v tidyr   1.1.4     v stringr 1.4.0
    ## v readr   2.0.2     v forcats 0.5.1

    ## -- Conflicts ------------------------------------------ tidyverse_conflicts() --
    ## x dplyr::filter() masks stats::filter()
    ## x dplyr::lag()    masks stats::lag()

``` r
vehicles <- read_csv("C:/Users/Wetauzer/Desktop/data/unsorted/vehicles.csv")
```

    ## New names:
    ## * `` -> ...1

    ## Rows: 458213 Columns: 26

    ## -- Column specification --------------------------------------------------------
    ## Delimiter: ","
    ## chr  (18): url, region, region_url, manufacturer, model, condition, cylinder...
    ## dbl   (7): ...1, id, price, year, odometer, lat, long
    ## dttm  (1): posting_date

    ## 
    ## i Use `spec()` to retrieve the full column specification for this data.
    ## i Specify the column types or set `show_col_types = FALSE` to quiet this message.

# Data Cleaning

## Summary of Vehicles

To start off, we will restrict the dataset to just cars in California to
make the dataset easier to handle. The variables that we will focus on
are price, year, manufacturer, region, color, condition, mileage, and
title status. A few more columns will be retained to help remove
duplicates

``` r
# Restrict data to just California
cars_df <- vehicles %>% filter(state=="ca")

# Select just the columns of interest
cars_df <- cars_df %>%
  select(price, year, manufacturer, model, VIN, region, paint_color, condition, odometer, title_status, description, id, posting_date)

# Check for NA values in the data
round(colSums(is.na(cars_df))/colSums(is.na(cars_df)|!is.na(cars_df)),3)
```

    ##        price         year manufacturer        model          VIN       region 
    ##        0.000        0.001        0.035        0.012        0.363        0.000 
    ##  paint_color    condition     odometer title_status  description           id 
    ##        0.303        0.349        0.112        0.006        0.000        0.000 
    ## posting_date 
    ##        0.000

There are 51,856 listings and some NAs in possibly important columns
(35% NAs in condition).

Since we will being using the the description to identify missing
values, we should first clean it up by removing punctuation and making
everything lowercase

``` r
# These lines will take a while since some descriptions are quite long

# Make the descriptions lower case
cars_df$description <- tolower(cars_df$description)
# Remove punctuation except for dollar signs as they will be used later
cars_df$description <- gsub('[^$[:alnum:][:space:]]+',' ', cars_df$description)
# Trim whitespace
cars_df$description <- str_squish(cars_df$description)
```

## Duplicates - the first thing we should do is go through and remove duplicate listings. To do this, we can group the duplicates together

and assign all duplicates the same id (we can call it new\_id). The
difficulty is in determining which cars are duplicates.

We will assume that listings with the same year, model, manufacturer and
description are duplicates (this may not always be the case for
dealerships that have several of the same model and just copy/paste the
description but if they’ve included the VIN, the cars will not be
recorded as duplicates). After grouping by those variables, we can group
cars which have a VIN by their VIN. We don’t want to use price or
odometer since people who repost their listing will often update these
values (and sometimes one was omitted by mistake and fixed in the other
posting).

``` r
# We want to use the description to group listings but some descriptions are incredibly long, which makes it difficult for R so we can just 
# take the first 50 characters of the description to match duplicate descriptions
cars_df <- cars_df %>% mutate(desc_mini = substring(description,1, 50))

# Group all of the cars by year, manufacturer, model and description. There may also be cars that were posted with a VIN in a posting a 
# without one in another listing, so we'll include all cars.
cars_df <- cars_df %>%  group_by(year, manufacturer, model, desc_mini) %>% mutate(new_id = max(id)) %>% ungroup()

# For cars that have a VIN, we can group them by their VIN and assign the highest id to the column new_id (since the id is arbitrary it 
# doesn't really matter what it is as long as it allows us to identify which are duplicate listings). Cars with no VIN will all receive the 
# same new_id but we'll update it in the following step 
cars_df[!is.na(cars_df$VIN),] <- cars_df[!is.na(cars_df$VIN),] %>% group_by(VIN) %>% mutate(new_id = max(id))
```

The original dataset had 51,856 but if we were to only count listings
considered unique, we have 33,438 listings. Before we remove the
duplicates, we should make sure that we don’t remove a duplicate that
has relevant data. Also, we need to consider which price to keep and
which mileage to keep. We can first start with the price. We can create
a dataframe that contains each price for each duplicate and then keep
the most recent price (as long as it makes sense - in some cases the
most recent price is omitted or is changed to 0 or some other value that
doesn’t make sense)

``` r
# We can make a dataset with all of the different prices in different columns for each vehicle
prices_wide <- cars_df %>% 
  group_by(new_id) %>%
  arrange(desc(posting_date)) %>%  # arrange so that the most recent posting comes first
  mutate(row = row_number()) %>% 
  pivot_wider(id_cols = new_id, names_from = row, values_from = price)

#  Each car id listed with all prices it was listed at
prices_wide
```

    ## # A tibble: 33,438 x 36
    ## # Groups:   new_id [33,438]
    ##        new_id   `1`   `2`   `3`   `4`   `5`   `6`   `7`   `8`   `9`  `10`  `11`
    ##         <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl>
    ##  1 7240997746     0    NA    NA    NA    NA    NA    NA    NA    NA    NA    NA
    ##  2 7240996551 14590 14590 14590 14590 14590 14990 14990    NA    NA    NA    NA
    ##  3 7240994424 30997    NA    NA    NA    NA    NA    NA    NA    NA    NA    NA
    ##  4 7240994244     0    NA    NA    NA    NA    NA    NA    NA    NA    NA    NA
    ##  5 7240994124 31990 31990    NA    NA    NA    NA    NA    NA    NA    NA    NA
    ##  6 7240993527  1995    NA    NA    NA    NA    NA    NA    NA    NA    NA    NA
    ##  7 7240993149 13000    NA    NA    NA    NA    NA    NA    NA    NA    NA    NA
    ##  8 7240992096 20000    NA    NA    NA    NA    NA    NA    NA    NA    NA    NA
    ##  9 7240991888 16995    NA    NA    NA    NA    NA    NA    NA    NA    NA    NA
    ## 10 7240991818 14590 14590    NA    NA    NA    NA    NA    NA    NA    NA    NA
    ## # ... with 33,428 more rows, and 24 more variables: 12 <dbl>, 13 <dbl>,
    ## #   14 <dbl>, 15 <dbl>, 16 <dbl>, 17 <dbl>, 18 <dbl>, 19 <dbl>, 20 <dbl>,
    ## #   21 <dbl>, 22 <dbl>, 23 <dbl>, 24 <dbl>, 25 <dbl>, 26 <dbl>, 27 <dbl>,
    ## #   28 <dbl>, 29 <dbl>, 30 <dbl>, 31 <dbl>, 32 <dbl>, 33 <dbl>, 34 <dbl>,
    ## #   35 <dbl>

We now have each car combined based on new\_id and are displaying all of
the prices listed for that car. Notice how the second car was listed 7
times with the price being lowered in more recent one being less than
the initial price (they likely adjsuted the price). We just want to keep
the most recent price - but we need to be careful

``` r
# Our methods for detecting duplicates above probably missed some duplicates or recorded a few unique cars as duplicates. We can find the 
# maximum and minimum price for each car and then look at listings in which these are different to see what's going on. 

my.max <- function(x) ifelse(!all(is.na(x)), max(x, na.rm=T), NA)
my.min <- function(x) ifelse(!all(is.na(x)), min(x, na.rm=T), NA)
prices_wide$max <- apply(prices_wide[,-1], MARGIN = 1, FUN = my.max)
prices_wide$min <- apply(prices_wide[,-1], MARGIN = 1, FUN = my.min)
prices_wide <- prices_wide %>% select(new_id, max, min, everything())

# We can look at the listings which have different prices but where no prices are zero:
prices_wide %>% 
  filter((max-min)/max<1 & (max-min)/max>0) %>%  
  arrange(by = `1`)
```

    ## # A tibble: 1,134 x 38
    ## # Groups:   new_id [1,134]
    ##        new_id   max   min   `1`   `2`   `3`   `4`   `5`   `6`   `7`   `8`   `9`
    ##         <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl>
    ##  1 7239624019  4999   103   103  4999    NA    NA    NA    NA    NA    NA    NA
    ##  2 7240604781  6500   134   134  6500    NA    NA    NA    NA    NA    NA    NA
    ##  3 7240514254 10991   155   155 10991    NA    NA    NA    NA    NA    NA    NA
    ##  4 7240397224 10999   161   161 10999    NA    NA    NA    NA    NA    NA    NA
    ##  5 7240494251 11499   169   169 11499    NA    NA    NA    NA    NA    NA    NA
    ##  6 7240445478 12999   193   193 12999    NA    NA    NA    NA    NA    NA    NA
    ##  7 7240420378 13999   209   209 13999    NA    NA    NA    NA    NA    NA    NA
    ##  8 7237835864 10900   224   224 10900    NA    NA    NA    NA    NA    NA    NA
    ##  9 7240604848 10995   226   226 10995    NA    NA    NA    NA    NA    NA    NA
    ## 10 7237991319 11900   245   245 11900    NA    NA    NA    NA    NA    NA    NA
    ## # ... with 1,124 more rows, and 26 more variables: 10 <dbl>, 11 <dbl>,
    ## #   12 <dbl>, 13 <dbl>, 14 <dbl>, 15 <dbl>, 16 <dbl>, 17 <dbl>, 18 <dbl>,
    ## #   19 <dbl>, 20 <dbl>, 21 <dbl>, 22 <dbl>, 23 <dbl>, 24 <dbl>, 25 <dbl>,
    ## #   26 <dbl>, 27 <dbl>, 28 <dbl>, 29 <dbl>, 30 <dbl>, 31 <dbl>, 32 <dbl>,
    ## #   33 <dbl>, 34 <dbl>, 35 <dbl>

In many cases the minimum value is referring to a down payment or
monthly payment amount. In other cases, the cars are just marked down to
increase the chance of selling it (but usually not by such a large
margin). We can consider the percentage that the minimum is of the
maximum to determine what price to use. If the minimum price is less
than 50% of the maximum price, it’s likely a down payment offer and we
will throw that price out

``` r
duplicate_price_function <- function(rowvector, percent_cutoff=.5){
  rowvector <- as.numeric(unlist(rowvector))
  rowvector <- rowvector[!is.na(rowvector)]
  rowvector <- rowvector[rowvector/my.max(rowvector)>percent_cutoff]
  return(rowvector[1])
}

# Since the prices are arranged with the most recent first, the function will filter out prices that are NA or less than the percent cutoff, 
# which is 50% by default.

prices_wide$price <- apply(prices_wide[-1], MARGIN = 1, duplicate_price_function) 
prices_wide <- prices_wide %>% select(new_id, price, max, min, everything())
prices_wide %>% filter(((max-min)/max)>0 & ((max-min)/max)<1) %>% 
  arrange(by = `1`)
```

    ## # A tibble: 1,134 x 39
    ## # Groups:   new_id [1,134]
    ##        new_id price   max   min   `1`   `2`   `3`   `4`   `5`   `6`   `7`   `8`
    ##         <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl>
    ##  1 7239624019  4999  4999   103   103  4999    NA    NA    NA    NA    NA    NA
    ##  2 7240604781  6500  6500   134   134  6500    NA    NA    NA    NA    NA    NA
    ##  3 7240514254 10991 10991   155   155 10991    NA    NA    NA    NA    NA    NA
    ##  4 7240397224 10999 10999   161   161 10999    NA    NA    NA    NA    NA    NA
    ##  5 7240494251 11499 11499   169   169 11499    NA    NA    NA    NA    NA    NA
    ##  6 7240445478 12999 12999   193   193 12999    NA    NA    NA    NA    NA    NA
    ##  7 7240420378 13999 13999   209   209 13999    NA    NA    NA    NA    NA    NA
    ##  8 7237835864 10900 10900   224   224 10900    NA    NA    NA    NA    NA    NA
    ##  9 7240604848 10995 10995   226   226 10995    NA    NA    NA    NA    NA    NA
    ## 10 7237991319 11900 11900   245   245 11900    NA    NA    NA    NA    NA    NA
    ## # ... with 1,124 more rows, and 27 more variables: 9 <dbl>, 10 <dbl>, 11 <dbl>,
    ## #   12 <dbl>, 13 <dbl>, 14 <dbl>, 15 <dbl>, 16 <dbl>, 17 <dbl>, 18 <dbl>,
    ## #   19 <dbl>, 20 <dbl>, 21 <dbl>, 22 <dbl>, 23 <dbl>, 24 <dbl>, 25 <dbl>,
    ## #   26 <dbl>, 27 <dbl>, 28 <dbl>, 29 <dbl>, 30 <dbl>, 31 <dbl>, 32 <dbl>,
    ## #   33 <dbl>, 34 <dbl>, 35 <dbl>

``` r
# The prices look much better now. Now to assign these prices to our cars_df dataframe. There are still a few low prices but we can address them later
cars_df$price <- prices_wide[match(cars_df$new_id, prices_wide$new_id),]$price
cars_df[duplicated(cars_df$new_id)|duplicated(cars_df$new_id,fromLast = T),] %>% arrange(new_id)
```

    ## # A tibble: 26,405 x 15
    ##    price  year manufacturer model   VIN    region paint_color condition odometer
    ##    <dbl> <dbl> <chr>        <chr>   <chr>  <chr>  <chr>       <chr>        <dbl>
    ##  1 18990  2015 dodge        challe~ 2C3CD~ merced silver      good         42681
    ##  2 18990  2015 dodge        challe~ 2C3CD~ santa~ silver      good         42681
    ##  3 24590  2017 chrysler     pacifi~ 2C4RC~ baker~ <NA>        good         32625
    ##  4 24590  2017 chrysler     pacifi~ 2C4RC~ santa~ <NA>        good         32625
    ##  5 16590  2013 lexus        ct 200~ JTHKD~ baker~ red         good         46536
    ##  6 16590  2013 lexus        ct 200~ JTHKD~ santa~ red         good         46536
    ##  7 26590  2017 chrysler     pacifi~ 2C4RC~ baker~ <NA>        good         11882
    ##  8 26590  2017 chrysler     pacifi~ 2C4RC~ santa~ <NA>        good         11882
    ##  9 14990  2015 buick        encore~ KL4CJ~ monte~ white       good         13671
    ## 10 14990  2015 buick        encore~ KL4CJ~ santa~ white       good         13671
    ## # ... with 26,395 more rows, and 6 more variables: title_status <chr>,
    ## #   description <chr>, id <dbl>, posting_date <dttm>, desc_mini <chr>,
    ## #   new_id <dbl>

Note, we are just updating the values in the columns and not removing
duplicates at this point - we want to repeat the process with other
observations so that we do inadvertantly remove relevant data

Next, we want to look at the mileage. Some people update the mileage for
different posts and sometimes it’s just due to a mistake on their part.

``` r
# Just as the function above assigned the most appropriate price, the following will assign the most appropriate mileage
miles_wide <- cars_df %>% 
  group_by(new_id) %>%
  arrange(desc(posting_date)) %>% 
  mutate(row = row_number()) %>% 
  pivot_wider(id_cols = new_id, names_from = row, values_from = odometer)

miles_wide$max <- apply(miles_wide[,-1], MARGIN = 1, FUN=my.max)
miles_wide$min <- apply(miles_wide[,-1], MARGIN = 1, FUN=my.min)
miles_wide[which(miles_wide$max - miles_wide$min >0),]
```

    ## # A tibble: 276 x 38
    ## # Groups:   new_id [276]
    ##        new_id   `1`   `2`    `3`   `4`   `5`   `6`   `7`   `8`   `9`  `10`  `11`
    ##         <dbl> <dbl> <dbl>  <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl>
    ##  1 7240989361 45388 31338     NA    NA    NA    NA    NA    NA    NA    NA    NA
    ##  2 7240985244 11061 20795     NA    NA    NA    NA    NA    NA    NA    NA    NA
    ##  3 7240983333 67973 62806 122701    NA    NA    NA    NA    NA    NA    NA    NA
    ##  4 7240981630 29450 23838     NA    NA    NA    NA    NA    NA    NA    NA    NA
    ##  5 7240977649 34476 28306  14633    NA    NA    NA    NA    NA    NA    NA    NA
    ##  6 7240976445 76543 76544     NA    NA    NA    NA    NA    NA    NA    NA    NA
    ##  7 7240969415 20438 19990  20095    NA    NA    NA    NA    NA    NA    NA    NA
    ##  8 7240963391 28768 29832  34215 38118 21268    NA    NA    NA    NA    NA    NA
    ##  9 7240952544 18823 17027     NA    NA    NA    NA    NA    NA    NA    NA    NA
    ## 10 7240942048 26585 26832     NA    NA    NA    NA    NA    NA    NA    NA    NA
    ## # ... with 266 more rows, and 26 more variables: 12 <dbl>, 13 <dbl>, 14 <dbl>,
    ## #   15 <dbl>, 16 <dbl>, 17 <dbl>, 18 <dbl>, 19 <dbl>, 20 <dbl>, 21 <dbl>,
    ## #   22 <dbl>, 23 <dbl>, 24 <dbl>, 25 <dbl>, 26 <dbl>, 27 <dbl>, 28 <dbl>,
    ## #   29 <dbl>, 30 <dbl>, 31 <dbl>, 32 <dbl>, 33 <dbl>, 34 <dbl>, 35 <dbl>,
    ## #   max <dbl>, min <dbl>

``` r
# Some cars have different values as the car was driven more, some due to the fact that it was listed at 0 in a posting and some possibly due to a mistake. Since odometers only go up, we'll use the maximum value
# Assign the correct miles back to our dataframe
cars_df$odometer <- miles_wide[match(cars_df$new_id, miles_wide$new_id),]$max
```

For the other variables, everything is sensible (i.e. title, color,
condition may be missing in some observations but they most likely won’t
chaneg) so we can just take the first non NA element and assign it to
all of the duplicate listings to ensure no information is loss when we
remove the duplicates. It will also minimize the number of NAs. This may
cause some cars which were actually unique to be overwritten and removed
but that will only occur with listings that had the same year, make,
model and description, and did not report their VIN, so it shouldn’t be
too common.

``` r
check_na_function <- function(rowvector){
  rowvector <- rowvector[!is.na(rowvector)]
  return(rowvector[1])
}
# Variables that we'd like to check:
variables_vector <- c("year", "manufacturer", "model", "paint_color", "condition" ,"title_status")

cars_df
```

    ## # A tibble: 51,856 x 15
    ##    price  year manufacturer model   VIN    region paint_color condition odometer
    ##    <dbl> <dbl> <chr>        <chr>   <chr>  <chr>  <chr>       <chr>        <dbl>
    ##  1 74995  2019 ford         f-250sd 1FT7W~ baker~ <NA>        <NA>         13388
    ##  2  9997  2014 honda        civic ~ <NA>   baker~ black       like new     77000
    ##  3 23997  2015 nissan       armada~ <NA>   baker~ black       excellent    55000
    ##  4 57995  2017 ram          2500    3C6UR~ baker~ silver      <NA>         71567
    ##  5 11900  2014 subaru       forest~ <NA>   baker~ <NA>        excellent   105277
    ##  6 52777  2014 porsche      cayman~ WP0AB~ baker~ <NA>        excellent    28537
    ##  7 32995  2010 ram          3500    3D73Y~ baker~ white       <NA>        168291
    ##  8 42000  2014 ram          <NA>    <NA>   baker~ <NA>        <NA>        114000
    ##  9 49995  2019 chevrolet    silver~ 1GC1K~ baker~ white       <NA>         58010
    ## 10    NA  2012 gmc          yukon   1GKS1~ baker~ <NA>        <NA>         46683
    ## # ... with 51,846 more rows, and 6 more variables: title_status <chr>,
    ## #   description <chr>, id <dbl>, posting_date <dttm>, desc_mini <chr>,
    ## #   new_id <dbl>

``` r
# A loop that will go through each variable and fix things up
for(i in 1:length(variables_vector)){
variable_listings <- cars_df %>%
  group_by(new_id) %>% 
  mutate(row = row_number()) %>% 
  pivot_wider(id_cols = new_id, names_from = row, values_from = variables_vector[i])
  variable_listings$value <- apply(variable_listings[,-1], MARGIN = 1, check_na_function)
  cars_df[[which(colnames(cars_df)==variables_vector[i])]] <- 
    variable_listings[match(cars_df$new_id, variable_listings$new_id),]$value  
}

# Now to delete the duplicates:
cars_df <- cars_df %>% filter(!duplicated(cars_df$new_id))
cars_df <- cars_df %>% select("price", "year", "manufacturer", "model", "odometer", "title_status", "paint_color", "condition", "description", "new_id", "region")
```

After removing duplicates, we have 33,524 cars in our dataframe.

## Years

The Year should be relatively easy to fix. Some cars have year listed as
NA but the year is listed in the description so that should be pretty
easy to fix by extracting possible years from the description using
regex patterns.

``` r
year_na <- cars_df %>% filter(is.na(cars_df$year))
cars_df[is.na(cars_df$year),]$year <- as.numeric(str_extract(year_na$description, "([1-2][0-9]{3})"))
cars_df %>% filter(is.na(cars_df$year)) # Now just one car with an NA, since everything is NA, we'll remove this listing
```

    ## # A tibble: 1 x 11
    ##   price  year manufacturer model odometer title_status paint_color condition
    ##   <dbl> <dbl> <chr>        <chr>    <dbl> <chr>        <chr>       <chr>    
    ## 1  9500    NA <NA>         <NA>        NA <NA>         <NA>        <NA>     
    ## # ... with 3 more variables: description <chr>, new_id <dbl>, region <chr>

``` r
cars_df <- cars_df %>% filter(!is.na(cars_df$year)) # Removing any rows that would have NA in the year column 
summary(cars_df$year)
```

    ##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
    ##    1900    2006    2013    2010    2016    2021

The minimum year is 1900, so that’s probably just carelessness on the
part of the poster

``` r
cars_df %>% filter(cars_df$year<=1900)# Just one car. It has the year in the description so we can fix that easily by using the same regew pattern we used above
```

    ## # A tibble: 1 x 11
    ##   price  year manufacturer model     odometer title_status paint_color condition
    ##   <dbl> <dbl> <chr>        <chr>        <dbl> <chr>        <chr>       <chr>    
    ## 1  4000  1900 <NA>         Suzuki S~    71000 salvage      <NA>        <NA>     
    ## # ... with 3 more variables: description <chr>, new_id <dbl>, region <chr>

``` r
cars_df[cars_df$year<=1900,]$year<- as.numeric(str_extract(cars_df[cars_df$year<=1900,]$description, "([1-2][0-9]{3})"))
summary(cars_df$year)
```

    ##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
    ##    1922    2006    2013    2010    2016    2021

``` r
# The years are all sensible now
```

## Price

The next thing to clean is price. There are both cars with prices too
high and too low, which are likely mistakes. There are also cars where
the price is NA and cars that list a down payment as their price, which
means they don’t accurately reflect the price. We can try to extract
prices from the description using regex patterns as we did with year,
except it may be more difficult in this case

``` r
# Filtering to deal with just cars that have NA in price column
price_na <- cars_df %>% filter(is.na(cars_df$price))
# Find all cars that have a price in the description - we will restrict this to values of 4 digits or more that follow a $ sign
price_in_description <- price_na[str_detect(price_na$description, "[$](\\d{4,})"),]
head(price_in_description$description, 1)
```

    ## [1] "2016 honda accord lx sedan vin ga185143 miles 71 564 2 4l pzev 4cyl fwd auto exterior alloy wheels backup camera featured abs 4 wheel automatic one owner power steering traction control interior cruise control ipod input power door locks power window tilt wheel tinted windows www premierautovisalia com john 559 799 0569 jaymzi 559 556 3311 alejandra 559 836 1245 se habla espanol premier auto sales welcome consignment park and sell call for details over 449 google reviews and growing 4 9 stars earned one happy customer at a time i personally invite you to contact us for your next quality pre owned sell us your trade for more $$$ call for free evaluation bad credit bankruptcy 2nd chance one year employment or same line of employment minimum gross monthly income $1400 proof of residence proof of income 8 complete references names address contact info we can and want to assist you 1st time buyers we have options call for details one year on job or same line of employments minimum gross monthly income $1400 no derogatory or charge offs we believe every customer matters happy to assist on a case by case consigned vehicle are sold as is and as equipped all vehicles with turbo supercharged are sold as is and as equipped we recommend you taking vehicle for 3rd party inspections and or purchase an extended warranty to ensure engine transmission powertrain allow us to save you money on your next purchase 27 years of car auction car buying experience specializing in most brands honda nissan toyota chev dodge chrysler bmw mbz gmc audi vw lexus acura and more"

From just a single observation we can see that this won’t work as the
price reflect the minimum monthly income to receive a loan, so we will
need to consider the words that occur around the price to determine if
it is a usable value or not

``` r
# The following code will give us the a few words before and after where the price occurs
head(unique(str_match_all(price_in_description$description, "(?:[^\\s]+\\s){0,5}[$](\\d{4,})[^\\s]?(?:\\s[^\\s]+){0,5}")))
```

    ## [[1]]
    ##      [,1]                                                                         
    ## [1,] "employment minimum gross monthly income $1400 proof of residence proof of"  
    ## [2,] "employments minimum gross monthly income $1400 no derogatory or charge offs"
    ##      [,2]  
    ## [1,] "1400"
    ## [2,] "1400"
    ## 
    ## [[2]]
    ##      [,1]                           [,2]  
    ## [1,] "$1995 down $298 per month on" "1995"
    ## 
    ## [[3]]
    ##      [,1]                           [,2]  
    ## [1,] "$1495 down $289 per month on" "1495"
    ## 
    ## [[4]]
    ##      [,1]                           [,2]  
    ## [1,] "$1995 down $295 per month on" "1995"
    ## 
    ## [[5]]
    ##      [,1]                                                            [,2]   
    ## [1,] "apr original price is only $12495 plus taxes and fees special" "12495"
    ## 
    ## [[6]]
    ##      [,1]                                                            [,2]   
    ## [1,] "apr original price is only $10495 plus taxes and fees special" "10495"

After going through all of these, there are certain works the preceded
by price. Those words were inserted into the regex pattern to extract
prices - in some cases, the only given price was a down payment or other
so no price could be extracted; they’ll remain as NAs

``` r
# When the following occur, they're followed by a price, so these cars we can adjust the price from NA.
price_in_description <- price_in_description %>% filter(str_detect(price_in_description$description,"(?:Asking Price: |Original price is only |SALE PRICE |precio |price is |EZ Price |asking )[$](\\d{4,})"))
# We can extract these prices and assign them. 
price_in_description$price <- as.numeric(str_match(price_in_description$description, "(?:Asking Price: |Original price is only |SALE PRICE |precio |price is |EZ Price |asking )[$](\\d{4,})")[,2])

# One is a mistake and conjoins price and the year
price_in_description[price_in_description$price>1000000,]$price <- 7999

# Assign the prices back to the cars dataframe and then drop the listings that remain NA
cars_df[match(price_in_description$new_id, cars_df$new_id),]$price <- price_in_description$price
cars_df<- cars_df %>% filter(!is.na(price))
```

We can repeat this process for cars that are too cheap (we’ll use $1000
as a cutoff) as many listers list payments as the price. This won’t
remove all of them, but it should improve the price and we can attempt
to fix others later

``` r
price_cheap <- cars_df %>% filter(cars_df$price <= 1000)
price_in_description <- price_cheap[str_detect(price_cheap$description, "[$](\\d{4,})"),]

#Some have an actual price while others are down payments, installment payments, discounts, etc. When the following occur, they're followed by a price, so these cars we can adjust the price from NA.
price_in_description <- price_in_description %>% filter(str_detect(price_in_description$description,"(?:Asking Price: |Original price is only |SALE PRICE |precio |price is |EZ Price |asking )[$](\\d{4,})"))

# We can extract these prices and assign them. 
price_in_description$price <- as.numeric(str_match(price_in_description$description, "(?:Asking Price: |Original price is only |SALE PRICE |precio |price is |EZ Price |asking )[$](\\d{4,})")[,2])
# The last listings mentions there's a loan on the car so we should remove that as it's not representative of the car's value
price_in_description <- price_in_description[-10,]
# Assign the prices back to the cars dataframe and then drop the listings that remain NA
cars_df[match(price_in_description$new_id, cars_df$new_id),]$price <- price_in_description$price
cars_df <- cars_df %>% filter(price>=1000)
```

Now for the few cars that are priced too high

``` r
cars_df %>% filter(price>400000)
```

    ## # A tibble: 4 x 11
    ##      price  year manufacturer model  odometer title_status paint_color condition
    ##      <dbl> <dbl> <chr>        <chr>     <dbl> <chr>        <chr>       <chr>    
    ## 1   1.11e7  2007 nissan       pathf~   150000 clean        <NA>        <NA>     
    ## 2   2.81e9  2020 gmc          <NA>         NA clean        <NA>        <NA>     
    ## 3   5.99e5  2020 toyota       <NA>         NA clean        <NA>        <NA>     
    ## 4   8.89e5  2005 toyota       land ~    99999 clean        <NA>        <NA>     
    ## # ... with 3 more variables: description <chr>, new_id <dbl>, region <chr>

``` r
# None seem to be real cars and are more like advertisements so we can omit them
cars_df <- cars_df %>% filter(price<400000)
```

With the excessively priced cars removed, the next thing to deal with is
mileage - we can use a similar approach as above to extract mileage from
the description and assign these to the mileage

``` r
# Filter for cars with NA in the odometer column
miles_na <- cars_df %>% filter(is.na(odometer))

# The miles are often followed by the term mi (the pattern will detect 100000 mi or 1000000 miles with or without a space) - The second pattern accounts for mileage with spaces (e.g. 54 000 miles) and the third for mileage the lists thousands with a k (e.g. 54 k miles)
miles_in_description <- miles_na[str_detect(miles_na$description, "(\\d{3,})[^\\s]?mi"),]
miles_in_description$description <- str_replace_all(miles_in_description$description, "(?<=\\d) +(?=\\d)", "")
miles_in_description$description <-str_replace_all(miles_in_description$description, "(\\d+)[kK]", "\\1000")

 
unique(str_match(miles_in_description$description, "(?:[^\\s]+\\s){0,5}(\\d{3,}[^\\s]?)mi[^\\s]?(?:\\s[^\\s]+){0,5}")[,2])
```

    ##  [1] "52000"    "19500"    "13250"    "14100"    "30900"    "13900"   
    ##  [7] "14995"    "12900"    "15495"    "13500"    "9995"     "29995"   
    ## [13] "20995"    "120000"   "128000"   "23011760" "148000"   "170207"  
    ## [19] "165247"   "149000"   "172817"   "298"      "100000"   "165300"  
    ## [25] "250000"   "2021120"  "181000"

``` r
miles_in_description$odometer <- str_match(miles_in_description$description, "(?:[^\\s]+\\s){0,5}(\\d{3,}[^\\s]?)mi[^\\s]?(?:\\s[^\\s]+){0,5}")[,2] 

miles_in_description
```

    ## # A tibble: 28 x 11
    ##    price  year manufacturer model    odometer title_status paint_color condition
    ##    <dbl> <dbl> <chr>        <chr>    <chr>    <chr>        <chr>       <chr>    
    ##  1 35000  2014 ford         <NA>     52000    clean        white       excellent
    ##  2 19500  2011 chevrolet    silvera~ 19500    clean        <NA>        <NA>     
    ##  3 13250  2013 mini         cooper   13250    clean        <NA>        <NA>     
    ##  4 14100  2014 subaru       outback  14100    clean        <NA>        <NA>     
    ##  5 30900  2014 ram          <NA>     30900    clean        <NA>        <NA>     
    ##  6 13900  2014 ford         fusion   13900    clean        <NA>        <NA>     
    ##  7 14995  1999 chevrolet    corvette 14995    clean        <NA>        <NA>     
    ##  8 12900  2012 gmc          acadia ~ 12900    clean        <NA>        <NA>     
    ##  9 15495  2016 honda        hrv      15495    clean        <NA>        <NA>     
    ## 10 13500  2017 ford         c-max se 13500    clean        <NA>        <NA>     
    ## # ... with 18 more rows, and 3 more variables: description <chr>, new_id <dbl>,
    ## #   region <chr>

All of the above mileage is

``` r
# miles_in_description$odometer <- as.numeric(str_extract(miles_in_description$odometer, "\\d{3,}"))
# miles_in_description[miles_in_description$odometer<1000,]$odometer <- miles_in_description[miles_in_description$odometer<1000,]$odometer*1000

cars_df[match(miles_in_description$new_id, cars_df$new_id),]$odometer <- as.numeric(miles_in_description$odometer)

cars_df <- cars_df %>% filter(odometer>=100)

cars_df %>% filter(year < 2010 & odometer < 400)
```

    ## # A tibble: 222 x 11
    ##    price  year manufacturer  model   odometer title_status paint_color condition
    ##    <dbl> <dbl> <chr>         <chr>      <dbl> <chr>        <chr>       <chr>    
    ##  1  3200  2005 honda         accord~      200 clean        <NA>        good     
    ##  2 11995  2006 toyota        tudnra~      192 clean        white       excellent
    ##  3 85000  2007 chevrolet     tahoe        175 clean        <NA>        good     
    ##  4  4900  2009 volkswagen    rabbit       144 clean        grey        excellent
    ##  5  5900  2007 volvo         xc90 3~      140 rebuilt      grey        <NA>     
    ##  6  7000  2004 bmw           645ci ~      129 clean        <NA>        excellent
    ##  7  5900  2004 mercedes-benz cl500        160 clean        silver      <NA>     
    ##  8  1000  1999 bmw           528i         137 salvage      <NA>        good     
    ##  9 35900  1929 ford          model ~      300 clean        red         like new 
    ## 10  5900  1998 toyota        4runne~      221 clean        silver      excellent
    ## # ... with 212 more rows, and 3 more variables: description <chr>,
    ## #   new_id <dbl>, region <chr>

``` r
# Cars that are more than 10 years old and were reported to have miles less than 400 miles are likely recorded in the thousands of miles. From the descriptions, there are occasionally vintage cars from the 20s or 30s with hundreds of miles on them so we'll only change the miles of more modern cars under 500
```

``` r
cars_df[cars_df$odometer<400 & cars_df$year<2010 & cars_df$year>1940,]$odometer <- cars_df[cars_df$odometer<400 & cars_df$year<2010 & cars_df$year>1940,]$odometer*1000

# Remove cars that likely have mistakes in the mileage
cars_df <- cars_df %>% filter(!(odometer<1000 & year <2018 & year > 1940))


# next, to remove cars with excessively high mileage
cars_df <- cars_df %>% filter(odometer<400000)
summary(cars_df$odometer)
```

    ##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
    ##     100   42983   84237   92975  131900  391747

The mileage all appear reasonable as low mileage values all seem to
relate to very new cars as we see below

``` r
summary(cars_df[cars_df$odometer<5000,]$year)
```

    ##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
    ##    1927    2017    2019    2010    2020    2021

# Searching the descriptions for more information

For each of the following, we’ll search through the description and
extract results that match other results that have occurred (for
example, we can extract the manufacturer based on the manufacturers that
have occurred in our dataset - if it happens to be some manufacturer not
in the dataset, it won’t be extracted). We can assume that if we end up
extracting more than one, it is most likely a dealership selling
multiple cars or just a posting that added in a bunch of extra stuff to
get more hits. In this case, we’ll leave it as NA.

Function for extracting

``` r
  check_length <- function(vector){
  if(length(vector) == 1)
    return(vector)
  else
    return(NA)
}

extractor <- function(dataframe, column_name){
  extract_this <- paste(c(unique(dataframe[[which(colnames(dataframe)==column_name)]])), collapse = "|")
  extract_list <- str_extract_all(tolower(dataframe[is.na(dataframe[[which(colnames(dataframe)==column_name)]]),]$description), extract_this)
  extract_list <- lapply(extract_list, unique)
  extract_vector <- unlist(lapply(extract_list, check_length))
  return(extract_vector)
} 

cars_df[is.na(cars_df$manufacturer),]$manufacturer <- extractor(cars_df, "manufacturer")
cars_df[is.na(cars_df$paint_color),]$paint_color <- na_if(extractor(cars_df, "paint_color"), "other")

cars_df <- cars_df %>% select(price, year, manufacturer, odometer, title_status, paint_color, condition, region)
summary(cars_df)
```

    ##      price             year      manufacturer          odometer     
    ##  Min.   :  1000   Min.   :1927   Length:26414       Min.   :   100  
    ##  1st Qu.:  7000   1st Qu.:2007   Class :character   1st Qu.: 42983  
    ##  Median : 13200   Median :2013   Mode  :character   Median : 84237  
    ##  Mean   : 16997   Mean   :2010                      Mean   : 92975  
    ##  3rd Qu.: 22585   3rd Qu.:2016                      3rd Qu.:131900  
    ##  Max.   :339998   Max.   :2021                      Max.   :391747  
    ##  title_status       paint_color         condition            region         
    ##  Length:26414       Length:26414       Length:26414       Length:26414      
    ##  Class :character   Class :character   Class :character   Class :character  
    ##  Mode  :character   Mode  :character   Mode  :character   Mode  :character  
    ##                                                                             
    ##                                                                             
    ## 

``` r
conditionchanger <- function(condition){
  if(is.na(condition))
    return(NA)
  if(condition=="salvage")
    return(1)
  if(condition=="fair")
    return(2)
  if(condition=="like new"|condition=="good")
    return(3)
  if(condition=="new"|condition=="excellent")
    return(4)
  else
    return(condition)
  }
cars_df$condition <- sapply(cars_df$condition, conditionchanger)

summary(cars_df)
```

    ##      price             year      manufacturer          odometer     
    ##  Min.   :  1000   Min.   :1927   Length:26414       Min.   :   100  
    ##  1st Qu.:  7000   1st Qu.:2007   Class :character   1st Qu.: 42983  
    ##  Median : 13200   Median :2013   Mode  :character   Median : 84237  
    ##  Mean   : 16997   Mean   :2010                      Mean   : 92975  
    ##  3rd Qu.: 22585   3rd Qu.:2016                      3rd Qu.:131900  
    ##  Max.   :339998   Max.   :2021                      Max.   :391747  
    ##                                                                     
    ##  title_status       paint_color          condition        region         
    ##  Length:26414       Length:26414       Min.   :1.000   Length:26414      
    ##  Class :character   Class :character   1st Qu.:3.000   Class :character  
    ##  Mode  :character   Mode  :character   Median :3.000   Mode  :character  
    ##                                        Mean   :3.423                     
    ##                                        3rd Qu.:4.000                     
    ##                                        Max.   :4.000                     
    ##                                        NA's   :8232

``` r
# Now for a dataframe that will contain just the response and predictor variables that we may use
cars_df <- cars_df %>% select(price, year, manufacturer, odometer, title_status, paint_color, condition, region)
```

I want to add a couple of variables that tell us a bit about the class
of the car and the country of origin

``` r
country_function <- function(manufacturer){
  if(is.na(manufacturer))
    return(NA)
  if(manufacturer == "chevrolet"|manufacturer =="ford"|manufacturer =="ram"|manufacturer =="saturn"|manufacturer =="chrysler"|
     manufacturer =="gmc"|manufacturer =="cadillac"|manufacturer =="lincoln"|manufacturer =="dodge"|manufacturer =="jeep"|
     manufacturer =="buick"|manufacturer =="pontiac"|manufacturer =="mercury"|manufacturer =="tesla"|manufacturer =="harley-davidson")
     return("USA")
   if(manufacturer == "nissan"|manufacturer =="honda"|manufacturer =="toyota"|manufacturer =="infiniti"|manufacturer =="lexus"|
      manufacturer =="mazda"|manufacturer =="acura"|manufacturer =="subaru"|manufacturer =="mitsubishi"|manufacturer =="datsun")
     return("Japan")
   if(manufacturer == "kia"|manufacturer =="hyundai")
     return("Korea")
   if(manufacturer == "audi"|manufacturer =="bmw"|manufacturer =="porsche"|manufacturer =="mercedes-benz"|manufacturer =="volkswagen")
     return("Germany")
   if(manufacturer == "rover"|manufacturer =="mini"|manufacturer =="land rover"|manufacturer =="volvo"|manufacturer =="jaguar"|
      manufacturer =="aston-martin")
     return("UK")
   if(manufacturer == "volvo")
     return("Sweden")
   if(manufacturer == "alfa-romeo"|manufacturer =="ferrari"|manufacturer =="fiat")
     return("Italy")
   else
     return("missing")
}

cars_df$country <- sapply(cars_df$manufacturer, country_function)
cars_df <- cars_df %>%
  mutate(manufacturer = as.factor(manufacturer), title_status = as.factor(title_status), paint_color = as.factor(paint_color), country = as.factor(country), region = as.factor(region))

round(colSums(is.na(cars_df))/colSums(is.na(cars_df)|!is.na(cars_df)),3)
```

    ##        price         year manufacturer     odometer title_status  paint_color 
    ##        0.000        0.000        0.025        0.000        0.003        0.186 
    ##    condition       region      country 
    ##        0.312        0.000        0.025

The one column that still resulted in a very large portion of NAs was
the condition and paint columns. It is likely that people often do not
include

``` r
# For the final dataframe, with both condition and paint color omitted:
final_df <- drop_na(cars_df %>% 
              select(-c(condition, paint_color))) # 25,681 cars

# If we wanted to do any sort of analysis involving color, we'd need to use the following, which is much more restricted
final_df_condition <- drop_na(cars_df %>% 
              select(-c(condition))) #20,960 cars
final_df_paint <- drop_na(cars_df %>% 
              select(-c(paint_color))) # 17,718 cars


drop_na(cars_df)
```

    ## # A tibble: 15,109 x 9
    ##    price  year manufacturer odometer title_status paint_color condition region  
    ##    <dbl> <dbl> <fct>           <dbl> <fct>        <fct>           <dbl> <fct>   
    ##  1  9997  2014 honda           77000 clean        black               3 bakersf~
    ##  2 23997  2015 nissan          55000 clean        black               4 bakersf~
    ##  3 11900  2014 subaru         105277 salvage      blue                4 bakersf~
    ##  4 24995  2013 chevrolet       87636 clean        black               4 bakersf~
    ##  5 17995  2008 jeep           109768 clean        white               4 bakersf~
    ##  6 30995  2015 gmc             92273 clean        white               4 bakersf~
    ##  7  8300  2006 pontiac         67000 clean        silver              4 bakersf~
    ##  8  3500  2009 dodge          157638 clean        blue                3 bakersf~
    ##  9  5300  2014 chevrolet       84717 clean        blue                3 bakersf~
    ## 10  7500  1999 ford           270000 clean        white               2 bakersf~
    ## # ... with 15,099 more rows, and 1 more variable: country <fct>

``` r
final_df
```

    ## # A tibble: 25,681 x 7
    ##    price  year manufacturer odometer title_status region      country
    ##    <dbl> <dbl> <fct>           <dbl> <fct>        <fct>       <fct>  
    ##  1 74995  2019 ford            13388 clean        bakersfield USA    
    ##  2  9997  2014 honda           77000 clean        bakersfield Japan  
    ##  3 23997  2015 nissan          55000 clean        bakersfield Japan  
    ##  4 57995  2017 ram             71567 clean        bakersfield USA    
    ##  5 11900  2014 subaru         105277 salvage      bakersfield Japan  
    ##  6 52777  2014 porsche         28537 clean        bakersfield Germany
    ##  7 32995  2010 ram            168291 clean        bakersfield USA    
    ##  8 42000  2014 ram            114000 clean        bakersfield USA    
    ##  9 49995  2019 chevrolet       58010 clean        bakersfield USA    
    ## 10 62995  2019 ford            73563 clean        bakersfield USA    
    ## # ... with 25,671 more rows

In the end, we have a data for 25,681 cars which is approximately half
the original dataset. The dataset should be almost entirely free of
duplicates and all NA values have been removed, so it would now be ready
for the analysis stage.
