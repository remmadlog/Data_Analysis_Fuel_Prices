Daily German Fuel Price Analysis (2022)
================

# Overview for fuel prices for 2022

## Loading and transforming

Loading data for 2022

``` r
agg_2022 <- read.csv("Datasets/German Retail Fuel Price Data 2014-2023/aggregated_years/aggregated2022.csv")
```

Loading library `tidyverse`

``` r
library(tidyverse)
```

Transforming `fuel` columns into one column plus an additional lable
column

``` r
stacked_2022 <- agg_2022 %>%
  pivot_longer(diesel:e10, names_to = "fuel", values_to = "price")
```

## Generating an overview

Generate a short overview of fuel prices for 2022 by month

``` r
ov_month <- stacked_2022 %>%
  select(month, fuel, price) %>%
  filter(price>1) %>%
  group_by(month,fuel) %>%
  summarise(average = mean(price),
            min = min(price),
            max = max(price),
            median = median(price)
  )
```

``` r
print(ov_month)
```

    ## # A tibble: 36 × 6
    ## # Groups:   month [12]
    ##    month fuel   average   min   max median
    ##    <int> <chr>    <dbl> <dbl> <dbl>  <dbl>
    ##  1     1 diesel    1.60  1.05  1.96   1.60
    ##  2     1 e10       1.68  1.09  2.05   1.67
    ##  3     1 e5        1.73  1.12  2.11   1.73
    ##  4     2 diesel    1.67  1.07  2.04   1.66
    ##  5     2 e10       1.75  1.13  2.11   1.74
    ##  6     2 e5        1.81  1.16  2.17   1.80
    ##  7     3 diesel    2.14  1.27  2.46   2.14
    ##  8     3 e10       2.06  1.08  3.84   2.06
    ##  9     3 e5        2.12  1.12  3.61   2.12
    ## 10     4 diesel    2.02  1.38  2.45   2.02
    ## # ℹ 26 more rows

Generate a short overview of fuel prices for 2022 by station

``` r
ov_station <- stacked_2022 %>%
  select(station_uuid, fuel, price) %>%
  # try to drop false entries
  filter(price>1) %>%
  group_by(station_uuid, fuel) %>%
  # only consider if we have at least 6 month of data
  filter(n()>5) %>%
  summarise(average = mean(price),
            min = min(price),
            max = max(price),
            median = median(price)
  ) %>%
  arrange(fuel,average)
```

``` r
head(ov_station)
```

    ## # A tibble: 6 × 6
    ## # Groups:   station_uuid [6]
    ##   station_uuid                         fuel   average   min   max median
    ##   <chr>                                <chr>    <dbl> <dbl> <dbl>  <dbl>
    ## 1 02e8c37e-28fa-4755-a718-e91f345e85f5 diesel    1.35  1.08  1.56   1.35
    ## 2 32882b06-b228-47c1-b841-f92d8fb21888 diesel    1.50  1.19  1.63   1.59
    ## 3 00061777-7224-4444-8888-acdc00000001 diesel    1.56  1.09  1.92   1.62
    ## 4 577b85e6-87ec-439e-bfb4-06af5635c980 diesel    1.59  1.58  1.62   1.58
    ## 5 00061467-0012-4444-8888-acdcffffffff diesel    1.64  1.37  2.09   1.63
    ## 6 00060054-0001-4444-8888-acdc00000001 diesel    1.68  1.21  2.08   1.69

Sorted output of cheapest stations

``` r
# cheapest diesel stations:
ov_station %>%
  filter(fuel == "diesel") %>%
  head()
```

    ## # A tibble: 6 × 6
    ## # Groups:   station_uuid [6]
    ##   station_uuid                         fuel   average   min   max median
    ##   <chr>                                <chr>    <dbl> <dbl> <dbl>  <dbl>
    ## 1 02e8c37e-28fa-4755-a718-e91f345e85f5 diesel    1.35  1.08  1.56   1.35
    ## 2 32882b06-b228-47c1-b841-f92d8fb21888 diesel    1.50  1.19  1.63   1.59
    ## 3 00061777-7224-4444-8888-acdc00000001 diesel    1.56  1.09  1.92   1.62
    ## 4 577b85e6-87ec-439e-bfb4-06af5635c980 diesel    1.59  1.58  1.62   1.58
    ## 5 00061467-0012-4444-8888-acdcffffffff diesel    1.64  1.37  2.09   1.63
    ## 6 00060054-0001-4444-8888-acdc00000001 diesel    1.68  1.21  2.08   1.69

``` r
# cheapest e5 stations:
ov_station %>%
  filter(fuel == "e5") %>%
  head()
```

    ## # A tibble: 6 × 6
    ## # Groups:   station_uuid [6]
    ##   station_uuid                         fuel  average   min   max median
    ##   <chr>                                <chr>   <dbl> <dbl> <dbl>  <dbl>
    ## 1 21656c59-4297-4655-9f8b-dc8bffac6ace e5       1.30  1.15  1.45   1.30
    ## 2 02e8c37e-28fa-4755-a718-e91f345e85f5 e5       1.44  1.33  1.53   1.46
    ## 3 00061777-7224-4444-8888-acdc00000001 e5       1.55  1.20  1.88   1.56
    ## 4 dedf6ac5-4dc1-42ab-95e1-e16a109380d4 e5       1.61  1.61  1.61   1.61
    ## 5 00060054-0001-4444-8888-acdc00000001 e5       1.66  1.26  2.09   1.60
    ## 6 577b85e6-87ec-439e-bfb4-06af5635c980 e5       1.72  1.70  1.76   1.71

``` r
# cheapest e10 stations:
ov_station %>%
  filter(fuel == "e10") %>%
  head()
```

    ## # A tibble: 6 × 6
    ## # Groups:   station_uuid [6]
    ##   station_uuid                         fuel  average   min   max median
    ##   <chr>                                <chr>   <dbl> <dbl> <dbl>  <dbl>
    ## 1 17a2706b-8365-438b-0ce0-0f923a13d177 e10      1.37  1.13  1.41   1.41
    ## 2 00061777-7224-4444-8888-acdc00000001 e10      1.49  1.16  1.83   1.51
    ## 3 1cd72e2a-5f19-423c-bfc1-4c773fdc2847 e10      1.54  1.24  1.60   1.60
    ## 4 00060054-0001-4444-8888-acdc00000001 e10      1.60  1.23  2.02   1.55
    ## 5 a0ed058e-0c77-4a16-8b82-e70f80908a6b e10      1.63  1.25  1.98   1.66
    ## 6 577b85e6-87ec-439e-bfb4-06af5635c980 e10      1.66  1.65  1.7    1.65

## Plotting

Import `ggplot2` for plotting

``` r
library(ggplot2)
```

Generate a line-plot showing the average price per month

``` r
ggplot(data=ov_month, aes(x=month, y=average, group = fuel, color=fuel)) +
  geom_line(stat="identity") +
  labs(title = "Average fuel price per month in 2022",
        y = "price in euro",
        x = "Month") +
  scale_x_continuous(breaks = c(1,2,3,4,5,6,7,8,9,10,11,12)) +
  geom_text(aes(label= round(average,2)),  hjust=1.5, size=3, col = "black") +
  coord_cartesian(ylim=c(1.5,2.2))
```

![](Analysis_Fuel_Price_files/figure-gfm/unnamed-chunk-10-1.png)<!-- -->

# General overview of fuel price

## Loading and transforming

Since we have one file for each year, we need to merge them into one
file

``` r
# generating a list of all files
list_of_files <- list.files(path = "Datasets/German Retail Fuel Price Data 2014-2023/aggregated_years",
                            pattern = "\\.csv$",
                            full.names = TRUE)

# merging all file in the list
agg_14_to_23 <- list_of_files %>%
  map_dfr(read.csv, header=TRUE, fill=TRUE)
```

As before we are transforming `fuel` columns into one column plus an
additional lable column

``` r
stacked_14_23 <- agg_14_to_23 %>%
  pivot_longer(diesel:e10, names_to = "fuel", values_to = "price")
```

## Plotting

We generate an overview too

``` r
ov_year_month <- stacked_14_23 %>%
  select(year, month, fuel, price) %>%
  filter(price>1) %>%
  group_by(year,month,fuel) %>%
  summarise(average = mean(price),
            min = min(price),
            max = max(price),
            median = median(price)
  ) %>%
  arrange(year,month)
```

``` r
print(ov_year_month)
```

    ## # A tibble: 321 × 7
    ## # Groups:   year, month [107]
    ##     year month fuel   average   min   max median
    ##    <int> <int> <chr>    <dbl> <dbl> <dbl>  <dbl>
    ##  1  2014     6 diesel    1.39  1.13  1.54   1.39
    ##  2  2014     6 e10       1.57  1.01  2.00   1.56
    ##  3  2014     6 e5        1.60  1.28  2      1.60
    ##  4  2014     7 diesel    1.38  1.08  1.89   1.38
    ##  5  2014     7 e10       1.55  1.05  2.00   1.55
    ##  6  2014     7 e5        1.59  1.08  1.89   1.59
    ##  7  2014     8 diesel    1.37  1.01  2      1.37
    ##  8  2014     8 e10       1.53  1.06  2      1.53
    ##  9  2014     8 e5        1.57  1.00  2      1.57
    ## 10  2014     9 diesel    1.37  1.04  2      1.37
    ## # ℹ 311 more rows

If we want to generate a plot the show the fuel prices over time we have
to combine the `year` and `month` into `date`. Therefore, we load the
`zoo` library

``` r
library(zoo)
```

Now we combine `year` and `month` into `date`

``` r
# combine year and month to a new date column
ov_year_month$date <- zoo::as.yearmon(paste(ov_year_month$year, ov_year_month$month), "%Y %m")
```

``` r
head(ov_year_month)
```

    ## # A tibble: 6 × 8
    ## # Groups:   year, month [2]
    ##    year month fuel   average   min   max median date     
    ##   <int> <int> <chr>    <dbl> <dbl> <dbl>  <dbl> <yearmon>
    ## 1  2014     6 diesel    1.39  1.13  1.54   1.39 Jun 2014 
    ## 2  2014     6 e10       1.57  1.01  2.00   1.56 Jun 2014 
    ## 3  2014     6 e5        1.60  1.28  2      1.60 Jun 2014 
    ## 4  2014     7 diesel    1.38  1.08  1.89   1.38 Jul 2014 
    ## 5  2014     7 e10       1.55  1.05  2.00   1.55 Jul 2014 
    ## 6  2014     7 e5        1.59  1.08  1.89   1.59 Jul 2014

We are now able to create a line plot the provides the fuel prices from
2014 to 2023

``` r
ggplot(data=ov_year_month, aes(x=date, y=average, group = fuel, color=fuel)) +
  geom_line(stat="identity") +
  labs(title = "Average fule price from 2014 to 2023",
        y = "price in euro",
        x = "Year/Month") +
  coord_cartesian(ylim=c(1,2.2))
```

![](Analysis_Fuel_Price_files/figure-gfm/unnamed-chunk-18-1.png)<!-- -->
