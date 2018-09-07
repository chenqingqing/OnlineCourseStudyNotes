#load library 
library(tidyverse)
library(nycflights13)
library(Lahman)
?flights

##filter(): pick observatons by their values 
filter(flights, month == 1, day == 1)
filter(flights, month == 11 | month == 12)
filter(flights, month %in% c(11,12)) #equal to the previous line 

filter(flights, !(arr_delay > 120 | dep_delay > 120) )
filter(flights, arr_delay <= 120, dep_delay <= 120)

###missing values: filter only includes rows where the condition is TRUE, if want to perserce missing values, ask for them explicityly
df <- tibble(x=c(1, NA, 3))
filter(df, x>1)
filter(df, is.na(x) | x>1)

### Exercise 
names(flights)
filter(flights, arr_delay >=120)
filter(flights, dest == 'HOU' | dest == 'IAH')
filter(flights, carrier == 'UA' | carrier == 'AA' | carrier == 'DA')
filter(flights, carrier %in% c('UA', 'AA', 'DA'))
filter(flights, month %in% c(7,8,9))
filter(flights, arr_delay > 120 & dep_delay <=0)
filter(flights, dep_delay > 60 & air_time > 30)
filter(flights, hour >= 0 & hour <= 6)
filter(flights, is.na(dep_time))


## arrange(): reorder the rows 
arrange(flights, year, month, day)
arrange(flights, desc(arr_delay))
### missing values are always sorted at the end
df <- tibble(x = c(5,2, NA))
arrange(df)
arrange(df, desc(x))

### Exercise 
arrange(flights, desc(is.na(dep_time)), dep_time)
arrange(flights, desc(dep_delay))
arrange(flights, air_time)
arrange(flights, desc(distance))
arrange(flights, distance)

## select(): select the variables 
select(flights, year, month, day)
select(flights, year:day)
select(flights, -c(year:day))
?select 
rename(flights, tail_num = tailnum)
### Exercise 
select(flights, time_hour, air_time, everything())
vars <- c("year", "month", "day", "dep_delay", "arr_delay")
select(flights, one_of(vars))
select(flights, contains("TIME"))

## mutate(): add new variables 
flights_sml <- select(flights, year:day, ends_with("delay"), distance, air_time)
flights_sml2 <- mutate(flights_sml, 
       gain = arr_delay - dep_delay, 
       speed = distance/air_time *60)

mutate(flights_sml, 
       gain = arr_delay - dep_delay, 
       hours = air_time / 60,
       gain_per_hour = gain / hours)
transmute(flights_sml, 
          gain = arr_delay - dep_delay, 
          hours = air_time / 60,
          gain_per_hour = gain / hours)
### modular arithmetic 
transmute(flights, 
          dep_time, 
          hour = dep_time %/% 100,
          minute = dep_time %% 100)
### offset: leading or lagging values 
(x <- 1:10)
lag(x)
lead(x)
### cumulative and rolling aggregates 
x 
cumsum(x)
cummean(x)

### ranking 
y <- c(1,2,2,NA,3,4)
min_rank(y)
min_rank(desc(y))
row_number(y)
dense_rank(y)
percent_rank(y)
cume_dist(y)

### summarize():collapses a data frame to a simple row 
summarize(flights, delay = mean(dep_delay, na.rm = TRUE))

by_day <- group_by(flights, year, month, day)
summarise(by_day, delay = mean(dep_delay, na.rm = TRUE))

### combining multiple operations with the pipe 
####duplicated version 
by_dest <- group_by(flights, dest)
delay <- summarize(by_dest,
                   count = n(),
                   dist <- mean(distance, na.rm = TRUE),
                   delay <- mean(arr_delay, na.rm = TRUE))
delay <- filter(delay, count > 20, dest != "HNL")
ggplot(delay,aes(x=dist, y = delay)) +
    geom_point(aes(size = count), alpha = 1/3) + geom_smooth(se = FALSE)

####replace version

delays <- flights %>% 
    group_by(dest) %>%
    summarize(
        count = n(),
        dist = mean(distance, na.rm = TRUE),
        delay = mean(arr_delay, na.rm = TRUE)
    ) %>%
    filter(count > 20, dest != "HNL")

ggplot(data = delays, aes(x=dist, y=delay)) + 
    geom_point(aes(size=count), alpha= 1/3) + 
    geom_smooth(se = FALSE)


### save database 
not_cancelled <- flights %>% 
    filter(!is.na(dep_delay), !is.na(arr_delay))
not_cancelled %>% group_by(year, month, day) %>%
    summarize(mean = mean(dep_delay))

### Counts 
delays <- not_cancelled %>%
    group_by(tailnum) %>%
    summarise(
        delay = mean(arr_delay)
    )
ggplot(delays, aes(x=delay)) + 
    geom_freqpoly(binwidth = 10) 

delays <- not_cancelled %>% 
    group_by(tailnum) %>%
    summarise(
        delay = mean(arr_delay, na.rm = TRUE),
        n = n()
    )
ggplot(delays, aes(x=n, y=delay)) +
    geom_point(alpha = 1/3)

delays %>%
    filter(n > 25) %>%
    ggplot(aes(x=n, y=delay)) +
    geom_point(alpha = 1/10)

Lahman
data(Lahman)
batting <- as_tibble(Lahman::Batting)
batters <- batting %>%
    group_by(playerID) %>%
    summarise(
        ba = sum(H, na.rm = TRUE) / sum(AB, na.rm = TRUE),
        ab = sum(AB, na.rm = TRUE)
    )
batters %>% 
    filter(ab >100) %>%
    ggplot(aes(x=ab, y=ba)) + geom_point() + 
    geom_smooth(se=FALSE)

batters %>%
    arrange(desc(ba))



