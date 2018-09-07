## Exploratory Data Analysis 
library(tidyverse)

ggplot(data = diamonds) + 
    geom_freqpoly(binwidth = 0.1, aes(x = x), color = "red") +
    geom_freqpoly(binwidth = 0.1, aes(x = y), color = "blue") + 
    geom_freqpoly(binwidth = 0.1, aes(x = z), color = "green")

##another way 
diamonds %>% 
    mutate(id = row_number()) %>%
    select(x, y, z, id) %>%
    gather(variable, value, -id) %>%
    ggplot(aes(x=value)) + 
    geom_density() +
    geom_rug() + facet_grid(variable ~ .)

## diamond price 
ggplot(data=diamonds, aes(x=price))+
    geom_histogram(binwidth = 10) + 
    coord_cartesian(xlim = c(0,2000))
## carat 
diamonds %>% group_by(carat) %>% count() %>%
    ggplot() +
    geom_histogram(aes(x = carat)) + 
    coord_cartesian(xlim = c(0,2))

filter(diamonds, carat == 0.99) %>% count()
filter(diamonds, carat == 1) %>% count()

## coord_cartesian
diamonds %>% 
    ggplot() + 
    geom_histogram(aes(x = carat)) +
    coord_cartesian(xlim = c(0,5))


diamonds %>%
    ggplot() + 
    geom_histogram(aes(x = carat)) +
    xlim(c(0,5))

diamonds %>%
    ggplot() +
    geom_histogram(aes(x = carat)) +
    coord_cartesian(ylim = c(0,1000))

diamonds %>%
    ggplot() +
    geom_histogram(aes(x = carat)) +
    ylim(c(0,1000))

## missing value
diamonds2 <- diamonds %>%
     mutate(y=ifelse(y < 3 | y> 20, NA, y))
ggplot(data = diamonds2, aes(x = x, y = y)) +
    geom_point()

ggplot(data=diamonds2, aes(x = x, y = y)) +
    geom_point(na.rm = TRUE)

###
nycflights13::flights %>%
    mutate(
        cancelled = is.na(dep_time),
        sched_hour = sched_dep_time %/% 100,
        sched_min = sched_dep_time %% 100,
        sched_dep_time = sched_hour + sched_min / 60 
    ) %>%
    ggplot(aes(sched_dep_time)) + geom_freqpoly(
        aes(color=cancelled),
        binwidth = 1/4
    )
## covariation 
ggplot(diamonds, aes(x = price)) +
    geom_freqpoly(aes(color = cut), binwidht = 500)

ggplot(diamonds, aes(x = price, y = ..density..)) +
    geom_freqpoly(aes(color = cut), binwidth = 500)
## reorder 
ggplot(mpg, aes(x = class, y = hwy)) + 
    geom_boxplot()

ggplot(mpg, aes(x = class, y = hwy)) +
    geom_boxplot(
        aes(x = reorder(class, hwy, FUN = median),
        y = hwy)
    ) + coord_flip()

library(ggstance)
cancellation <- nycflights13::flights %>% 
    mutate(
        cancelled = is.na(dep_time),
        sched_hour = sched_dep_time %/% 100,
        sched_min = sched_dep_time %% 100,
        sched_dep_time = sched_hour + sched_min / 60
    )
ggplot(cancellation) + 
    geom_boxplot(aes(x = cancelled, y=sched_dep_time)) + 
    coord_flip()

ggplot(cancellation) +
    geom_boxplot(aes(x=cancelled, y=sched_dep_time))

ggplot(diamonds,aes(x=cut,y=price))+
    geom_boxplot()+coord_flip()

library(lvplot)
ggplot(diamonds, aes(x=cut, y=price)) + 
    geom_lv(aes(fill=..LV..)) + coord_flip()
?geom_lv

## two categorical variables 
ggplot(diamonds) +
    geom_count(aes(x = cut, y = color))

diamonds %>% 
    count(color, cut) %>%
    ggplot(aes(x=color, y=cut)) +
    geom_tile(aes(fill=n))


library(viridis)


diamonds %>% 
    count(color, cut) %>%
    group_by(color) %>%
    mutate(prop = n / sum(n)) %>%
    ggplot(mapping = aes(x = color, y = cut)) +
    geom_tile(mapping = aes(fill = prop)) +
    scale_fill_viridis(limits = c(0, 1))


nycflights13::flights %>%
    group_by(month,dest) %>%
    summarize(dep_delay = mean(dep_delay, na.rm = TRUE)) %>%
    ggplot(aes(x=factor(month),y=dest,fill=dep_delay))+geom_tile()+
    labs(x = "Month", y = "Destination", fill = "Departure Delay")


require(forcats)
nycflights13::flights %>%
    group_by(month,dest) %>%
    summarize(dep_delay = mean(dep_delay, na.rm = TRUE)) %>%
    group_by(dest) %>%
    filter(n() == 12) %>%
    ungroup() %>%
    mutate(dest = fct_reorder(dest, dep_delay)) %>%
    ggplot(aes(x=factor(month),y=dest,fill=dep_delay))+geom_tile()+
    labs(x = "Month", y = "Destination", fill = "Departure Delay")+
    scale_fill_viridis() 

### two continuous variables 
ggplot(diamonds) + 
    geom_bin2d(aes(x=carat, y=price))

library(hexbin)
ggplot(diamonds) + 
    geom_hex(aes(x=carat, y=price))

ggplot(diamonds, aes(x = carat, y=price)) +
    geom_boxplot(aes(group = cut_width(carat, 0.1)))

ggplot(diamonds2, aes(x=carat, y=price)) +
    geom_boxplot(aes(group = cut_number(carat, 20)))


ggplot(data = diamonds, 
       mapping = aes(x = price,
                     colour = cut_width(carat, 0.3))) +
    geom_freqpoly()


#cut width
ggplot(data = diamonds, aes(x = price, y = ..density.., 
                                      colour = cut_width(carat, 0.3))) +
    geom_freqpoly()

#cut number 
ggplot(data = diamonds, 
       mapping = aes(x = price,
                     colour = cut_number(carat, 10))) +
    geom_freqpoly()

ggplot(data = diamonds, 
       mapping = aes(x = price,
                     y = ..density..,
                     colour = cut_number(carat, 10))) +
    geom_freqpoly()

ggplot(diamonds, aes(x = cut_number(price, 10), y = carat)) +
    geom_boxplot() +
    coord_flip() +
    xlab("Price")
ggplot(diamonds, aes(x = cut_number(carat, 10), y = price)) +
    geom_boxplot() +
    coord_flip() +
    xlab("Carat")

ggplot(diamonds, aes(x = cut_number(carat, 5), y = price, color = cut)) +
    geom_boxplot()


ggplot(diamonds, aes(color = cut_number(carat, 5), y = price, x = cut)) +
    geom_boxplot()












