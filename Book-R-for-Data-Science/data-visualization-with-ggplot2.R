#load library
library(tidyverse)
library(maps)

#Q1: do cars with big engines use more fuel than cars with small engines?
##mpg dataframe
ggplot2::mpg
##mpg variable description
?mpg

##creating a ggplot 
ggplot(data = mpg) + geom_point(mapping = aes(x=displ, y=hwy))

## scatterplot of hwy versus cyl 
ggplot(data = mpg) + geom_point(mapping = aes(x=cyl, y=hwy))

#Aesthetic Mappings
##mapping color to dataset 
ggplot(data = mpg) + geom_point(mapping = aes(x=displ, y=hwy, color=class))
ggplot(data = mpg) + geom_point(mapping = aes(x=displ, y=hwy, color=displ<5))
ggplot(data = mpg) + geom_point(mapping = aes(x=displ, y=hwy), color='blue') #set color manually, should take the color ourside the aes()

## mapping size to dataset 
ggplot(data = mpg) + geom_point(mapping = aes(x=displ, y=hwy, size=class)) #we get a warning here, because mapping an unordered variable(class) to an ordered aesthetic (size) is not a good idea

## mapping alpha (transparency) to dataset 
ggplot(data = mpg) + geom_point(mapping = aes(x=displ, y=hwy, alpha=class))

## mapping shape of points to dataset 
ggplot(data = mpg) + geom_point(mapping = aes(x=displ, y=hwy, shape=class)) #suv legend is missed because ggplot only use six shapes at a time. By default, additional groups will go unplotted when you use this aesthetic
ggplot(data = mpg) + geom_point(mapping = aes(x=displ, y=hwy), shape=23, fill='red', size=2)
?geom_point

# Facets 
##one variable
ggplot(data = mpg) + 
    geom_point(mapping = aes(x=displ, y=hwy)) +
    facet_wrap(~class, nrow=2)
##combination of two variable
ggplot(data = mpg) + 
    geom_point(mapping = aes(x=displ, y=hwy)) + 
    facet_grid(drv~cyl)
ggplot(data = mpg) + 
    geom_point(mapping = aes(x=displ, y=hwy)) + 
    facet_grid(. ~cyl)
ggplot(data = mpg) + 
    geom_point(mapping = aes(x=displ, y=hwy)) + 
    facet_wrap(~cyl)

?facet_wrap

#Geometric Object 
ggplot(data=mpg) +
    geom_smooth(aes(x=displ, y=hwy))
ggplot(data = mpg) +
    geom_smooth(aes(x=displ, y=hwy, linetype=drv, color=drv), show.legend = FALSE)

##add multiple geoms 
ggplot(data = mpg, aes(x=displ, y=hwy, color=drv)) +
    geom_point() +
    geom_smooth()
ggplot(data = mpg, aes(x=displ, y=hwy)) +
    geom_point(aes(color=drv)) +
    geom_smooth()
ggplot(data = mpg, aes(x=displ, y=hwy, color=drv)) +
    geom_point(aes(color=drv)) +
    geom_smooth()


ggplot(data = mpg, aes(x=displ, y=hwy, color=drv)) +
    geom_point() +
    geom_smooth(se = FALSE)

ggplot(data = mpg, aes(x=displ, y=hwy, color=drv)) +
    geom_point() +
    geom_smooth()
?geom_smooth


#Statistical Transformations - Bar Chart 
ggplot(data = diamonds) +
    geom_bar(aes(x=cut))

ggplot(data=diamonds) + 
    geom_bar(aes(x=cut, y=..prop.., group=1))
  
ggplot(data=diamonds) + 
 stat_summary(
    aes(x=cut, y=depth),
    fun.ymin = min,
    fun.ymax = max,
    fun.y = mean
)

ggplot(data=diamonds) + 
    geom_col(aes(x=cut, y=depth))

## Position Adjustments 
ggplot(data = diamonds) + 
    geom_bar(mapping = aes(x=cut, color = cut))
ggplot(data = diamonds) + 
    geom_bar(mapping = aes(x=cut, fill = cut))

ggplot(data = diamonds) + 
    geom_bar(mapping = aes(x=cut, fill = clarity))

### if don't want to stack bar chart, we can use one of three other options: "identity","dodge" or "fill"
#### position = "identity" 
ggplot(data=diamonds, aes(x=cut, fill=clarity)) +
    geom_bar(alpha = 1/5, position = "identity")

ggplot(data = diamonds, aes(x=cut, color=clarity)) +
    geom_bar(fill=NA, position = "identity")

#### position = "fill"
ggplot(data=diamonds) +
    geom_bar(aes(x=cut, fill=clarity),
             position = "fill")

#### position = "dodge"
ggplot(data=diamonds) + 
    geom_bar(aes(x=cut, fill=clarity), 
             position = "dodge") 

#### position = "jitter" add a small amoiunt of random noise to each point to solve the overlapping problem 
ggplot(data=mpg) +
    geom_point(aes(x=displ, y=hwy))
ggplot(data=mpg) +
    geom_point(aes(x=displ, y=hwy), position = "jitter")

## Coordinate Systems 
### coord_flip()
ggplot(data=mpg, aes(x=class, y=hwy)) + 
    geom_boxplot()

ggplot(data=mpg, aes(x=class, y=hwy)) + 
    geom_boxplot() +
    coord_flip()
### coord_quickmap() set aspect ratio correctly for maps 
nz <- map_data("nz")
ggplot(nz, aes(long, lat, group=group)) + 
    geom_polygon(fill="white", color="black")

ggplot(nz, aes(long, lat, group=group)) +
    geom_polygon(fill="white", color="black") +
    coord_quickmap()

### coord_polar() uses polar coordinates 
bar <- ggplot(data=diamonds) +
    geom_bar(aes(x=cut, fill=cut), show.legend = FALSE,  width = 1) +
    theme(aspect.ratio = 1)  + 
    labs(x=NULL, y=NULL)
bar + coord_flip()
bar + coord_polar()


#### Exercise 
ggplot(data = diamonds) + 
    geom_bar(mapping = aes(x = cut, fill = clarity), width = 1)
ggplot(data = diamonds) + 
    geom_bar(mapping = aes(x = cut, fill = clarity), width=1)+coord_polar(theta = "y")
?labs
p <- ggplot(mtcars, aes(mpg, wt, colour = cyl)) + geom_point()
p + labs(colour = "Cylinders")

ggplot(data = mpg, mapping = aes(x = cty, y = hwy)) +
    geom_point() + 
    geom_abline()
