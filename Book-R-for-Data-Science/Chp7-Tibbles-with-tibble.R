library(tidyverse)
library(nycflights13)

# print 
# can set the print of the output, width = Inf will print out all columns 
nycflights13::flights %>% print(n = 10, width = Inf)

options(tibble.print_max = n, tibble.print_min = m) #if more than m rows, print only n rows 
options(dplyr.print_min = Inf) # to always show all rows 
options(tibble.width = Inf) # to always print all collumns, regardless of the width of the screen 

# subsetting 
as.tibble(mtcars)
df <- data.frame(abc = 1, xyz = "a")
df$x
df[, "xyz"]
df[, c("abc", "xyz")]
tbl <- as.tibble(df)
tbl %>% .[["xyz"]]
tbl[,c("abc","xyz")]
