library(tidyverse)

#read 
read_csv() #reads comma-delimited files
read_csv2() #reads semicolon-delimited files
read_tsv() #reads tab-delimited fiels 
read_delim() #reads in files with any delimiter
read_fwf() #reads fixed-width files, can specify fields by fwf_widths() or fwf_positions
read_table() #reads files where columns are separated by white space
read_log() # reads Apache style log files , webreadr is built on top of read_log()

#read_csv()
read_csv("The first line of metadata
         The second line of metadata
         x,y,z
         1,2,3", skip = 2)

read_csv("# A comment I want to skip
         x,y,z
         1,2,3", comment = "#")
read_csv("1,2,3\n4,5,6", col_names = FALSE)
read_csv("a,b,c\n1,2,.", na= ".")


x <- "x,y\n1,'a,b'"
read_delim(x, ",", quote = "'")

guess_parser("15:01")

library(readr)
challenge <- read_csv(
    readr_example("challenge.csv"),
    col_types = cols(
        x = col_integer(),
        y = col_character()
    )
)
# then can tweak the type of the x column 
challenge <- read_csv(
    readr_example("challenge.csv"),
    col_types = cols(
        x = col_double(),
        y = col_character()
    )
)
tail(challenge)
# then can fix the y is a date time 
challenge <- read_csv(
    readr_example("challenge.csv"),
    col_types = cols(
        x = col_double(),
        y = col_date()
    )
)
tail(challenge)


df <- tribble(
    ~x, ~y, 
    "1", "1.21",
    "2","2.32"
)
df
type_convert(df)

## write to a file 
#write_csv(challenge, "challenge.csv")

#write_rds(challenge,"challenge.rds")
#read_rds("challenge.rds")

#library(feather)
#write_feather(challenge, "challenge.feather")
#read_feather("chellenge.feather")
