# inspect data frame and tibble
Teams
as.tibble(Teams)

# subsetting a data frame sometimes generates vectors
class(Teams[,20])

# subsetting a tibble always generates tibbles
class(as.tibble(Teams[,20]))

# pulling a vector out of a tibble
class(as.tibble(Teams)$HR)

# access a non-existing column in a data frame or a tibble
Teams$hr
as.tibble(Teams)$hr

# create a tibble with comples objects
tibble(id = c(1,2,3), func = c(mean, median, sd))