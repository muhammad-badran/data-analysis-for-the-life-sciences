install.packages("dplyr")

data <- read.csv('femaleMiceWeights.csv')

View(data)

library(dplyr)

# using filter command
controls <- filter(data, Diet=="chow")

# using select command
controls <- select(controls, Bodyweight)

# creating a vector
unlist(controls)

# pipe
controls <- filter(data, Diet=="chow") %>% select(Bodyweight) %>% unlist
