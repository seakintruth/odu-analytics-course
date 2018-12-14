library(tidyverse)

# Constants.
raw_filename <- "datasets/infant_hiv.csv"

#Get the data and look at it
raw <- read_csv(raw_filename)
View(raw)

# Get the data, and skip the first two lines. Then look at it
raw <- read_csv(raw_filename, skip = 2)
View(raw)

# Get the data, skip the first two lines and substitute NA for any "-"
raw <- read_csv(raw_filename, skip = 2, na = c("-"))
View(raw)

# notice that the final 22 rows are not part of the data set
num_rows <- nrow(raw)-22

# pull out of the relevant rows from the data set and assign them to sliced
sliced <- raw %>% slice(1:num_rows)

# isolate only the percent columns and assign it to percents
percents <- sliced %>% select(-c(ISO3, Countries)) 

# remove the percent sign and update percents. We use map_dfr to apply it to all dataframe rows.
percents <- percents %>% map_dfr(str_replace, pattern = "%", replacement = "")

# remove the > sign and update percents. We use map_dfr to apply it to all dataframe rows.
percents <- percents %>% map_dfr(str_replace, pattern = ">", replacement = "")

# since they are percents they need to be numeric
percents <-  percents %>% map_dfr(function(col) as.numeric(col))

# since they are percents they need to be divided by 100
percents <-  percents %>% map_dfr(function(col) col/100)

# now let's give better column names to percents
names(percents) <- c("2009-Estimate", "2009-UpperBound", "2009-LowerBound", "2010-Estimate", "2010-UpperBound", "2010-LowerBound")

# now let's work on the country labels (ISO3 Code and Countries Col)
labels <- sliced %>% select(ISO3, Countries)
View(labels)

# ugh, there is a blank ISO3 code corresponding to Kosovo
# https://en.wikipedia.org/wiki/Kosovo
#  According to Wikipedia, UNK is used for Kosovo residents whose travel documents 
# were issued by the United Nations, so we will fill that
labels <- labels %>% mutate(ISO3=replace(ISO3, Countries == "Kosovo", "UNK"))

# okay now we can combine our two tbls labels and percents
comboData <- cbind(labels, percents)

# next we want to use our tidyr to get it into a format where the measurement type and year are data points as opposed to headers
comboData <- comboData %>% gather("YearAndMeasureType", "Value", 3:8)

# closer the only thing left to do is separater out YearAndType into two columns
comboData <- comboData %>% separate(YearAndMeasureType, c("Year", "MeasurementType"), sep="-")

# write it out to disk.
write.csv(comboData, file="datasets/tidy_infant_hiv.csv", row.names=F)
