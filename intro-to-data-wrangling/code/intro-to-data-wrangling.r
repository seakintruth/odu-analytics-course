storms = read.csv("datasets/storms.csv", header=T)

cases = read.csv("datasets/cases.csv", header=T, check.names = F) # add check.names = F here

pollution = read.csv("datasets/pollution.csv", header=T)

ratio = storms$pressure/storms$wind

library(tidyr)
gather(cases, "year", "n", 2:4)
pollution
spread(pollution, size, amount)

storms2 = separate(storms, date, c("year", "month", "day"), sep="-")
unite(storms2, "date", year, month, day, sep="-")

library(dplyr)
select(storms, storm, pressure)
select(storms, -storm)
select(storms, wind:date)

filter(storms, wind >= 50)
filter(storms, wind >= 50, storm %in% c("Alberto", "Alex", "Allison"))

mutate(storms, ratio = pressure / wind)
mutate(storms, ratio = pressure / wind, inverse = ratio^-1)

summarise(pollution, median = median(amount), variance = var(amount))
summarise(pollution, mean = mean(amount), sum = sum(amount), n=n())

arrange(storms, wind)
arrange(storms, desc(wind))
arrange(storms, wind, date)



select(storms, storm, pressure)
storms %>% select(storm, pressure)


filter(storms, wind >= 50)
storms %>% filter(wind >= 50)
storms %>% filter(wind >= 50) %>% select(storm, pressure)
storms %>% mutate(ratio = pressure / wind) %>% select(storm, ratio)

pollution %>% group_by(city)
pollution %>% group_by(city) %>% summarise(mean=mean(amount), sum=sum(amount), n=n())
pollution %>% group_by(city) %>% summarise(mean=mean(amount))

tb = read.csv("datasets/tb.csv", header=T)
View(tb)
tb %>% group_by(country, year)
tb %>% group_by(country, year) %>% summarise(cases = sum(cases)) 
tb %>% group_by(country, year) %>% summarise(cases = sum(cases)) %>% summarise(cases = sum(cases))

y = read.csv("datasets/y.csv", header=T)
z = read.csv("datasets/z.csv", header=T)
bind_cols(y, z)
bind_rows(y, z)
union(y, z)
intersect(y, z)
setdiff(y, z)

songs = read.csv("datasets/songs.csv", header=T)
artists = read.csv("datasets/artists.csv", header=T)
left_join(songs, artists, by = "name")
inner_join(songs, artists, by = "name")
semi_join(songs, artists, by = "name")
anti_join(songs, artists, by = "name")
