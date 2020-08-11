library(mosaic)
library(tidyverse)
library(ggplot2)


# Question One

gb <- read.csv("../STA380/data/greenbuildings.csv")
View(gb)
head(gb)

gb$green_rating = as.factor(gb$green_rating)
  
#### Cluster Price range from low 10s to high 60s.
ggplot(data = gb) + 
  geom_point(mapping = aes(x = cluster_rent, y = Rent, color = green_rating)) +
  labs(title = "Rent per Square of Green Building and NonGreen Building")


#### Is all green building having a higher than market rental price? shown in a percentage graph
d1 = gb %>%
  group_by(green_rating ) %>%
  summarize(good_performance = sum(Rent > cluster_rent)/n())

ggplot(data = d1) + 
  geom_bar(mapping = aes(x = green_rating, y = good_performance ), stat='identity') +
  labs(title = "BUildings with Higher Rent than local market Average Rent")

##### proportionally, green buildings usually have more rent per square-foot per calendar year than average rent in local market

#### reproduce the analysis of the excel guru, find out the $ premium in ecobuilding
gb_filtered = filter(gb, leasing_rate>.1)

gb_filtered %>%
  group_by(green_rating) %>%
  summarize(Rent.med = median(Rent))

###### So far the results support the EXCEL guru's analysis, that green buildings have a higher rental price comparatively. 
###### Green Buildings have a 27.6 vs Nongreen Buildings have a 25.

### but we want to know how other factors play in determining rent


### could it be NonGreen building are mostly older buildings?
ggplot(data = gb) + 
  geom_point(mapping = aes(x = age, y = Rent, color = green_rating))+
  labs(title = "Building Rent differed by Age of Building")
###### The green building did not show a pattern in higher rent for the building that have the similar building age

### could it be NonGreen building renovated more?
gb %>%
  group_by(green_rating) %>%
  summarize(renovated.count = count(renovated > 0))

gb_filtered = filter(gb, renovated>0)

ggplot(data = gb_filtered) + 
  geom_point(mapping = aes(x = Rent, y = cluster_rent, color = green_rating),stat='identity') +
  labs(title = "Renovated Building in Local Market")
###### The green buildings generally have a higher return than the nonGreen building in the same local market rental rate


#### If we estimated the return of Green and NonGreen building of similar conditions
hist(gb$stories)
hist(gb$age)

gb_filtered = filter(gb, 20 > stories, stories > 10, age < 25, age > 10)

ggplot(data = gb_filtered) + 
  geom_point(mapping = aes(x = cluster_rent, y = Rent, color = green_rating)) +
  labs(title = "Rent per Square of Green Building and NonGreen Building in Similar Condition")
gb_filtered %>%
  group_by(green_rating) %>%
  summarize(Rent.med = median(Rent))
# There is a small green building premium but it is not as high as the EXCEL Guru estimated
