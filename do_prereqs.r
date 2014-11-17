#####################################################################################################
print("entered do_prereqs.r")

print("loading prereq packages")
require(reshape2)
require(plyr)
require(stringr)
require(ggplot2)
require(maptools)
require(grid)

print("reading pop data")
example_pop <- read.csv("data/working_age_people_1996.csv")
print("reading house data")
example_house <- read.csv("data/council_houses_2011.csv")

print("reading shapefiles")
datazones_shp <- readShapeSpatial(
  "shapefiles/scotland_2001_datazones/scotland_dz_2001.shp"
)

print("Making proportions")
example_pop <- transform(example_pop, proportion=workingage_count/total_count)
example_house <- transform(example_house, proportion=councilhouse_count/total_count)

print("fortifying shapefiles")
datazones_shp@data$id <- rownames(datazones_shp@data)
id_name <- subset(datazones_shp@data, select=c("id", "zonecode"))

datazones_map <- fortify(datazones_shp)
datazones_map <- join(datazones_map, id_name)
datazones_map <- rename(datazones_map, replace=c("zonecode"="datazone"))


theme_clean <- function(base_size=12){
  theme_grey(base_size) %+replace%
    theme(
      axis.title=element_blank(),
      axis.text=element_blank(),
      panel.background=element_blank(),
      panel.grid=element_blank(),
      axis.ticks.length=unit(0, "cm"),
      axis.ticks.margin=unit(0, "cm"),
      panel.margin=unit(0, "lines"),
      plot.margin=unit(c(0,0,0,0), "lines"),
      complete=TRUE
    )
}

print("connecting pop to dzs")
pop_joined <- join(datazones_map, example_pop, by="datazone", type="full")
pop_joined <- arrange(pop_joined, group, order)

print("connecting house to dzs")
house_joined <- join(datazones_map, example_house, by="datazone", type="full")
house_joined <- arrange(house_joined, group, order)

