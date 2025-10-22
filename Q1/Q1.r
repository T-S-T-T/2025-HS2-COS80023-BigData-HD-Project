# Load required library
library(dplyr)

# Import relevant tables from ../data/
accident <- read.csv("../data/accident.csv")
location <- read.csv("../data/accident_Location.csv")
road_surface <- read.csv("../data/road_surface_cond.csv")
weather <- read.csv("../data/atmospheric_cond.csv")
person <- read.csv("../data/person.csv")
node <- read.csv("../data/node.csv")

# Quick checks
str(accident)       # Inspect structure of Accident table
table(accident$road_geometry)   # Frequency of road geometry categories
table(accident$severity)        # Frequency of severity categories
