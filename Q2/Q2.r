# Load required library
library(dplyr)

# Import relevant tables from ../data/
accident <- read.csv("../data/accident.csv")              # Core table: time, date, light_condition, severity
person <- read.csv("../data/person.csv")                  # Injury level, hospitalisation, demographics
vehicle <- read.csv("../data/vehicle.csv")                # Vehicle type, body, movement
weather <- read.csv("../data/atmospheric_cond.csv") # Weather confounder
road_surface <- read.csv("../data/road_surface_cond.csv") # Road surface confounder
location <- read.csv("../data/accident_Location.csv")     # Road type, context
node <- read.csv("../data/node.csv")                      # Spatial context (urban vs rural)

# Quick checks
str(accident)                     # Inspect structure of Accident table
table(accident$light_condition)   # Frequency of lighting categories
table(accident$severity)          # Frequency of severity categories
