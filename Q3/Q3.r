# Load required library
library(dplyr)

# Import relevant tables from ../data/
person      <- read.csv("../data/person.csv")                   # Seatbelt/helmet, hospitalisation, demographics
accident    <- read.csv("../data/accident.csv")                 # Severity, time, speed limit, light conditions
vehicle     <- read.csv("../data/vehicle.csv")                  # Vehicle type/body; links to whether belts/helmets apply
event       <- read.csv("../data/accident_Event.csv")           # Collision details (impact type, object hit)
weather     <- read.csv("../data/atmospheric_cond.csv")   # Weather confounder
road_surface<- read.csv("../data/road_surface_cond.csv")  # Road surface confounder
node        <- read.csv("../data/node.csv")                     # Spatial context (LGA, postcode, lat/long)
location    <- read.csv("../data/accident_Location.csv")        # Road type/context

# Quick sanity checks
str(person)
table(person$hospitalisation)
table(person$seatbelt_worn, useNA = "ifany")
table(person$helmet_worn, useNA = "ifany")
