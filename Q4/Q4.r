# Load required library
library(dplyr)

# Import relevant tables from ../data/
accident      <- read.csv("../data/accident.csv")                 # Severity, geometry, speed limit, light, time
road_surface  <- read.csv("../data/road_surface_cond.csv")  # Road surface state/sequence
weather       <- read.csv("../data/atmospheric_cond.csv")   # Weather at time of crash
location      <- read.csv("../data/accident_Location.csv")        # Road type/context
node          <- read.csv("../data/node.csv")                     # Spatial context (lat/long, LGA, postcode)
vehicle       <- read.csv("../data/vehicle.csv")                  # Vehicle attributes (body type, movement, intent)
person        <- read.csv("../data/person.csv")                   # Driver/passenger demographics, restraint/helmet
dca           <- read.csv("../data/sub_dca.csv")      # DCA codes/descriptions (mechanism)

# Quick sanity checks
str(accident)
table(accident$severity, useNA = "ifany")
table(accident$light_condition, useNA = "ifany")