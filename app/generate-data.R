library(dplyr)
library(lubridate)
library(stringr)

set.seed(123)

state_county_coords <- list(
  # Midwest (Heavy concentration)
  "IA" = list(
    counties = c("Polk County", "Linn County", "Johnson County", "Black Hawk County", "Woodbury County", "Dubuque County", "Story County", "Warren County", "Madison County", "Jasper County"),
    lat_range = c(40.375, 43.501), lon_range = c(-96.639, -90.140), region = "Midwest"
  ),
  "IL" = list(
    counties = c("Cook County", "DuPage County", "Lake County", "Will County", "Kane County", "McHenry County", "Madison County", "St. Clair County", "Winnebago County", "Sangamon County"),
    lat_range = c(36.970, 42.508), lon_range = c(-91.513, -87.020), region = "Midwest"
  ),
  "IN" = list(
    counties = c("Marion County", "Lake County", "Allen County", "Hamilton County", "St. Joseph County", "Vanderburgh County", "Tippecanoe County", "Porter County", "Elkhart County", "Johnson County"),
    lat_range = c(37.771, 41.761), lon_range = c(-88.098, -84.784), region = "Midwest"
  ),
  "NE" = list(
    counties = c("Douglas County", "Lancaster County", "Sarpy County", "Hall County", "Buffalo County", "Washington County", "Dodge County", "Madison County", "Platte County", "Scotts Bluff County"),
    lat_range = c(40.001, 43.001), lon_range = c(-104.053, -95.308), region = "Great Plains"
  ),
  "KS" = list(
    counties = c("Johnson County", "Sedgwick County", "Shawnee County", "Wyandotte County", "Saline County", "Butler County", "Reno County", "Harvey County", "Douglas County", "Leavenworth County"),
    lat_range = c(36.993, 40.003), lon_range = c(-102.052, -94.589), region = "Great Plains"
  ),
  "MN" = list(
    counties = c("Hennepin County", "Ramsey County", "Dakota County", "Anoka County", "Washington County", "Wright County", "Stearns County", "Olmsted County", "St. Louis County", "Carver County"),
    lat_range = c(43.499, 49.384), lon_range = c(-97.239, -89.483), region = "Midwest"
  ),
  "ND" = list(
    counties = c("Cass County", "Burleigh County", "Grand Forks County", "Ward County", "Williams County", "Stark County", "Morton County", "Stutsman County", "Walsh County", "Barnes County"),
    lat_range = c(45.935, 49.000), lon_range = c(-104.049, -96.554), region = "Great Plains"
  ),
  "SD" = list(
    counties = c("Minnehaha County", "Pennington County", "Lincoln County", "Brown County", "Codington County", "Brookings County", "Lawrence County", "Yankton County", "Meade County", "Hughes County"),
    lat_range = c(42.479, 45.945), lon_range = c(-104.058, -96.436), region = "Great Plains"
  ),
  "OH" = list(
    counties = c("Cuyahoga County", "Franklin County", "Hamilton County", "Montgomery County", "Summit County", "Lucas County", "Stark County", "Butler County", "Lorain County", "Mahoning County"),
    lat_range = c(38.403, 41.977), lon_range = c(-84.820, -80.519), region = "Midwest"
  ),
  "MO" = list(
    counties = c("St. Louis County", "Jackson County", "St. Charles County", "Jefferson County", "Clay County", "Greene County", "Boone County", "Platte County", "Cass County", "Franklin County"),
    lat_range = c(35.995, 40.613), lon_range = c(-95.774, -89.099), region = "Midwest"
  ),
  "WI" = list(
    counties = c("Milwaukee County", "Dane County", "Waukesha County", "Brown County", "Racine County", "Outagamie County", "Winnebago County", "Washington County", "Rock County", "Kenosha County"),
    lat_range = c(42.491, 47.308), lon_range = c(-92.889, -86.805), region = "Midwest"
  ),
  "MI" = list(
    counties = c("Wayne County", "Oakland County", "Macomb County", "Kent County", "Genesee County", "Washtenaw County", "Ottawa County", "Ingham County", "Kalamazoo County", "Saginaw County"),
    lat_range = c(41.696, 48.306), lon_range = c(-90.418, -82.413), region = "Midwest"
  ),
  
  # Southeast (Moderate concentration)
  "TX" = list(
    counties = c("Harris County", "Dallas County", "Tarrant County", "Bexar County", "Travis County", "Collin County", "Hidalgo County", "Fort Bend County", "El Paso County", "Denton County"),
    lat_range = c(25.837, 36.501), lon_range = c(-106.646, -93.508), region = "Southeast"
  ),
  "FL" = list(
    counties = c("Miami-Dade County", "Broward County", "Palm Beach County", "Hillsborough County", "Orange County", "Pinellas County", "Duval County", "Lee County", "Polk County", "Brevard County"),
    lat_range = c(24.396, 31.000), lon_range = c(-87.635, -79.974), region = "Southeast"
  ),
  "GA" = list(
    counties = c("Fulton County", "Gwinnett County", "DeKalb County", "Cobb County", "Clayton County", "Cherokee County", "Henry County", "Forsyth County", "Hall County", "Houston County"),
    lat_range = c(30.356, 35.000), lon_range = c(-85.605, -80.751), region = "Southeast"
  ),
  "NC" = list(
    counties = c("Mecklenburg County", "Wake County", "Guilford County", "Forsyth County", "Cumberland County", "Durham County", "Union County", "Gaston County", "New Hanover County", "Cabarrus County"),
    lat_range = c(33.842, 36.588), lon_range = c(-84.322, -75.460), region = "Southeast"
  ),
  "VA" = list(
    counties = c("Fairfax County", "Virginia Beach City", "Norfolk City", "Chesapeake City", "Richmond City", "Newport News City", "Alexandria City", "Hampton City", "Portsmouth City", "Suffolk City"),
    lat_range = c(36.540, 39.466), lon_range = c(-83.675, -75.166), region = "Southeast"
  ),
  
  # West (Sparse)
  "CA" = list(
    counties = c("Los Angeles County", "San Diego County", "Orange County", "Riverside County", "San Bernardino County", "Santa Clara County", "Alameda County", "Sacramento County", "Contra Costa County", "Fresno County"),
    lat_range = c(32.534, 42.009), lon_range = c(-124.482, -114.131), region = "West"
  ),
  "CO" = list(
    counties = c("Denver County", "El Paso County", "Jefferson County", "Arapahoe County", "Adams County", "Boulder County", "Larimer County", "Douglas County", "Pueblo County", "Mesa County"),
    lat_range = c(36.993, 41.003), lon_range = c(-109.060, -102.042), region = "West"
  ),
  "WA" = list(
    counties = c("King County", "Pierce County", "Snohomish County", "Spokane County", "Clark County", "Thurston County", "Kitsap County", "Whatcom County", "Yakima County", "Skagit County"),
    lat_range = c(45.544, 49.002), lon_range = c(-124.848, -116.916), region = "West"
  ),
  "OR" = list(
    counties = c("Multnomah County", "Washington County", "Clackamas County", "Lane County", "Marion County", "Jackson County", "Deschutes County", "Linn County", "Yamhill County", "Douglas County"),
    lat_range = c(41.992, 46.299), lon_range = c(-124.703, -116.463), region = "West"
  ),
  
  # Northeast (Sparse)
  "NY" = list(
    counties = c("New York County", "Kings County", "Queens County", "Bronx County", "Richmond County", "Nassau County", "Suffolk County", "Westchester County", "Erie County", "Monroe County"),
    lat_range = c(40.477, 45.016), lon_range = c(-79.762, -71.777), region = "Northeast"
  ),
  "PA" = list(
    counties = c("Philadelphia County", "Allegheny County", "Montgomery County", "Bucks County", "Chester County", "Delaware County", "Lancaster County", "York County", "Berks County", "Westmoreland County"),
    lat_range = c(39.719, 42.516), lon_range = c(-80.519, -74.689), region = "Northeast"
  ),
  "NJ" = list(
    counties = c("Bergen County", "Middlesex County", "Essex County", "Hudson County", "Monmouth County", "Ocean County", "Union County", "Passaic County", "Morris County", "Camden County"),
    lat_range = c(38.928, 41.357), lon_range = c(-75.560, -73.894), region = "Northeast"
  )
)

generate_realistic_locations <- function(n) {
  states <- names(state_county_coords)
  
  # Create regional weights - heavy Midwest/Great Plains, sparse elsewhere
  regional_weights <- sapply(states, function(s) {
    region <- state_county_coords[[s]]$region
    switch(region,
      "Midwest" = 35,       
      "Great Plains" = 25,  
      "Southeast" = 8,      
      "West" = 4,           
      "Northeast" = 4       
    )
  })
  
  # Sample states based on weights
  selected_states <- sample(states, n, replace = TRUE, prob = regional_weights)
  
  locations <- data.frame(
    state = character(n),
    county = character(n),
    latitude = numeric(n),
    longitude = numeric(n),
    region = character(n),
    stringsAsFactors = FALSE
  )
  
  for (i in 1:n) {
    state <- selected_states[i]
    state_info <- state_county_coords[[state]]
    
    # Generate coordinates with Gulf of Mexico filter
    repeat {
      lat <- runif(1, state_info$lat_range[1], state_info$lat_range[2])
      lon <- runif(1, state_info$lon_range[1], state_info$lon_range[2])
      
      # Exclude Gulf of Mexico (rough approximation)
      # Gulf is roughly: lat < 30 AND lon > -97 AND lon < -82
      if (!(lat < 30 && lon > -97 && lon < -82)) {
        break
      }
    }
    
    locations$state[i] <- state
    locations$county[i] <- sample(state_info$counties, 1)
    locations$latitude[i] <- round(lat, 4)
    locations$longitude[i] <- round(lon, 4)
    locations$region[i] <- state_info$region
  }
  
  return(locations)
}

# Create realistic equipment data with logical relationships
create_realistic_equipment_data <- function(locations) {
  n <- nrow(locations)
  
  # Base equipment info
  equipment_id <- paste0("JD", sprintf("%05d", 1:n))
  
  # Expanded model list with older equipment
  all_models <- c(
    # Current models (2018+)
    "9620RX Tractor", "S780 Combine", "8R 410 Tractor", 
    "X9 1100 Combine", "6155R Tractor", "S790 Combine",
    "8R 370 Tractor", "S770 Combine",
    
    # Older models (2010-2017)
    "8320R Tractor", "8370R Tractor", "S680 Combine", "S670 Combine",
    "7250R Tractor", "7280R Tractor", "7230R Tractor",
    "9560R Tractor", "9510R Tractor", "9460R Tractor",
    
    # Legacy models (2005-2015)
    "8430 Tractor", "8330 Tractor", "8230 Tractor", "8130 Tractor",
    "9630 Tractor", "9530 Tractor", "9430 Tractor",
    "S660 Combine", "S650 Combine", "9600 Combine", "9610 Combine",
    "7820 Tractor", "7720 Tractor", "7620 Tractor",
    
    # Vintage models (2000-2010)
    "8420 Tractor", "8320 Tractor", "8220 Tractor", "8120 Tractor",
    "9520 Tractor", "9420 Tractor", "9320 Tractor",
    "9660 STS Combine", "9560 STS Combine", "9460 STS Combine",
    "7810 Tractor", "7710 Tractor", "7610 Tractor"
  )
  
  # Weight newer models more heavily
  model_weights <- c(
    rep(15, 8),   # Current models (high weight)
    rep(10, 9),   # 2010-2017 models (medium-high weight)
    rep(6, 14),   # 2005-2015 models (medium weight)
    rep(3, 14)    # 2000-2010 models (low weight) - adjusted to 14
  )
  
  models <- sample(all_models, n, replace = TRUE, prob = model_weights)
  
  # Adjust year ranges based on model era
  years <- sapply(models, function(model) {
    if (model %in% all_models[1:8]) {
      sample(2018:2024, 1)  # Current models
    } else if (model %in% all_models[9:17]) {
      sample(2012:2020, 1)  # Older models
    } else if (model %in% all_models[18:30]) {
      sample(2005:2018, 1)  # Legacy models
    } else {
      sample(2000:2012, 1)  # Vintage models
    }
  })
  
  # Current date for seasonal logic
  current_date <- as.Date("2024-12-15")
  
  # Seasonal logic based on current date (December = Off-Season/Harvest end)
  season_probs <- list(
    "Off-Season" = 0.6,
    "Harvest" = 0.3, 
    "Planting" = 0.05,
    "Growing" = 0.05
  )
  seasons <- sample(names(season_probs), n, replace = TRUE, prob = unlist(season_probs))
  
  # Temperature based on region and season
  temp_base <- ifelse(locations$region %in% c("West", "Southeast"), 50, 35)
  temp_seasonal <- ifelse(seasons == "Off-Season", -10, 
                         ifelse(seasons == "Harvest", 10, 20))
  temperatures <- pmax(20, pmin(85, temp_base + temp_seasonal + rnorm(n, 0, 8)))
  
  # Field conditions based on temperature and season
  field_conditions <- ifelse(temperatures < 32, "Frozen",
                            ifelse(seasons == "Off-Season", 
                                   sample(c("Dry", "Wet"), n, replace = TRUE, prob = c(0.7, 0.3)),
                                   sample(c("Dry", "Wet", "Muddy"), n, replace = TRUE, prob = c(0.5, 0.3, 0.2))))
  
  # Status logic: more equipment idle in off-season
  status_probs <- if(mean(seasons == "Off-Season") > 0.5) {
    c("Active" = 0.4, "Maintenance" = 0.3, "Repair" = 0.1, "Idle" = 0.2)
  } else {
    c("Active" = 0.7, "Maintenance" = 0.15, "Repair" = 0.1, "Idle" = 0.05)
  }
  statuses <- sample(names(status_probs), n, replace = TRUE, prob = status_probs)
  
  # Hours operated based on age and status
  base_hours <- pmax(100, (2024 - years) * 400 + rnorm(n, 0, 200))
  hours_operated <- ifelse(statuses == "Active", base_hours * runif(n, 1.0, 1.3),
                          ifelse(statuses == "Idle", base_hours * runif(n, 0.7, 0.9), base_hours))
  
  # Model-specific characteristics
  is_combine <- grepl("Combine", models)
  is_large_tractor <- grepl("9620RX|8R 410", models)
  
  # Fuel consumption based on model type
  fuel_consumption <- ifelse(is_combine, runif(n, 18, 25),
                            ifelse(is_large_tractor, runif(n, 15, 22), runif(n, 8, 15)))
  
  # Fuel efficiency inversely related to consumption
  fuel_efficiency <- pmax(2.5, 12 - fuel_consumption * 0.4 + rnorm(n, 0, 0.5))
  
  # Uptime based on age and maintenance status
  base_uptime <- pmax(75, 95 - (2024 - years) * 2)
  uptime <- ifelse(statuses == "Repair", runif(n, 60, 80),
                  ifelse(statuses == "Maintenance", runif(n, 80, 90),
                        base_uptime + rnorm(n, 0, 3)))
  uptime <- pmax(60, pmin(98, uptime))
  
  # Maintenance overdue more likely for older equipment and lower uptime
  maint_prob <- pmax(0.05, pmin(0.4, 0.1 + (2024 - years) * 0.03 + (95 - uptime) * 0.01))
  maintenance_overdue <- rbinom(n, 1, maint_prob) == 1
  
  # Error codes more likely with older equipment and maintenance issues
  error_prob <- ifelse(maintenance_overdue | uptime < 85, 0.5, 0.2)
  has_errors <- rbinom(n, 1, error_prob) == 1
  error_codes <- ifelse(has_errors, 
                       sample(c("E001", "E002", "E003", "W001", "W002"), n, replace = TRUE),
                       "")
  
  # Application and implement matching
  application_season_match <- list(
    "Planting" = c("Planting", "Tillage"),
    "Growing" = c("Spraying", "Tillage"),
    "Harvest" = c("Harvesting", "Transport"),
    "Off-Season" = c("Maintenance", "Transport", "Tillage")
  )
  
  applications <- sapply(seasons, function(s) {
    if(s %in% names(application_season_match)) {
      sample(application_season_match[[s]], 1)
    } else {
      sample(c("Tillage", "Planting", "Spraying", "Harvesting", "Transport"), 1)
    }
  })
  
  # Implement matching to application
  implement_map <- list(
    "Tillage" = c("Disk", "Chisel Plow", "Field Cultivator"),
    "Planting" = c("Planter", "Drill", "None"),
    "Spraying" = c("Sprayer", "None"),
    "Harvesting" = c("Header", "None"),
    "Transport" = c("Wagon", "None"),
    "Maintenance" = c("None")
  )
  
  implements <- sapply(applications, function(a) {
    if(a %in% names(implement_map)) {
      sample(implement_map[[a]], 1)
    } else {
      "None"
    }
  })
  
  # Load factor based on application and conditions
  load_base <- ifelse(applications == "Harvesting", runif(n, 75, 95),
                     ifelse(applications == "Planting", runif(n, 60, 85), runif(n, 45, 80)))
  load_factors <- pmax(30, pmin(95, load_base + ifelse(field_conditions == "Muddy", -15, 0)))
  
  # Ground speed related to conditions and load
  speed_base <- ifelse(is_combine, runif(n, 4, 8), runif(n, 6, 12))
  ground_speeds <- pmax(3, speed_base - ifelse(field_conditions == "Muddy", 2, 0) - 
                       ifelse(load_factors > 85, 1, 0))
  
  # Acres covered based on hours, speed, and status
  acres_base <- hours_operated * ground_speeds * 0.8
  acres_covered <- ifelse(statuses == "Active", acres_base * runif(n, 0.8, 1.2),
                         acres_base * runif(n, 0.3, 0.7))
  
  return(data.frame(
    equipment_id = equipment_id,
    model = models,
    year_manufactured = years,
    dealer_id = paste0("D", sprintf("%03d", sample(1:50, n, replace = TRUE))),
    customer_id = paste0("C", sprintf("%04d", sample(1:200, n, replace = TRUE))),
    region = locations$region,
    state = locations$state,
    county = locations$county,
    latitude = locations$latitude,
    longitude = locations$longitude,
    date_recorded = current_date - sample(0:30, n, replace = TRUE),
    season = seasons,
    hours_operated = round(hours_operated, 1),
    fuel_consumption_gph = round(fuel_consumption, 2),
    fuel_efficiency_mph = round(fuel_efficiency, 2),
    uptime_percentage = round(uptime, 1),
    acres_covered = round(acres_covered, 0),
    ground_speed_mph = round(ground_speeds, 1),
    status = statuses,
    maintenance_overdue = maintenance_overdue,
    error_codes = error_codes,
    operator_id = paste0("OP", sprintf("%03d", sample(1:150, n, replace = TRUE))),
    application_type = applications,
    implement_attached = implements,
    load_factor_percentage = round(load_factors, 1),
    field_conditions = field_conditions,
    temperature_f = round(temperatures, 0),
    soil_type = sample(c("Clay", "Loam", "Sandy", "Silt"), n, replace = TRUE),
    stringsAsFactors = FALSE
  ))
}

# Create machine-level data with product line attributes
create_machine_data_with_product_lines <- function(locations) {
  n <- nrow(locations)
  
  # Define product line characteristics
  product_lines <- data.frame(
    model_series = c("S700 Series", "S600 Series", "X9 Series", "9R Series", 
                     "8R Series", "7R Series", "6R Series", "9620RX Series", "8RX Series"),
    specific_models = c("S780, S790", "S660, S670, S680", "X9 1000, X9 1100", 
                       "9470R, 9520R, 9570R", "8320R, 8370R, 8400R", 
                       "7200R, 7250R, 7280R", "6120R, 6140R, 6155R",
                       "9620RX", "8295RX, 8320RX, 8345RX"),
    category = c("Combine", "Combine", "Combine", "Tractor", "Tractor", 
                 "Tractor", "Tractor", "Track Tractor", "Track Tractor"),
    intro_year = c(2018, 2012, 2020, 2019, 2014, 2007, 2010, 2017, 2019),
    stringsAsFactors = FALSE
  )
  
  # Sample product lines with appropriate weights
  series_weights <- c(30, 40, 15, 25, 60, 70, 80, 15, 10) # Fleet size weights
  selected_series <- sample(product_lines$model_series, n, replace = TRUE, prob = series_weights)
  
  # Create individual machine records
  equipment_data <- data.frame(
    equipment_id = paste0("JD", sprintf("%05d", 1:n)),
    stringsAsFactors = FALSE
  )
  
  # Add product line attributes for each machine
  for (i in 1:n) {
    series <- selected_series[i]
    series_info <- product_lines[product_lines$model_series == series, ]
    
    equipment_data$model_series[i] <- series
    equipment_data$category[i] <- series_info$category
    equipment_data$intro_year[i] <- series_info$intro_year
    
    # Generate specific model within series
    models_in_series <- strsplit(series_info$specific_models, ", ")[[1]]
    equipment_data$specific_model[i] <- sample(models_in_series, 1)
    
    # Year manufactured within realistic range for the series
    min_year <- max(series_info$intro_year, 2010)
    max_year <- min(series_info$intro_year + 8, 2024)
    equipment_data$year_manufactured[i] <- sample(min_year:max_year, 1)
    
    # Customer and dealer information
    equipment_data$customer_id[i] <- paste0("C", sprintf("%04d", sample(1:200, 1)))
    equipment_data$dealer_id[i] <- paste0("D", sprintf("%03d", sample(1:50, 1)))
    
    # Location information
    equipment_data$region[i] <- locations$region[i]
    equipment_data$state[i] <- locations$state[i]
    equipment_data$county[i] <- locations$county[i]
    equipment_data$latitude[i] <- locations$latitude[i]
    equipment_data$longitude[i] <- locations$longitude[i]
    
    # Performance metrics (based on age and category)
    machine_age <- 2024 - equipment_data$year_manufactured[i]
    
    # Annual hours based on category and age
    if (series_info$category == "Combine") {
      equipment_data$annual_hours[i] <- round(runif(1, 180, 380), 0)
    } else if (str_detect(series, "9R|9620RX|8R|8RX")) {
      equipment_data$annual_hours[i] <- round(runif(1, 700, 1300), 0)
    } else {
      equipment_data$annual_hours[i] <- round(runif(1, 500, 1000), 0)
    }
    
    # Cumulative hours based on age and annual usage
    base_hours <- equipment_data$annual_hours[i] * machine_age
    equipment_data$total_hours[i] <- round(base_hours * runif(1, 0.8, 1.2), 0)
    
    # Fuel efficiency based on category and generation
    if (series_info$category == "Combine") {
      if (machine_age <= 3) {
        equipment_data$fuel_efficiency[i] <- round(runif(1, 4.5, 6.2), 2)
      } else {
        equipment_data$fuel_efficiency[i] <- round(runif(1, 3.8, 5.5), 2)
      }
    } else if (str_detect(series, "9R|9620RX")) {
      equipment_data$fuel_efficiency[i] <- round(runif(1, 4.0, 5.0), 2)
    } else if (str_detect(series, "8R|8RX")) {
      equipment_data$fuel_efficiency[i] <- round(runif(1, 4.5, 5.8), 2)
    } else if (str_detect(series, "7R")) {
      equipment_data$fuel_efficiency[i] <- round(runif(1, 5.0, 6.5), 2)
    } else {
      equipment_data$fuel_efficiency[i] <- round(runif(1, 5.5, 7.2), 2)
    }
    
    # Customer satisfaction (varies by product line and age)
    base_satisfaction <- case_when(
      str_detect(series, "X9|S700") ~ runif(1, 4.2, 4.8),
      str_detect(series, "9R|9620RX") ~ runif(1, 4.0, 4.6),
      str_detect(series, "S600|8R") ~ runif(1, 3.8, 4.4),
      TRUE ~ runif(1, 3.5, 4.2)
    )
    equipment_data$customer_satisfaction[i] <- round(pmax(1, base_satisfaction - (machine_age * 0.1)), 1)
    
    # Purchase price (varies by category and model year)
    if (series_info$category == "Combine") {
      if (str_detect(series, "X9")) {
        base_price <- runif(1, 650, 750)
      } else if (str_detect(series, "S700")) {
        base_price <- runif(1, 520, 600)
      } else {
        base_price <- runif(1, 400, 480)
      }
    } else if (str_detect(series, "9R|9620RX")) {
      base_price <- runif(1, 450, 520)
    } else if (str_detect(series, "8R|8RX")) {
      base_price <- runif(1, 350, 420)
    } else if (str_detect(series, "7R")) {
      base_price <- runif(1, 250, 320)
    } else {
      base_price <- runif(1, 180, 250)
    }
    
    # Adjust for model year (newer = more expensive)
    year_factor <- 1 + (equipment_data$year_manufactured[i] - 2015) * 0.03
    equipment_data$purchase_price_k[i] <- round(base_price * year_factor, 0)
  }
  
  return(equipment_data)
}

# Generate the data
locations <- generate_realistic_locations(500)
equipment_data <- create_machine_data_with_product_lines(locations)