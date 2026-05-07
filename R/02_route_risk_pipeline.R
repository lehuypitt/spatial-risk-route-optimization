# R/02_route_risk_pipeline.R
# Project: Spatial Risk Forecasting and Route Optimization
# Purpose: Main project pipeline that combines data cleaning, grid aggregation,
#          ZINB modeling, Google route extraction, and route scoring.
#
# Expected inputs:
# - raw_crime_data: DataSF/SFPD historical incident data, already read into R.
# - seriousness_lookup: data frame with columns category and seriousness.
# - districts or district_path: grid-cell district labels matching the grid order.
#
# Example:
#   source("R/00_setup.R")
#   source("R/01_helpers.R")
#   source("R/02_route_risk_pipeline.R")
#
#   raw <- readr::read_csv("data/Police_Department_Incident_Reports__Historical_2003_to_May_2018.csv")
#   seriousness_lookup <- readr::read_csv("data/seriousness_lookup.csv")
#
#   result <- get_route_risk(
#     raw_crime_data = raw,
#     seriousness_lookup = seriousness_lookup,
#     district_path = "data/District.csv",
#     day_of_week = "Friday",
#     hour = 18L,
#     time_date = "2019-05-10 18:00:00 PDT",
#     starting = "24 Hour Fitness, San Francisco",
#     ending = "Civic Center, San Francisco"
#   )
#
#   result$route_scores

get_route_risk <- function(
  raw_crime_data,
  seriousness_lookup,
  districts = NULL,
  district_path = NULL,
  day_of_week,
  hour,
  time_date,
  starting,
  ending,
  years = 2003:2018,
  time_weight = c("none", "divide_by_seconds", "multiply_by_seconds"),
  api_key = Sys.getenv("GOOGLE_MAPS_API_KEY")
) {
  time_weight <- match.arg(time_weight)

  grid <- make_downtown_grid()

  crime_data <- prepare_crime_data(
    raw_data = raw_crime_data,
    seriousness_lookup = seriousness_lookup
  ) |>
    filter_to_grid_bbox(grid)

  grid_counts <- aggregate_grid_counts(
    crime_data = crime_data,
    grid = grid,
    day_of_week = day_of_week,
    hour = hour,
    years = years
  ) |>
    attach_grid_districts(
      districts = districts,
      district_path = district_path
    )

  zinb_fit <- fit_zinb_model(grid_counts)
  risk_grid <- predict_risk_scores(zinb_fit)

  directions_json <- get_google_directions(
    time_date = time_date,
    starting = starting,
    ending = ending,
    api_key = api_key
  )

  routes <- extract_all_routes(directions_json)

  route_scores <- score_all_routes(
    routes = routes,
    risk_grid = risk_grid,
    time_weight = time_weight
  )

  list(
    route_scores = route_scores,
    risk_grid = risk_grid,
    model = zinb_fit$model,
    model_data = zinb_fit$model_data,
    routes = routes,
    directions_json = directions_json
  )
}
