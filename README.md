# Spatial Risk Forecasting and Route Optimization

This repository contains cleaned R scripts for a Berkeley STAT 222 project on estimating lower-risk walking routes in San Francisco using historical crime data.

The project:
1. divides downtown San Francisco into 0.1-mile grid cells,
2. aggregates historical reported crimes by grid cell,
3. fits a zero-inflated negative binomial regression model,
4. converts fitted counts and crime seriousness weights into grid-level risk scores, and
5. scores Google Maps walking route alternatives by the grid cells they cross.

## Repository structure

```text
spatial-risk-route-optimization/
├── R/
│   ├── 00_setup.R
│   ├── 01_helpers.R
│   └── 02_route_risk_pipeline.R
├── data/
│   └── README.md
├── paper/
│   └── STAT222_Project_Final.pdf
├── .gitignore
└── README.md
```

## Data

The raw incident-level crime data should not be committed directly to GitHub because it is large. Download it from DataSF/San Francisco Police Department historical incident reports and place it locally in `data/`.

You also need a seriousness lookup file with columns:

```text
category,seriousness
```

If using district-level zero inflation, provide a `District.csv` file with one district label per grid cell in the same order returned by `make_downtown_grid()`.

## Google Maps API key

Do not put an API key in the code. Set it as an environment variable:

```r
Sys.setenv(GOOGLE_MAPS_API_KEY = "your-key")
```

or add this line to `~/.Renviron`:

```text
GOOGLE_MAPS_API_KEY=your-key
```

## Example usage

```r
source("R/00_setup.R")
source("R/01_helpers.R")
source("R/02_route_risk_pipeline.R")

raw <- readr::read_csv("data/Police_Department_Incident_Reports__Historical_2003_to_May_2018.csv")
seriousness_lookup <- readr::read_csv("data/seriousness_lookup.csv")

result <- get_route_risk(
  raw_crime_data = raw,
  seriousness_lookup = seriousness_lookup,
  district_path = "data/District.csv",
  day_of_week = "Friday",
  hour = 18L,
  time_date = "2019-05-10 18:00:00 PDT",
  starting = "24 Hour Fitness, San Francisco",
  ending = "Civic Center, San Francisco"
)

result$route_scores
```

## Important limitations

This was an academic project and should not be interpreted as a production safety recommendation system. Reported crime data reflect reporting processes, enforcement patterns, geography, and historical biases. Route scores are intended to demonstrate spatial statistical modeling and route-scoring methodology, not to make real-time safety guarantees.

## Collaborators

This project was completed as a STAT 222 course project at UC Berkeley with collaborators listed in the accompanying paper.
