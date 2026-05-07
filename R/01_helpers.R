# R/01_helpers.R
# Project: Spatial Risk Forecasting and Route Optimization
# Purpose: Cleaned helper functions for grid construction, crime aggregation,
#          ZINB modeling, Google route parsing, and route risk scoring.
#
# Notes:
# - No working directory is set here.
# - No API key is stored here. Use Sys.getenv("GOOGLE_MAPS_API_KEY").
# - Functions avoid relying on global objects where possible.

# ---------------------------------------------------------------------
# Grid utilities
# ---------------------------------------------------------------------

make_downtown_grid <- function(
  x_min = -122.45,
  x_max = -122.40,
  y_min = 37.75,
  y_max = 37.805,
  x_step = 0.001850575,
  y_step = 0.001460563
) {
  x0 <- seq(from = x_min, to = x_max, by = x_step)
  y0 <- seq(from = y_min, to = y_max, by = y_step)

  grid <- expand.grid(x_0 = x0, y_0 = y0) |>
    tibble::as_tibble() |>
    dplyr::mutate(
      x_1 = x_0 + x_step,
      y_1 = y_0 + y_step
    ) |>
    dplyr::arrange(x_0, y_0) |>
    dplyr::mutate(
      cell_id = dplyr::row_number(),
      grid_x = as.integer(factor(x_0, levels = sort(unique(x_0)))),
      grid_y = as.integer(factor(y_0, levels = sort(unique(y_0))))
    ) |>
    dplyr::select(cell_id, grid_x, grid_y, x_0, x_1, y_0, y_1)

  grid
}

grid_breaks <- function(grid) {
  list(
    x = sort(unique(c(grid$x_0, grid$x_1))),
    y = sort(unique(c(grid$y_0, grid$y_1)))
  )
}

add_grid_centers <- function(grid_data) {
  grid_data |>
    dplyr::mutate(
      center_x = (x_0 + x_1) / 2,
      center_y = (y_0 + y_1) / 2
    )
}

attach_grid_districts <- function(grid_data, districts = NULL, district_path = NULL) {
  if (!is.null(district_path)) {
    district_df <- readr::read_csv(district_path, show_col_types = FALSE)
    if (!"District" %in% names(district_df)) {
      stop("district_path must contain a column named 'District'.", call. = FALSE)
    }
    districts <- district_df$District
  }

  if (is.null(districts)) {
    stop("Provide either districts or district_path.", call. = FALSE)
  }

  if (length(districts) != nrow(grid_data)) {
    stop(
      "Length of districts does not match number of grid cells. ",
      "Expected ", nrow(grid_data), " but got ", length(districts), ".",
      call. = FALSE
    )
  }

  grid_data |>
    dplyr::mutate(District = as.factor(districts))
}

# ---------------------------------------------------------------------
# Crime data cleaning
# ---------------------------------------------------------------------

standardize_crime_columns <- function(data) {
  data <- janitor::clean_names(data)

  # Support both the older SFPD column name and clearer alternatives.
  if (!"incidnt_num" %in% names(data)) {
    if ("incident_number" %in% names(data)) {
      data <- dplyr::rename(data, incidnt_num = incident_number)
    } else if ("incident_num" %in% names(data)) {
      data <- dplyr::rename(data, incidnt_num = incident_num)
    }
  }

  required <- c("category", "date", "time", "x", "y", "pd_district", "incidnt_num")
  missing <- setdiff(required, names(data))
  if (length(missing) > 0) {
    stop(
      "Crime data is missing required columns: ",
      paste(missing, collapse = ", "),
      call. = FALSE
    )
  }

  data
}

prepare_crime_data <- function(
  raw_data,
  seriousness_lookup,
  missing_district_fallback = "INGLESIDE"
) {
  if (!all(c("category", "seriousness") %in% names(seriousness_lookup))) {
    stop("seriousness_lookup must contain columns: category, seriousness.", call. = FALSE)
  }

  data <- standardize_crime_columns(raw_data)

  data <- data |>
    dplyr::mutate(
      date = as.Date(date, "%m/%d/%Y"),
      year = lubridate::year(date),
      month = lubridate::month(date),
      day = lubridate::day(date),
      day_of_week = factor(
        day_of_week,
        levels = c(
          "Monday", "Tuesday", "Wednesday", "Thursday",
          "Friday", "Saturday", "Sunday"
        )
      ),
      category = as.factor(category),
      pd_district = as.factor(pd_district)
    )

  # The original data had a small number of missing districts. For the course project,
  # these were manually assigned to INGLESIDE based on nearby coordinates.
  data$pd_district[is.na(data$pd_district)] <- missing_district_fallback

  data <- data |>
    tidyr::separate(
      time,
      into = c("hour", "minute", "second"),
      sep = ":",
      remove = TRUE,
      convert = TRUE,
      fill = "right"
    ) |>
    dplyr::select(-second)

  data <- data |>
    dplyr::left_join(seriousness_lookup, by = "category") |>
    dplyr::filter(!is.na(seriousness))

  # Deduplicate incidents and aggregate seriousness when one incident has
  # multiple crime labels.
  incident_seriousness <- data |>
    dplyr::group_by(incidnt_num) |>
    dplyr::summarise(seriousness_total = sum(seriousness, na.rm = TRUE), .groups = "drop")

  data_unique <- data |>
    dplyr::distinct(incidnt_num, .keep_all = TRUE) |>
    dplyr::left_join(incident_seriousness, by = "incidnt_num") |>
    dplyr::filter(!is.na(x), !is.na(y))

  data_unique
}

filter_to_grid_bbox <- function(crime_data, grid) {
  crime_data |>
    dplyr::filter(
      min(grid$x_0) < x, x < max(grid$x_1),
      min(grid$y_0) < y, y < max(grid$y_1)
    )
}

assign_crimes_to_grid <- function(crime_data, grid) {
  breaks <- grid_breaks(grid)
  n_x <- length(unique(grid$grid_x))
  n_y <- length(unique(grid$grid_y))

  assigned <- crime_data |>
    dplyr::mutate(
      grid_x = findInterval(x, breaks$x, rightmost.closed = TRUE),
      grid_y = findInterval(y, breaks$y, rightmost.closed = TRUE)
    ) |>
    dplyr::filter(
      grid_x >= 1, grid_x <= n_x,
      grid_y >= 1, grid_y <= n_y
    ) |>
    dplyr::left_join(
      grid |> dplyr::select(cell_id, grid_x, grid_y),
      by = c("grid_x", "grid_y")
    )

  assigned
}

aggregate_grid_counts <- function(
  crime_data,
  grid,
  day_of_week = NULL,
  hour = NULL,
  years = 2003:2018
) {
  filtered <- crime_data |>
    dplyr::filter(year %in% years)

  if (!is.null(day_of_week)) {
    filtered <- filtered |> dplyr::filter(.data$day_of_week == day_of_week)
  }

  if (!is.null(hour)) {
    filtered <- filtered |> dplyr::filter(.data$hour == hour)
  }

  assigned <- assign_crimes_to_grid(filtered, grid)

  summary_by_cell <- assigned |>
    dplyr::group_by(cell_id) |>
    dplyr::summarise(
      total_count = dplyr::n(),
      total_seriousness = sum(seriousness_total, na.rm = TRUE),
      .groups = "drop"
    )

  n_years <- length(unique(years))

  out <- grid |>
    dplyr::left_join(summary_by_cell, by = "cell_id") |>
    dplyr::mutate(
      total_count = tidyr::replace_na(total_count, 0L),
      total_seriousness = tidyr::replace_na(total_seriousness, 0),
      # ZINB uses integer counts. This is the average yearly count,
      # rounded upward to preserve rare but nonzero cells.
      Count = ceiling(total_count / n_years),
      # Average yearly seriousness. For empty cells, use a small positive
      # baseline to avoid multiplying fitted counts by exactly zero.
      Severity = dplyr::if_else(
        total_count == 0,
        1 / n_years,
        total_seriousness / n_years
      )
    ) |>
    dplyr::select(
      cell_id, grid_x, grid_y, x_0, x_1, y_0, y_1,
      Count, Severity, total_count, total_seriousness
    )

  out
}

# ---------------------------------------------------------------------
# ZINB modeling and risk scoring
# ---------------------------------------------------------------------

fit_zinb_model <- function(grid_counts) {
  required <- c("Count", "District", "x_0", "x_1", "y_0", "y_1")
  missing <- setdiff(required, names(grid_counts))
  if (length(missing) > 0) {
    stop(
      "grid_counts is missing required columns: ",
      paste(missing, collapse = ", "),
      call. = FALSE
    )
  }

  model_data <- add_grid_centers(grid_counts) |>
    dplyr::mutate(
      Ave_x = as.numeric(scale(center_x)),
      Ave_y = as.numeric(scale(center_y)),
      District = as.factor(District)
    )

  model <- pscl::zeroinfl(
    Count ~ Ave_x + Ave_y | District,
    data = model_data,
    dist = "negbin"
  )

  list(model = model, model_data = model_data)
}

predict_risk_scores <- function(zinb_fit) {
  model <- zinb_fit$model
  model_data <- zinb_fit$model_data

  predicted_count <- as.numeric(
    stats::predict(model, newdata = model_data, type = "count")
  )

  model_data |>
    dplyr::mutate(
      predicted_count = predicted_count,
      score = predicted_count * Severity
    )
}

# ---------------------------------------------------------------------
# Google Directions helpers
# ---------------------------------------------------------------------

get_google_directions <- function(
  time_date,
  starting,
  ending,
  mode = "walking",
  alternatives = TRUE,
  api_key = Sys.getenv("GOOGLE_MAPS_API_KEY")
) {
  if (!nzchar(api_key)) {
    stop(
      "GOOGLE_MAPS_API_KEY is not set. Use Sys.setenv(GOOGLE_MAPS_API_KEY = 'your-key') ",
      "or add it to ~/.Renviron.",
      call. = FALSE
    )
  }

  departure_time <- as.POSIXct(time_date)

  res <- googleway::google_directions(
    origin = starting,
    destination = ending,
    mode = mode,
    departure_time = departure_time,
    key = api_key,
    alternatives = alternatives,
    simplify = FALSE
  )

  RJSONIO::fromJSON(paste(res, collapse = ""))
}

extract_route_coordinates <- function(num_route, directions_json) {
  steps <- directions_json$routes[[num_route]]$legs[[1]]$steps
  n <- length(steps)

  start_res <- matrix(NA_real_, nrow = n, ncol = 2)
  end_res <- matrix(NA_real_, nrow = n, ncol = 2)

  for (i in seq_len(n)) {
    start_res[i, ] <- unlist(steps[[i]]$start_location)
    end_res[i, ] <- unlist(steps[[i]]$end_location)
  }

  route <- data.frame(start_res, end_res)
  names(route) <- c("y_start", "x_start", "y_end", "x_end")
  tibble::as_tibble(route)
}

extract_route_times <- function(num_route, directions_json) {
  steps <- directions_json$routes[[num_route]]$legs[[1]]$steps
  n <- length(steps)

  tibble::tibble(
    segment_id = seq_len(n),
    seconds = vapply(
      steps,
      function(step) as.numeric(step$duration$value),
      numeric(1)
    )
  )
}

extract_all_routes <- function(directions_json) {
  n_routes <- length(directions_json$routes)

  lapply(seq_len(n_routes), function(i) {
    list(
      route_id = i,
      coordinates = extract_route_coordinates(i, directions_json),
      times = extract_route_times(i, directions_json)
    )
  })
}

# ---------------------------------------------------------------------
# Route-to-grid intersection and route scoring
# ---------------------------------------------------------------------

segment_cells <- function(
  x_start,
  y_start,
  x_end,
  y_end,
  risk_grid,
  tol = 1e-12
) {
  breaks <- grid_breaks(risk_grid)
  x_breaks <- breaks$x
  y_breaks <- breaks$y

  dx <- x_end - x_start
  dy <- y_end - y_start

  t_values <- c(0, 1)

  if (abs(dx) > tol) {
    x_cross <- x_breaks[
      x_breaks > min(x_start, x_end) + tol &
        x_breaks < max(x_start, x_end) - tol
    ]
    t_values <- c(t_values, (x_cross - x_start) / dx)
  }

  if (abs(dy) > tol) {
    y_cross <- y_breaks[
      y_breaks > min(y_start, y_end) + tol &
        y_breaks < max(y_start, y_end) - tol
    ]
    t_values <- c(t_values, (y_cross - y_start) / dy)
  }

  t_values <- sort(unique(round(t_values[t_values >= 0 & t_values <= 1], 12)))

  if (length(t_values) < 2) {
    return(risk_grid[0, ])
  }

  mid_t <- (head(t_values, -1) + tail(t_values, -1)) / 2

  midpoints <- tibble::tibble(
    x = x_start + mid_t * dx,
    y = y_start + mid_t * dy
  ) |>
    dplyr::mutate(
      grid_x = findInterval(x, x_breaks, rightmost.closed = TRUE),
      grid_y = findInterval(y, y_breaks, rightmost.closed = TRUE)
    )

  n_x <- length(unique(risk_grid$grid_x))
  n_y <- length(unique(risk_grid$grid_y))

  midpoints <- midpoints |>
    dplyr::filter(
      grid_x >= 1, grid_x <= n_x,
      grid_y >= 1, grid_y <= n_y
    ) |>
    dplyr::distinct(grid_x, grid_y)

  if (nrow(midpoints) == 0) {
    return(risk_grid[0, ])
  }

  risk_grid |>
    dplyr::semi_join(midpoints, by = c("grid_x", "grid_y"))
}

score_route <- function(
  route_coordinates,
  risk_grid,
  route_times = NULL,
  time_weight = c("none", "divide_by_seconds", "multiply_by_seconds")
) {
  time_weight <- match.arg(time_weight)

  if (nrow(route_coordinates) == 0) {
    return(tibble::tibble(route_score = NA_real_, n_cells = 0L))
  }

  segment_scores <- vector("list", nrow(route_coordinates))

  for (i in seq_len(nrow(route_coordinates))) {
    cells <- segment_cells(
      x_start = route_coordinates$x_start[i],
      y_start = route_coordinates$y_start[i],
      x_end = route_coordinates$x_end[i],
      y_end = route_coordinates$y_end[i],
      risk_grid = risk_grid
    )

    raw_score <- sum(cells$score, na.rm = TRUE)

    if (time_weight == "none") {
      adjusted_score <- raw_score
    } else {
      if (is.null(route_times)) {
        stop("route_times is required when time_weight is not 'none'.", call. = FALSE)
      }

      seconds <- route_times$seconds[i]

      if (time_weight == "divide_by_seconds") {
        adjusted_score <- raw_score / seconds
      } else {
        adjusted_score <- raw_score * seconds
      }
    }

    segment_scores[[i]] <- tibble::tibble(
      segment_id = i,
      raw_score = raw_score,
      adjusted_score = adjusted_score,
      n_cells = nrow(cells)
    )
  }

  segment_scores <- dplyr::bind_rows(segment_scores)

  if (time_weight == "none") {
    # For the unweighted score, count each traversed grid cell once.
    all_cells <- dplyr::bind_rows(lapply(seq_len(nrow(route_coordinates)), function(i) {
      segment_cells(
        x_start = route_coordinates$x_start[i],
        y_start = route_coordinates$y_start[i],
        x_end = route_coordinates$x_end[i],
        y_end = route_coordinates$y_end[i],
        risk_grid = risk_grid
      )
    })) |>
      dplyr::distinct(cell_id, .keep_all = TRUE)

    route_score <- sum(all_cells$score, na.rm = TRUE)
    n_cells <- nrow(all_cells)
  } else {
    route_score <- sum(segment_scores$adjusted_score, na.rm = TRUE)
    n_cells <- sum(segment_scores$n_cells, na.rm = TRUE)
  }

  tibble::tibble(
    route_score = route_score,
    n_cells = n_cells
  )
}

score_all_routes <- function(
  routes,
  risk_grid,
  time_weight = c("none", "divide_by_seconds", "multiply_by_seconds")
) {
  time_weight <- match.arg(time_weight)

  dplyr::bind_rows(lapply(routes, function(route) {
    out <- score_route(
      route_coordinates = route$coordinates,
      route_times = route$times,
      risk_grid = risk_grid,
      time_weight = time_weight
    )

    out |>
      dplyr::mutate(route_id = route$route_id, .before = 1)
  })) |>
    dplyr::arrange(route_score)
}
