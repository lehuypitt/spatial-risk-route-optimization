# Data

Do not commit the full raw crime dataset to GitHub.

Recommended local files:

```text
data/
├── Police_Department_Incident_Reports__Historical_2003_to_May_2018.csv
├── District.csv
└── seriousness_lookup.csv
```

Expected columns:

## Crime data

The raw DataSF/SFPD historical incident data should include at least:

```text
category
date
time
x
y
pd_district
incidnt_num
day_of_week
```

The helper script also supports alternative incident-number column names such as `incident_number` or `incident_num`.

## seriousness_lookup.csv

Must contain:

```text
category,seriousness
```

Only rows with non-missing seriousness scores are retained.

## District.csv

Must contain:

```text
District
```

There should be one district label per grid cell, matching the order returned by `make_downtown_grid()`.
