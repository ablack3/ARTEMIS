# Example ARTEMIS run script against an OMOP CDM database.
# This script includes:
# - Database connection setup
# - All key ARTEMIS input parameters
# - Pipeline execution
# - Result inspection and export

library(ARTEMIS)
library(DatabaseConnector)

## -----------------------------
## 1) Database connection setup
## -----------------------------

# Option A (quick demo): SQLite CDM bundled in ARTEMIS
db_path <- system.file("extdata", "testing_db.sqlite", package = "ARTEMIS")
connectionDetails <- DatabaseConnector::createConnectionDetails(
  dbms = "sqlite",
  server = db_path
)
cdmSchema <- "main"
writeSchema <- "main"

# Validate the connection once and ensure we always close it.
validationConnection <- DatabaseConnector::connect(connectionDetails)
on.exit(DatabaseConnector::disconnect(validationConnection), add = TRUE)
DatabaseConnector::querySql(validationConnection, "SELECT 1;")

# Option B (example template): PostgreSQL OMOP CDM
# connectionDetails <- DatabaseConnector::createConnectionDetails(
#   dbms = "postgresql",
#   server = "hostname/database",
#   user = "my_user",
#   password = "my_password",
#   port = "5432",
#   pathToDriver = Sys.getenv("DATABASECONNECTOR_JAR_FOLDER")
# )
# cdmSchema <- "cdm_schema"
# writeSchema <- "results_schema"
# validationConnection <- DatabaseConnector::connect(connectionDetails)
# on.exit(DatabaseConnector::disconnect(validationConnection), add = TRUE)
# DatabaseConnector::querySql(validationConnection, "SELECT 1;")

## ------------------------
## 2) ARTEMIS run settings
## ------------------------

# Cohort inputs
df_json <- loadCohort()
cohort_name <- "lungcancer"
cohort_json <- df_json$json[1]

# Drug/regimen inputs
validdrugs <- loadDrugs()
regimens <- loadRegimens(condition = "all")
regGroups <- loadGroups()

# Alignment inputs (generateRawAlignments)
gap_penalty <- 0.4                     # g
time_penalty_factor <- 0.4             # Tfac
score_matrix <- NULL                   # set to defaultSmatrix for custom scoring
verbose <- 0                           # 0, 1, or 2
memory_window <- -1                    # mem
loss_method <- "PropDiff"              # PropDiff | AbsDiff | Quadratic | PropQuadratic | LogCosh

# Post-processing inputs
min_valid_drugs <- 1                   # filter_stringDF threshold
regimen_combine_days <- 28             # processAlignments::regimenCombine
discontinuation_days <- 120            # calculateEras::discontinuationTime

## -------------------------
## 3) Build cohort drug data
## -------------------------

con_df <- getConDF(
  connectionDetails = connectionDetails,
  json = cohort_json,
  name = cohort_name,
  cdmSchema = cdmSchema,
  writeSchema = writeSchema
)

# SQLite test DB can return numeric epoch dates; coerce when needed.
if (is.numeric(con_df$drug_exposure_start_date) ||
    is.integer(con_df$drug_exposure_start_date)) {
  con_df$drug_exposure_start_date <- as.POSIXct(
    con_df$drug_exposure_start_date,
    origin = "1970-01-01",
    tz = "UTC"
  )
}

stringDF <- stringDF_from_cdm(
  con_df = con_df,
  validDrugs = validdrugs
)

stringDF <- filter_stringDF(
  stringDF = stringDF,
  min = min_valid_drugs
)

## -----------------------
## 4) Run ARTEMIS pipeline
## -----------------------

output_all <- generateRawAlignments(
  stringDF = stringDF,
  regimens = regimens,
  g = gap_penalty,
  Tfac = time_penalty_factor,
  s = score_matrix,
  verbose = verbose,
  mem = memory_window,
  method = loss_method
)

processedAll <- processAlignments(
  rawOutput = output_all,
  regimenCombine = regimen_combine_days,
  regimens = regimens
)

processedEras <- calculateEras(
  processedAll = processedAll,
  discontinuationTime = discontinuation_days
)

regStats <- generateRegimenStats(processedEras)

## ---------------------
## 5) View key outputs
## ---------------------

cat("\nRows in raw output: ", nrow(output_all), "\n", sep = "")
cat("Rows in processed output: ", nrow(processedAll), "\n", sep = "")
cat("Rows in era output: ", nrow(processedEras), "\n\n", sep = "")

head(output_all)
head(processedAll)
head(processedEras)
head(regStats)

# Visualization helpers
plotAlignment(processedEras)
plotScoreDistribution(processedEras)
plotRegimenLengthDistribution(processedEras)
plotFrequency(processedEras, top_n = 10)
plotSankey(processedEras, regGroups)

## --------------------------
## 6) Optional artifact write
## --------------------------

# Writes anonymized patient-level CSVs, summary stats, and plots as zip files.
writeOutputs(
  output_all = output_all,
  processedAll = processedAll,
  processedEras = processedEras,
  regGroups = regGroups,
  regStats = regStats,
  connectionDetails = connectionDetails,
  cdmSchema = cdmSchema,
  con_df = con_df,
  stringDF = stringDF,
  skipSummaryStats = FALSE
)
