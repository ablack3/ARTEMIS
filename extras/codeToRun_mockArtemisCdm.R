# Example ARTEMIS run script using the bundled DuckDB mock CDM.
#
# Usage:
#   Rscript extras/codeToRun_mockArtemisCdm.R           # fast mode (default)
#   Rscript extras/codeToRun_mockArtemisCdm.R full      # full mode

suppressPackageStartupMessages({
  library(ARTEMIS)
  library(DatabaseConnector)
})

args <- commandArgs(trailingOnly = TRUE)
mode <- if (length(args) >= 1 && tolower(args[[1]]) == "full") "full" else "fast"

cdm_path <- loadMockArtemisCdm()
connectionDetails <- createMockArtemisConnectionDetails(absolute = cdm_path)
cdmSchema <- "main"
writeSchema <- "main"

# Validate connection and ensure it is closed.
validationConnection <- DatabaseConnector::connect(connectionDetails)
on.exit(DatabaseConnector::disconnect(validationConnection), add = TRUE)
DatabaseConnector::querySql(validationConnection, "SELECT 1;")

df_json <- loadCohort()
validdrugs <- loadDrugs()
regimens <- loadRegimens(condition = "all")
regGroups <- loadGroups()

if (mode == "fast") {
  regimens <- regimens[seq_len(min(120, nrow(regimens))), , drop = FALSE]
}

cohort_name <- paste0("mock_lung_", format(Sys.time(), "%Y%m%d%H%M%S"))
con_df <- getConDF(
  connectionDetails = connectionDetails,
  json = df_json$json[1],
  name = cohort_name,
  cdmSchema = cdmSchema,
  writeSchema = writeSchema
)

stringDF <- stringDF_from_cdm(con_df = con_df, validDrugs = validdrugs)
stringDF <- filter_stringDF(stringDF = stringDF, min = 1)

if (mode == "fast" && nrow(stringDF) > 200) {
  set.seed(20260306)
  sampled_ids <- sample(stringDF$person_id, size = 200, replace = FALSE)
  stringDF <- stringDF[stringDF$person_id %in% sampled_ids, , drop = FALSE]
}

output_all <- generateRawAlignments(
  stringDF = stringDF,
  regimens = regimens,
  g = 0.4,
  Tfac = 0.4,
  verbose = 0,
  mem = -1,
  method = "PropDiff"
)

processedAll <- processAlignments(
  rawOutput = output_all,
  regimens = regimens,
  regimenCombine = 28
)

processedEras <- calculateEras(
  processedAll = processedAll,
  discontinuationTime = 120
)

regStats <- generateRegimenStats(processedEras)

cat("\nMode: ", mode, "\n", sep = "")
cat("Rows in con_df: ", nrow(con_df), "\n", sep = "")
cat("Rows in stringDF: ", nrow(stringDF), "\n", sep = "")
cat("Rows in output_all: ", nrow(output_all), "\n", sep = "")
cat("Rows in processedAll: ", nrow(processedAll), "\n", sep = "")
cat("Rows in processedEras: ", nrow(processedEras), "\n\n", sep = "")

print(head(regStats))

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
