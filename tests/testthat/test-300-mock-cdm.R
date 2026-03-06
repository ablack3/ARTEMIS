test_that("mock CDM helpers return a valid path and connection details", {
  testthat::skip_if_not_installed("duckdb")

  db_path <- ARTEMIS::loadMockArtemisCdm()
  expect_true(file.exists(db_path))

  connectionDetails <- ARTEMIS::createMockArtemisConnectionDetails()
  con <- DatabaseConnector::connect(connectionDetails)
  on.exit(DatabaseConnector::disconnect(con), add = TRUE)

  n_person <- DatabaseConnector::querySql(con, "SELECT COUNT(*) AS n FROM main.person;")
  expect_gte(as.integer(n_person$n[[1]]), 1500L)
})

test_that("mock CDM works with default cohort and yields treated + untreated", {
  testthat::skip_if_not_installed("duckdb")

  connectionDetails <- ARTEMIS::createMockArtemisConnectionDetails()
  df_json <- ARTEMIS::loadCohort()
  valid_drugs <- ARTEMIS::loadDrugs()

  cohort_name <- paste0("mock_cohort_", as.integer(Sys.time()))
  con_df <- ARTEMIS::getConDF(
    connectionDetails = connectionDetails,
    json = df_json$json[1],
    name = cohort_name,
    cdmSchema = "main",
    writeSchema = "main"
  )

  expect_gt(nrow(con_df), 0)
  expect_gt(length(unique(con_df$person_id)), 0)

  stringDF <- ARTEMIS::stringDF_from_cdm(con_df = con_df, validDrugs = valid_drugs)
  expect_gt(nrow(stringDF), 0)

  subjects <- unique(as.character(con_df$person_id))
  treated <- unique(as.character(stringDF$person_id))
  untreated <- setdiff(subjects, treated)

  expect_gt(length(treated), 0)
  expect_gt(length(untreated), 0)
})

test_that("fast ARTEMIS pipeline runs on mock CDM", {
  testthat::skip_if_not_installed("duckdb")

  connectionDetails <- ARTEMIS::createMockArtemisConnectionDetails()
  df_json <- ARTEMIS::loadCohort()
  valid_drugs <- ARTEMIS::loadDrugs()
  regimens <- ARTEMIS::loadRegimens(condition = "all")

  cohort_name <- paste0("mock_pipeline_", as.integer(Sys.time()))
  con_df <- ARTEMIS::getConDF(
    connectionDetails = connectionDetails,
    json = df_json$json[1],
    name = cohort_name,
    cdmSchema = "main",
    writeSchema = "main"
  )
  stringDF <- ARTEMIS::stringDF_from_cdm(con_df = con_df, validDrugs = valid_drugs)
  stringDF <- ARTEMIS::filter_stringDF(stringDF = stringDF, min = 1)

  set.seed(123)
  if (nrow(stringDF) > 60) {
    keep <- sample(stringDF$person_id, size = 60, replace = FALSE)
    stringDF <- stringDF[stringDF$person_id %in% keep, , drop = FALSE]
  }
  regimens <- regimens[seq_len(min(40, nrow(regimens))), , drop = FALSE]

  output_all <- ARTEMIS::generateRawAlignments(
    stringDF = stringDF,
    regimens = regimens,
    g = 0.4,
    Tfac = 0.4,
    verbose = 0,
    mem = -1,
    method = "PropDiff"
  )
  expect_gt(nrow(output_all), 0)

  processedAll <- ARTEMIS::processAlignments(
    rawOutput = output_all,
    regimens = regimens,
    regimenCombine = 28
  )
  expect_true(all(c("personID", "component", "adjustedS") %in% names(processedAll)))

  processedEras <- ARTEMIS::calculateEras(processedAll = processedAll, discontinuationTime = 120)
  regStats <- ARTEMIS::generateRegimenStats(processedEras = processedEras)
  expect_gt(nrow(regStats), 0)
})

test_that("writeOutputs smoke test runs on mock CDM sample", {
  testthat::skip_if_not_installed("duckdb")
  testthat::skip_if_not_installed("webshot")
  if (!webshot::is_phantomjs_installed()) {
    testthat::skip("phantomjs not installed; skipping writeOutputs smoke test")
  }

  connectionDetails <- ARTEMIS::createMockArtemisConnectionDetails()
  df_json <- ARTEMIS::loadCohort()
  valid_drugs <- ARTEMIS::loadDrugs()
  regimens <- ARTEMIS::loadRegimens(condition = "all")
  regGroups <- ARTEMIS::loadGroups()

  cohort_name <- paste0("mock_outputs_", as.integer(Sys.time()))
  con_df <- ARTEMIS::getConDF(
    connectionDetails = connectionDetails,
    json = df_json$json[1],
    name = cohort_name,
    cdmSchema = "main",
    writeSchema = "main"
  )
  stringDF <- ARTEMIS::stringDF_from_cdm(con_df = con_df, validDrugs = valid_drugs)
  stringDF <- ARTEMIS::filter_stringDF(stringDF = stringDF, min = 1)

  set.seed(456)
  if (nrow(stringDF) > 10) {
    keep <- sample(stringDF$person_id, size = 10, replace = FALSE)
    stringDF <- stringDF[stringDF$person_id %in% keep, , drop = FALSE]
  }
  regimens <- regimens[seq_len(min(20, nrow(regimens))), , drop = FALSE]

  output_all <- ARTEMIS::generateRawAlignments(
    stringDF = stringDF,
    regimens = regimens,
    g = 0.4,
    Tfac = 0.4,
    verbose = 0,
    mem = -1,
    method = "PropDiff"
  )
  processedAll <- ARTEMIS::processAlignments(rawOutput = output_all, regimens = regimens, regimenCombine = 28)
  processedEras <- ARTEMIS::calculateEras(processedAll = processedAll, discontinuationTime = 120)
  regStats <- ARTEMIS::generateRegimenStats(processedEras = processedEras)

  zip_files <- file.path(here::here(), c("output_data.zip", "output_stats.zip", "output_plots.zip"))
  on.exit(unlink(zip_files, force = TRUE), add = TRUE)

  ARTEMIS::writeOutputs(
    output_all = output_all,
    processedAll = processedAll,
    processedEras = processedEras,
    regGroups = regGroups,
    regStats = regStats,
    connectionDetails = connectionDetails,
    cdmSchema = "main",
    con_df = con_df,
    stringDF = stringDF,
    skipSummaryStats = FALSE
  )

  expect_true(all(file.exists(zip_files)))
})

test_that("mock CDM generator is deterministic for fixed seed", {
  testthat::skip_if_not_installed("duckdb")

  source(file.path("data-raw", "build_mock_artemis_cdm.R"))
  path_a <- tempfile(pattern = "mockA_", fileext = ".duckdb")
  path_b <- tempfile(pattern = "mockB_", fileext = ".duckdb")

  on.exit(unlink(c(path_a, path_b), force = TRUE), add = TRUE)

  build_mock_artemis_cdm(output_path = path_a, n_person = 250, seed = 42, run_validation = FALSE)
  build_mock_artemis_cdm(output_path = path_b, n_person = 250, seed = 42, run_validation = FALSE)

  get_metrics <- function(path) {
    con <- DBI::dbConnect(duckdb::duckdb(path, read_only = TRUE))
    on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)
    DBI::dbGetQuery(con, "
      SELECT
        (SELECT COUNT(*) FROM person) AS n_person,
        (SELECT COUNT(*) FROM condition_occurrence) AS n_condition_occurrence,
        (SELECT COUNT(*) FROM drug_exposure) AS n_drug_exposure,
        (SELECT SUM(drug_concept_id) FROM drug_exposure) AS sum_drug_concept_id,
        (SELECT SUM(person_id) FROM drug_exposure) AS sum_person_id
    ")
  }

  expect_equal(get_metrics(path_a), get_metrics(path_b))
})
