#!/usr/bin/env Rscript

resolve_repo_root <- function() {
  if (file.exists("DESCRIPTION") && dir.exists("R")) {
    return(normalizePath(".", winslash = "/", mustWork = TRUE))
  }
  if (file.exists("../DESCRIPTION") && dir.exists("../R")) {
    return(normalizePath("..", winslash = "/", mustWork = TRUE))
  }
  stop("Could not locate repo root. Run this script from the ARTEMIS root or data-raw/.")
}

sample_date <- function(n, start_date, end_date) {
  as.Date(sample(seq.Date(start_date, end_date, by = "day"), size = n, replace = TRUE))
}

to_posix_num <- function(x) {
  as.numeric(as.POSIXct(x, tz = "UTC"))
}

extract_default_cohort_concept_ids <- function(json_string) {
  cohort_json <- jsonlite::fromJSON(json_string, simplifyVector = FALSE)
  items <- cohort_json$ConceptSets[[1]]$expression$items
  unique(vapply(items, function(x) as.numeric(x$concept$CONCEPT_ID), numeric(1)))
}

create_person_table <- function(n_person, source_person, seed) {
  set.seed(seed + 1L)
  person_id <- seq_len(n_person)
  gender <- sample(c(8507L, 8532L), n_person, replace = TRUE, prob = c(0.52, 0.48))
  yob <- sample(1940:2002, n_person, replace = TRUE)
  mob <- sample(1:12, n_person, replace = TRUE)
  dob <- sample(1:28, n_person, replace = TRUE)
  birth_date <- as.Date(sprintf("%04d-%02d-%02d", yob, mob, dob))

  race_pool <- unique(stats::na.omit(source_person$race_concept_id))
  ethnicity_pool <- unique(stats::na.omit(source_person$ethnicity_concept_id))
  if (length(race_pool) == 0) {
    race_pool <- 8527L
  }
  if (length(ethnicity_pool) == 0) {
    ethnicity_pool <- 38003563L
  }

  data.frame(
    person_id = person_id,
    gender_concept_id = gender,
    year_of_birth = yob,
    month_of_birth = mob,
    day_of_birth = dob,
    birth_datetime = to_posix_num(birth_date),
    race_concept_id = sample(race_pool, n_person, replace = TRUE),
    ethnicity_concept_id = sample(ethnicity_pool, n_person, replace = TRUE),
    location_id = NA_integer_,
    provider_id = NA_integer_,
    care_site_id = NA_integer_,
    person_source_value = paste0("mock_person_", person_id),
    gender_source_value = ifelse(gender == 8532L, "F", "M"),
    gender_source_concept_id = gender,
    race_source_value = NA_character_,
    race_source_concept_id = NA_integer_,
    ethnicity_source_value = NA_character_,
    ethnicity_source_concept_id = NA_integer_,
    stringsAsFactors = FALSE
  )
}

create_condition_occurrence_table <- function(person, source_condition, primary_condition_id, seed) {
  set.seed(seed + 2L)
  n_person <- nrow(person)
  person_id <- person$person_id

  index_date <- sample_date(
    n_person,
    start_date = as.Date("2018-01-01"),
    end_date = as.Date("2023-12-31")
  )

  background_ids <- unique(source_condition$condition_concept_id)
  background_ids <- background_ids[background_ids != primary_condition_id]
  if (length(background_ids) < 5) {
    stop("Not enough background condition IDs in source data.")
  }

  type_pool <- unique(stats::na.omit(source_condition$condition_type_concept_id))
  status_pool <- unique(stats::na.omit(source_condition$condition_status_concept_id))
  if (length(type_pool) == 0) {
    type_pool <- 32817L
  }

  primary_rows <- data.frame(
    person_id = person_id,
    condition_concept_id = primary_condition_id,
    condition_start_date = index_date,
    condition_end_date = index_date + sample(60:180, n_person, replace = TRUE),
    condition_type_concept_id = sample(type_pool, n_person, replace = TRUE),
    condition_status_concept_id = if (length(status_pool) > 0) sample(status_pool, n_person, replace = TRUE) else NA_integer_,
    stringsAsFactors = FALSE
  )

  n_background <- sample(0:2, n_person, replace = TRUE, prob = c(0.35, 0.45, 0.20))
  bg_list <- vector("list", length = n_person)
  for (i in seq_len(n_person)) {
    if (n_background[i] == 0) {
      next
    }
    start_dates <- index_date[i] + sample(-365:120, n_background[i], replace = TRUE)
    bg_list[[i]] <- data.frame(
      person_id = rep.int(person_id[i], n_background[i]),
      condition_concept_id = sample(background_ids, n_background[i], replace = TRUE),
      condition_start_date = start_dates,
      condition_end_date = start_dates + sample(1:30, n_background[i], replace = TRUE),
      condition_type_concept_id = sample(type_pool, n_background[i], replace = TRUE),
      condition_status_concept_id = if (length(status_pool) > 0) sample(status_pool, n_background[i], replace = TRUE) else NA_integer_,
      stringsAsFactors = FALSE
    )
  }

  out <- do.call(rbind, c(list(primary_rows), bg_list[!vapply(bg_list, is.null, logical(1))]))
  out <- out[order(out$person_id, out$condition_start_date, out$condition_concept_id), ]
  out$condition_occurrence_id <- seq_len(nrow(out))
  out$condition_start_datetime <- to_posix_num(out$condition_start_date)
  out$condition_end_datetime <- to_posix_num(out$condition_end_date)
  out$stop_reason <- NA_character_
  out$provider_id <- NA_integer_
  out$visit_occurrence_id <- NA_integer_
  out$visit_detail_id <- NA_integer_
  out$condition_source_value <- as.character(out$condition_concept_id)
  out$condition_source_concept_id <- out$condition_concept_id
  out$condition_status_source_value <- NA_character_

  out[, c(
    "condition_occurrence_id", "person_id", "condition_concept_id",
    "condition_start_date", "condition_start_datetime",
    "condition_end_date", "condition_end_datetime", "condition_type_concept_id",
    "condition_status_concept_id", "stop_reason", "provider_id", "visit_occurrence_id",
    "visit_detail_id", "condition_source_value", "condition_source_concept_id",
    "condition_status_source_value"
  )]
}

build_drug_pool <- function(concept, concept_ancestor, valid_ids) {
  ingredient <- concept[tolower(concept$concept_class_id) == "ingredient", c("concept_id", "concept_name")]
  colnames(ingredient) <- c("ingredient_id", "ingredient_name")
  mapping <- merge(concept_ancestor, ingredient, by.x = "ancestor_concept_id", by.y = "ingredient_id")
  mapping <- unique(mapping[, c("descendant_concept_id", "ancestor_concept_id", "ingredient_name")])
  colnames(mapping) <- c("drug_concept_id", "ingredient_id", "ingredient_name")

  mapping$is_valid <- mapping$ingredient_id %in% valid_ids
  valid_pool <- unique(mapping$drug_concept_id[mapping$is_valid])
  invalid_pool <- unique(mapping$drug_concept_id[!mapping$is_valid])

  list(mapping = mapping, valid_pool = valid_pool, invalid_pool = invalid_pool)
}

create_drug_exposure_table <- function(person, condition_occurrence, source_drug, pools, seed) {
  set.seed(seed + 3L)
  n_person <- nrow(person)
  person_id <- person$person_id

  groups <- sample(
    c("treated", "mixed", "untreated"),
    size = n_person,
    replace = TRUE,
    prob = c(0.60, 0.20, 0.20)
  )

  treated_preferred <- c(1344905L, 1378382L, 45775965L)
  treated_pool <- unique(c(treated_preferred[treated_preferred %in% pools$valid_pool], pools$valid_pool))
  treated_pool <- treated_pool[!is.na(treated_pool)]
  untreated_pool <- pools$invalid_pool
  untreated_pool <- untreated_pool[!is.na(untreated_pool)]

  if (length(treated_pool) < 3) {
    stop("Not enough treated drug concepts available.")
  }
  if (length(untreated_pool) < 10) {
    stop("Not enough untreated drug concepts available.")
  }

  type_pool <- unique(stats::na.omit(source_drug$drug_type_concept_id))
  if (length(type_pool) == 0) {
    type_pool <- 38000177L
  }

  index_by_person <- tapply(condition_occurrence$condition_start_date, condition_occurrence$person_id, min)
  de_list <- vector("list", length = n_person)

  for (i in seq_len(n_person)) {
    pid <- person_id[i]
    idx <- as.Date(index_by_person[as.character(pid)])
    if (is.na(idx)) {
      idx <- as.Date("2020-01-01")
    }

    rows <- list()
    add_row <- function(drug_concept_id, start_date, days_supply = 1L) {
      data.frame(
        person_id = pid,
        drug_concept_id = as.integer(drug_concept_id),
        drug_exposure_start_date = as.Date(start_date),
        drug_exposure_end_date = as.Date(start_date) + as.integer(max(1L, days_supply)),
        drug_type_concept_id = as.integer(sample(type_pool, 1)),
        days_supply = as.integer(days_supply),
        stringsAsFactors = FALSE
      )
    }

    if (groups[i] == "treated") {
      n_lines <- sample(1:2, 1, prob = c(0.7, 0.3))
      line_start <- idx + sample(0:60, 1)
      for (line in seq_len(n_lines)) {
        cycle <- sample(c(21L, 28L), 1)
        n_cycles <- sample(4:8, 1)
        pattern <- sample(c("pembro", "carbo_pacli", "triplet"), 1, prob = c(0.35, 0.35, 0.30))
        if (pattern == "pembro") {
          regimen_drugs <- c(45775965L)
        } else if (pattern == "carbo_pacli") {
          regimen_drugs <- c(1344905L, 1378382L)
        } else {
          regimen_drugs <- c(45775965L, 1344905L, 1378382L)
        }
        regimen_drugs <- regimen_drugs[regimen_drugs %in% treated_pool]
        if (length(regimen_drugs) == 0) {
          regimen_drugs <- sample(treated_pool, 2)
        }
        for (cy in 0:(n_cycles - 1)) {
          dose_day <- line_start + (cy * cycle) + sample(-1:1, 1)
          for (drug in regimen_drugs) {
            rows[[length(rows) + 1L]] <- add_row(drug, dose_day, days_supply = 1L)
          }
        }
        line_start <- line_start + (n_cycles * cycle) + sample(14:35, 1)
      }
    } else if (groups[i] == "mixed") {
      cycle <- sample(c(21L, 28L), 1)
      n_cycles <- sample(3:6, 1)
      regimen_drugs <- sample(treated_pool, 2)
      tx_start <- idx + sample(0:90, 1)
      for (cy in 0:(n_cycles - 1)) {
        dose_day <- tx_start + (cy * cycle) + sample(-1:1, 1)
        for (drug in regimen_drugs) {
          rows[[length(rows) + 1L]] <- add_row(drug, dose_day, days_supply = 1L)
        }
      }
      supp_n <- sample(5:10, 1)
      supp_days <- sort(tx_start + sample(-120:240, supp_n, replace = TRUE))
      supp_drugs <- sample(untreated_pool, supp_n, replace = TRUE)
      for (j in seq_len(supp_n)) {
        rows[[length(rows) + 1L]] <- add_row(supp_drugs[j], supp_days[j], days_supply = sample(1:5, 1))
      }
    } else {
      n_exp <- sample(8:15, 1)
      exp_days <- sort(idx + sample(-120:360, n_exp, replace = TRUE))
      exp_drugs <- sample(untreated_pool, n_exp, replace = TRUE)
      for (j in seq_len(n_exp)) {
        rows[[length(rows) + 1L]] <- add_row(exp_drugs[j], exp_days[j], days_supply = sample(1:10, 1))
      }
    }

    de_list[[i]] <- do.call(rbind, rows)
  }

  out <- do.call(rbind, de_list)
  out <- out[order(out$person_id, out$drug_exposure_start_date, out$drug_concept_id), ]
  out$drug_exposure_id <- seq_len(nrow(out))
  out$drug_exposure_start_datetime <- to_posix_num(out$drug_exposure_start_date)
  out$drug_exposure_end_datetime <- to_posix_num(out$drug_exposure_end_date)
  out$verbatim_end_date <- as.Date(NA)
  out$stop_reason <- NA_character_
  out$refills <- NA_integer_
  out$quantity <- NA_real_
  out$sig <- NA_character_
  out$route_concept_id <- NA_integer_
  out$lot_number <- NA_character_
  out$provider_id <- NA_integer_
  out$visit_occurrence_id <- NA_integer_
  out$visit_detail_id <- NA_integer_
  out$drug_source_value <- as.character(out$drug_concept_id)
  out$drug_source_concept_id <- out$drug_concept_id
  out$route_source_value <- NA_character_
  out$dose_unit_source_value <- NA_character_

  out[, c(
    "drug_exposure_id", "person_id", "drug_concept_id", "drug_exposure_start_date",
    "drug_exposure_start_datetime", "drug_exposure_end_date", "drug_exposure_end_datetime",
    "verbatim_end_date", "drug_type_concept_id", "stop_reason", "refills", "quantity",
    "days_supply", "sig", "route_concept_id", "lot_number", "provider_id",
    "visit_occurrence_id", "visit_detail_id", "drug_source_value",
    "drug_source_concept_id", "route_source_value", "dose_unit_source_value"
  )]
}

add_atc_mappings <- function(concept, concept_ancestor, drug_exposure, invalid_pool) {
  atc_concepts <- data.frame(
    concept_id = c(199900001L, 199900002L, 199900003L),
    concept_name = c("Antineoplastic adjuncts", "Analgesics and antipyretics", "Systemic antibacterials"),
    domain_id = "Drug",
    vocabulary_id = "ATC",
    concept_class_id = "ATC 1st",
    standard_concept = "C",
    concept_code = c("L01X", "N02B", "J01C"),
    valid_start_date = as.numeric(as.POSIXct("1970-01-01", tz = "UTC")),
    valid_end_date = as.numeric(as.POSIXct("2099-12-31", tz = "UTC")),
    invalid_reason = NA_character_,
    stringsAsFactors = FALSE
  )

  concept_aug <- unique(rbind(concept, atc_concepts))
  invalid_used <- sort(unique(drug_exposure$drug_concept_id[drug_exposure$drug_concept_id %in% invalid_pool]))
  if (length(invalid_used) == 0) {
    return(list(concept = concept_aug, concept_ancestor = concept_ancestor))
  }

  assigned_atc <- atc_concepts$concept_id[((seq_along(invalid_used) - 1L) %% nrow(atc_concepts)) + 1L]
  atc_map <- data.frame(
    ancestor_concept_id = assigned_atc,
    descendant_concept_id = invalid_used,
    min_levels_of_separation = 1L,
    max_levels_of_separation = 1L,
    stringsAsFactors = FALSE
  )

  ca_aug <- unique(rbind(concept_ancestor, atc_map))
  list(concept = concept_aug, concept_ancestor = ca_aug)
}

validate_mock_cdm <- function(repo_root, db_path, n_person_expected) {
  source(file.path(repo_root, "R", "inputOutput.R"), local = .GlobalEnv)
  load(file.path(repo_root, "data", "validdrugs.rda"))
  load(file.path(repo_root, "data", "df_json.rda"))

  connectionDetails <- DatabaseConnector::createConnectionDetails(
    dbms = "duckdb",
    server = db_path
  )
  cdmSchema <- "main"
  writeSchema <- "main"
  cohort_name <- paste0("mock_lung_", format(Sys.time(), "%Y%m%d%H%M%S"))

  con_df <- getConDF(
    connectionDetails = connectionDetails,
    json = df_json$json[1],
    name = cohort_name,
    cdmSchema = cdmSchema,
    writeSchema = writeSchema
  )
  if (nrow(con_df) == 0) {
    stop("Validation failed: getConDF returned zero rows.")
  }

  stringDF <- stringDF_from_cdm(con_df = con_df, validDrugs = validdrugs)
  if (nrow(stringDF) == 0) {
    stop("Validation failed: stringDF_from_cdm returned zero rows.")
  }

  subjects <- unique(as.character(con_df$person_id))
  treated <- unique(as.character(stringDF$person_id))
  untreated <- setdiff(subjects, treated)
  if (length(treated) == 0 || length(untreated) == 0) {
    stop("Validation failed: treated/untreated split is empty.")
  }

  con <- DBI::dbConnect(duckdb::duckdb(db_path, read_only = TRUE))
  on.exit({
    if (!is.null(con) && DBI::dbIsValid(con)) {
      DBI::dbDisconnect(con, shutdown = TRUE)
    }
  }, add = TRUE)
  n_person <- DBI::dbGetQuery(con, "SELECT COUNT(*) AS n FROM person;")$n[[1]]
  if (n_person != n_person_expected) {
    stop("Validation failed: person row count mismatch. Expected ", n_person_expected, ", got ", n_person)
  }

  missing_drug_links <- DBI::dbGetQuery(con, "
    SELECT COUNT(*) AS n
    FROM (
      SELECT DISTINCT d.drug_concept_id
      FROM drug_exposure d
      LEFT JOIN concept_ancestor ca
        ON d.drug_concept_id = ca.descendant_concept_id
      LEFT JOIN concept c
        ON ca.ancestor_concept_id = c.concept_id
      WHERE LOWER(c.concept_class_id) = 'ingredient'
      GROUP BY d.drug_concept_id
      HAVING COUNT(*) = 0
    ) x;
  ")$n[[1]]
  if (missing_drug_links > 0) {
    stop("Validation failed: some drug_exposure rows have no ingredient mapping.")
  }

  DBI::dbDisconnect(con, shutdown = TRUE)
  con <- NULL

  cleanup_tables <- c(
    cohort_name,
    paste0(cohort_name, "_inclusion"),
    paste0(cohort_name, "_inclusion_result"),
    paste0(cohort_name, "_inclusion_stats"),
    paste0(cohort_name, "_summary_stats"),
    paste0(cohort_name, "_censor_stats"),
    paste0(cohort_name, "_checksum")
  )
  cleanup_con <- DBI::dbConnect(duckdb::duckdb(db_path))
  on.exit({
    if (!is.null(cleanup_con) && DBI::dbIsValid(cleanup_con)) {
      DBI::dbDisconnect(cleanup_con, shutdown = TRUE)
    }
  }, add = TRUE)
  for (tbl in cleanup_tables) {
    quoted <- DBI::dbQuoteIdentifier(cleanup_con, tbl)
    DBI::dbExecute(cleanup_con, paste0("DROP TABLE IF EXISTS ", quoted, ";"))
  }

  list(
    n_person = n_person,
    n_con_df_rows = nrow(con_df),
    n_con_df_subjects = length(subjects),
    n_stringdf_subjects = length(treated),
    n_untreated_subjects = length(untreated)
  )
}

build_mock_artemis_cdm <- function(
    output_path = NULL,
    n_person = 2000L,
    seed = 20260306L,
    run_validation = TRUE) {
  repo_root <- resolve_repo_root()
  if (is.null(output_path)) {
    output_path <- file.path(repo_root, "inst", "extdata", "mockArtemisCdm.duckdb")
  }
  output_path <- normalizePath(output_path, winslash = "/", mustWork = FALSE)

  source_sqlite_path <- file.path(repo_root, "inst", "extdata", "testing_db.sqlite")
  if (!file.exists(source_sqlite_path)) {
    stop("Missing source SQLite file: ", source_sqlite_path)
  }

  source_con <- DBI::dbConnect(RSQLite::SQLite(), source_sqlite_path)
  on.exit(DBI::dbDisconnect(source_con), add = TRUE)

  concept <- DBI::dbReadTable(source_con, "concept")
  concept$valid_start_date <- as.numeric(concept$valid_start_date)
  concept$valid_end_date <- as.numeric(concept$valid_end_date)
  concept_ancestor <- DBI::dbReadTable(source_con, "concept_ancestor")
  source_person <- DBI::dbReadTable(source_con, "person")
  source_observation_period <- DBI::dbReadTable(source_con, "observation_period")
  source_condition <- DBI::dbReadTable(source_con, "condition_occurrence")
  source_drug <- DBI::dbReadTable(source_con, "drug_exposure")
  source_cdm_source <- DBI::dbReadTable(source_con, "cdm_source")

  load(file.path(repo_root, "data", "validdrugs.rda"))
  load(file.path(repo_root, "data", "df_json.rda"))

  default_cohort_ids <- extract_default_cohort_concept_ids(df_json$json[1])
  compatible_ids <- intersect(default_cohort_ids, concept$concept_id)
  if (length(compatible_ids) == 0) {
    stop("No default cohort concept IDs found in source concept table.")
  }
  primary_condition_id <- if (37166577L %in% compatible_ids) 37166577L else compatible_ids[[1]]

  person <- create_person_table(
    n_person = n_person,
    source_person = source_person,
    seed = seed
  )

  period_type_pool <- unique(stats::na.omit(source_observation_period$period_type_concept_id))
  if (length(period_type_pool) == 0) {
    period_type_pool <- 32817L
  }
  observation_period <- data.frame(
    observation_period_id = seq_len(n_person),
    person_id = person$person_id,
    observation_period_start_date = as.Date("2015-01-01"),
    observation_period_end_date = as.Date("2024-12-31"),
    period_type_concept_id = sample(period_type_pool, n_person, replace = TRUE),
    stringsAsFactors = FALSE
  )

  condition_occurrence <- create_condition_occurrence_table(
    person = person,
    source_condition = source_condition,
    primary_condition_id = primary_condition_id,
    seed = seed
  )

  pools <- build_drug_pool(
    concept = concept,
    concept_ancestor = concept_ancestor,
    valid_ids = unique(validdrugs$valid_concept_id)
  )

  drug_exposure <- create_drug_exposure_table(
    person = person,
    condition_occurrence = condition_occurrence,
    source_drug = source_drug,
    pools = pools,
    seed = seed
  )

  atc_aug <- add_atc_mappings(
    concept = concept,
    concept_ancestor = concept_ancestor,
    drug_exposure = drug_exposure,
    invalid_pool = pools$invalid_pool
  )
  concept <- atc_aug$concept
  concept_ancestor <- atc_aug$concept_ancestor

  cdm_source <- source_cdm_source[rep(1, 1), , drop = FALSE]
  cdm_source$cdm_source_name <- "ARTEMIS mock CDM (DuckDB)"
  cdm_source$cdm_source_abbreviation <- "mockArtemisCdm"
  cdm_source$source_description <- "Deterministic synthetic OMOP CDM for ARTEMIS demos and tests."
  cdm_source$cdm_etl_reference <- "data-raw/build_mock_artemis_cdm.R"
  cdm_source$source_release_date <- as.Date("2026-03-06")
  cdm_source$cdm_release_date <- as.Date("2026-03-06")

  if (file.exists(output_path)) {
    file.remove(output_path)
  }
  dir.create(dirname(output_path), recursive = TRUE, showWarnings = FALSE)

  target_con <- DBI::dbConnect(duckdb::duckdb(output_path))
  on.exit(DBI::dbDisconnect(target_con, shutdown = TRUE), add = TRUE)

  DBI::dbWriteTable(target_con, "cdm_source", cdm_source, overwrite = TRUE)
  DBI::dbWriteTable(target_con, "concept", concept, overwrite = TRUE)
  DBI::dbWriteTable(target_con, "concept_ancestor", concept_ancestor, overwrite = TRUE)
  DBI::dbWriteTable(target_con, "person", person, overwrite = TRUE)
  DBI::dbWriteTable(target_con, "observation_period", observation_period, overwrite = TRUE)
  DBI::dbWriteTable(target_con, "condition_occurrence", condition_occurrence, overwrite = TRUE)
  DBI::dbWriteTable(target_con, "drug_exposure", drug_exposure, overwrite = TRUE)

  summary <- list(
    output_path = output_path,
    n_person = nrow(person),
    n_condition_occurrence = nrow(condition_occurrence),
    n_drug_exposure = nrow(drug_exposure),
    n_concept = nrow(concept),
    n_concept_ancestor = nrow(concept_ancestor)
  )

  if (isTRUE(run_validation)) {
    validation <- validate_mock_cdm(
      repo_root = repo_root,
      db_path = output_path,
      n_person_expected = nrow(person)
    )
    summary <- c(summary, validation)
  }

  print(summary)
  invisible(summary)
}

if (sys.nframe() == 0) {
  suppressPackageStartupMessages({
    library(DBI)
    library(RSQLite)
    library(duckdb)
    library(jsonlite)
    library(DatabaseConnector)
    library(CohortGenerator)
    library(CirceR)
    library(SqlRender)
    library(magrittr)
    library(dplyr)
    library(lubridate)
    library(cli)
    library(stringr)
  })
  build_mock_artemis_cdm()
}
