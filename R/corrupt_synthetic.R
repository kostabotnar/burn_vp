corrupt_error_types <- c(
  "range", "hematocrit_fraction", "fluid_total", "fluid_not_monotonic", "sap_below_dap",
  "ne_gating", "burn_after_icu", "burn_outside_period", "death_alive", "duplicate_id",
  "invalid_binary", "identifier"
)

corrupt_text <- function(x) {
  if (inherits(x, "Date")) format(x, "%Y-%m-%d") else as.character(x)
}

corrupt_cycle <- function(options, k) options[[(k - 1) %% length(options) + 1]]

# Same id scheme as qc_position_id() in R/qc_rules.R, which lintr does not see from this file.
corrupt_rule_id <- function(prefix, codebook, name) {
  qc_position_id(prefix, codebook, name) # nolint: object_usage_linter.
}

# One entry per error type: `eligible` marks rows the error can be placed in, and
# `make(row, k, picks)` returns the column, the new value and the QC rule expected to detect
# the k-th injection. `picks` holds every row chosen for that type.
corrupt_specs <- function(data, codebook) {
  n <- nrow(data)
  all_rows <- rep(TRUE, n)
  present <- function(col) !is.na(data[[col]])
  change <- function(col, value, rule) list(col = col, value = value, rule = rule)
  list(
    range = list(eligible = all_rows, make = function(row, k, picks) {
      choice <- corrupt_cycle(list(list("age_years", 150), list("map_24h_mmhg", 400),
                                   list("peak_lactate_0_24h_mmol_l", 45)), k)
      change(choice[[1]], choice[[2]], corrupt_rule_id("RNG", codebook, choice[[1]]))
    }),
    hematocrit_fraction = list(
      eligible = present("hematocrit_0_24h_pct"),
      make = function(row, k, picks) {
        change("hematocrit_0_24h_pct", data$hematocrit_0_24h_pct[row] / 100, "UNI_001")
      }
    ),
    fluid_total = list(eligible = present("total_fluids_24h_ml"), make = function(row, k, picks) {
      change("total_fluids_24h_ml", data$total_fluids_24h_ml[row] + 500, "FLU_006")
    }),
    fluid_not_monotonic = list(
      eligible = (data$ra_24h_ml > 10) %in% TRUE,
      make = function(row, k, picks) {
        change("ra_48h_ml", round(data$ra_24h_ml[row] / 2), "FLU_010")
      }
    ),
    sap_below_dap = list(eligible = present("dap_24h_mmhg"), make = function(row, k, picks) {
      change("sap_24h_mmhg", data$dap_24h_mmhg[row] - 10, "HEM_003")
    }),
    ne_gating = list(eligible = data$ne_24h %in% "No", make = function(row, k, picks) {
      change("peak_ne_24h_ug_kg_min", 0.15, "VAS_004")
    }),
    burn_after_icu = list(eligible = present("icu_admission_date"), make = function(row, k, picks) {
      change("burn_date", data$icu_admission_date[row] + 2, "DAT_002")
    }),
    burn_outside_period = list(eligible = all_rows, make = function(row, k, picks) {
      change("burn_date", as.Date("2014-12-31") - 30 * (k - 1), "DAT_001")
    }),
    death_alive = list(
      eligible = data$hospital_mortality %in% "Yes",
      make = function(row, k, picks) change("vital_status_follow_up", "Alive", "OUT_002")
    ),
    # The copied ID comes from a row that keeps its own ID, so the duplicate survives.
    duplicate_id = list(eligible = all_rows, make = function(row, k, picks) {
      source_row <- setdiff(seq_len(n), picks)[k]
      change("patient_id", data$patient_id[source_row], "DUP_001")
    }),
    invalid_binary = list(eligible = all_rows, make = function(row, k, picks) {
      col <- corrupt_cycle(c("diabetes", "inhalation_injury", "sepsis"), k)
      change(col, "Maybe", corrupt_rule_id("CAT", codebook, col))
    }),
    identifier = list(eligible = all_rows, make = function(row, k, picks) {
      choice <- corrupt_cycle(list(c("01019012345", "IDN_001"), c("123-45-6789", "IDN_002"),
                                   c("jane.doe@example.com", "IDN_003"),
                                   c("+47 912 34 567", "IDN_004")), k)
      change(corrupt_cycle(c("covid19_status", "burn_mechanism"), k), choice[1], choice[2])
    })
  )
}

# Injects known errors into clean synthetic data so QC detection can be measured. Each row
# receives at most one injected error, so every (row, column) cell is changed at most once.
# The attribute "injected" lists each change and the QC rule expected to detect it.
corrupt_synthetic <- function(data, codebook, n_per_type = 3, seed = 1) {
  if (!is.numeric(n_per_type) || length(n_per_type) != 1 || is.na(n_per_type) ||
        n_per_type < 1 || n_per_type != round(n_per_type)) {
    stop("'n_per_type' must be a single positive whole number.", call. = FALSE)
  }
  needed_rows <- n_per_type * (length(corrupt_error_types) + 1)
  if (nrow(data) < needed_rows) {
    stop(sprintf("'data' needs at least %d rows for n_per_type = %d.", needed_rows, n_per_type),
         call. = FALSE)
  }
  specs <- corrupt_specs(data, codebook)
  injected <- list()
  used <- rep(FALSE, nrow(data))
  withr::with_seed(
    seed, {
      for (type in corrupt_error_types) {
        pool <- which(specs[[type]]$eligible & !used)
        picks <- pool[sample.int(length(pool), min(n_per_type, length(pool)))]
        for (k in seq_along(picks)) {
          row <- picks[k]
          ch <- specs[[type]]$make(row, k, picks)
          injected[[length(injected) + 1]] <- data.frame(
            row = row, column = ch$col, original = corrupt_text(data[[ch$col]][row]),
            corrupted = corrupt_text(ch$value), error_type = type, expected_rule_id = ch$rule,
            stringsAsFactors = FALSE
          )
          data[[ch$col]][row] <- ch$value
          used[row] <- TRUE
        }
      }
    },
    .rng_kind = "Mersenne-Twister", .rng_normal_kind = "Inversion",
    .rng_sample_kind = "Rejection"
  )
  attr(data, "injected") <- do.call(rbind, injected)
  data
}
