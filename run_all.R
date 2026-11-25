# Run from the package root with Rscript run_all.R.
required_inputs <- file.path("inputs", c(
  "income_definition_panels.rds", "corrected_core_tables.csv",
  "total_fitted_shortfall_contrasts.csv", "reconstructed_panel.rds", "audit_input.rds"
))
missing_inputs <- required_inputs[!file.exists(required_inputs)]
if (length(missing_inputs)) {
  stop("Missing prepared inputs: ", paste(missing_inputs, collapse = ", "))
}

dir.create("results/tables", recursive = TRUE, showWarnings = FALSE)
dir.create("results/figures", recursive = TRUE, showWarnings = FALSE)
source("code/paper_corrected_tables_20261031.R")
writeLines(capture.output(sessionInfo()), "results/session_info.txt")
