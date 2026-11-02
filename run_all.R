# =====================================================================
# Runs everything for Chapter 9 and the solver comparison in Section 8.1.2,
# one script after the other. Start it as an RStudio Background Job.
# Each script saves its results to results/ as it goes, so nothing is lost
# if the run is interrupted.
# =====================================================================
dir.create("results", showWarnings = FALSE)
dir.create("figures", showWarnings = FALSE)
for (f in c("02_robustness.R", "03_scaling.R", "04_figures.R", "05_solver_comparison.R")) {
  cat("\n================ ", f, " started at ", format(Sys.time(), "%H:%M:%S"), " ================\n", sep = "")
  source(f, echo = FALSE)
}
cat("\nAll done at", format(Sys.time(), "%H:%M:%S"), "\n")
