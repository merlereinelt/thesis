# =====================================================================
# 05  Solver comparison HiGHS vs lpSolve (Section 8.1.2)
#     Same LP (welfare-maximizing CE), same code, only the solver changes.
#     lpSolve runs in a separate R process that is stopped after
#     LIMIT_MIN minutes, so the script always finishes.
#     Needs the package callr: install.packages("callr")
# =====================================================================
source("cb_core.R")
LIMIT_MIN <- 60          # time limit for lpSolve in minutes

cases <- list(
  list(name = "Baseline, reduced game", n = 4, B = 3, reduce = TRUE),
  list(name = "Baseline, full game",    n = 4, B = 3, reduce = FALSE),
  list(name = "Five students, B = 2",   n = 5, B = 2, reduce = TRUE))

# builds the game and solves the welfare-maximizing CE with one solver
solve_case <- function(wd, n, B, reduce, solver) {
  setwd(wd); source("cb_core.R")
  inst <- make_instance(n = n, m = 3, q = c(2, 1, 1), B = B, k = 2, seed = if (n == 4) 7 else 1)
  g  <- if (reduce) reduce_game(inst) else build_game(inst)
  ob <- obedience_matrix(g)
  r  <- ce_lp(g, ob, "welfare_max", solver = solver)
  c(profiles = g$P, welfare = r$welfare, seconds = r$time)
}

res <- NULL
for (cs in cases) {
  cat("\n", cs$name, "\n", sep = "")
  h <- solve_case(getwd(), cs$n, cs$B, cs$reduce, "highs")
  cat("  HiGHS  :", round(h["seconds"], 2), "s   (", h["profiles"], "profiles )\n")
  cat("  lpSolve started at", format(Sys.time(), "%H:%M:%S"), "\n")
  l <- tryCatch(
    callr::r(solve_case, args = list(getwd(), cs$n, cs$B, cs$reduce, "lpsolve"),
             timeout = LIMIT_MIN * 60),
    error = function(e) c(profiles = h[["profiles"]], welfare = NA, seconds = NA))
  if (is.na(l["seconds"])) cat("  lpSolve: not finished within", LIMIT_MIN, "minutes\n")
  else cat("  lpSolve:", round(l["seconds"], 2), "s   same value:",
           abs(l["welfare"] - h["welfare"]) < 1e-6, "\n")
  res <- rbind(res, data.frame(case = cs$name, profiles = h[["profiles"]],
                               highs_s = h[["seconds"]], lpsolve_s = l[["seconds"]],
                               limit_min = LIMIT_MIN))
  write.csv(res, "results/solver_comparison.csv", row.names = FALSE)   # saved after every case
}
cat("\nSaved: results/solver_comparison.csv\n")
print(res, row.names = FALSE)
