# =====================================================================
# 03  Scaling experiments (Section 9.2)
#     One factor at a time, 50 seeds per configuration
# =====================================================================
source("cb_core.R")
SOLVER <- "highs"
SEEDS  <- 1:50

configs <- c(
  # 9.2.1 number of students (B = 2 allows n up to 6)
  lapply(2:6, function(n) list(series = "students_B2", n = n, m = 3, q = c(2, 1, 1), B = 2)),
  lapply(2:4, function(n) list(series = "students_B3", n = n, m = 3, q = c(2, 1, 1), B = 3)),
  # 9.2.2 course capacities at n = 4
  lapply(list(c(2, 1, 1), c(2, 2, 1), c(2, 2, 2), c(3, 2, 2)),
         function(q) list(series = "capacity", n = 4, m = 3, q = q, B = 3)),
  # bid budget
  lapply(1:3, function(B) list(series = "budget", n = 4, m = 3, q = c(2, 1, 1), B = B)))

run_instance <- function(cf, seed) {
  inst <- make_instance(n = cf$n, m = cf$m, q = cf$q, B = cf$B, k = 2, seed = seed)
  pl   <- planner_lp(inst, solver = SOLVER)
  plg  <- planner_lp(inst, solver = SOLVER, guarantee = TRUE)
  A_full <- nrow(action_set(cf$m, cf$B, 2))^cf$n
  t0 <- Sys.time(); g <- reduce_game(inst); t_red <- as.numeric(difftime(Sys.time(), t0, units = "secs"))
  ob <- obedience_matrix(g)
  W  <- rowSums(g$U); ne <- pure_ne(g)
  hi <- ce_lp(g, ob, "welfare_max", solver = SOLVER)
  lo <- ce_lp(g, ob, "welfare_min", solver = SOLVER)
  mm <- ce_lp(g, ob, "maximin",     solver = SOLVER)
  ml <- ce_lp(g, ob, "welfare_max", solver = SOLVER, min_util = min(mm$utilities) - 1e-7)
  data.frame(series = cf$series, n = cf$n, m = cf$m, B = cf$B,
             q = paste(cf$q, collapse = "-"), Q = sum(cf$q), seed = seed,
             A_full = A_full, A_red = g$P, nnz = length(ob$A$v),
             z_lp = pl$z, z_guarantee = plg$z,
             planner_min = min(pl$utilities), planner_zero = sum(pl$utilities < 1e-9),
             W_best = hi$welfare, W_worst = lo$welfare, W_maximin = ml$welfare,
             t_maximin = min(mm$utilities), best_min = min(hi$utilities),
             n_ne = length(ne), W_bestNE = if (length(ne)) max(W[ne]) else NA,
             t_reduce = t_red, t_best = hi$time, t_mm = mm$time + ml$time)
}

res <- NULL
for (cf in configs) for (s in SEEDS) {
  cat(cf$series, " n =", cf$n, " q =", paste(cf$q, collapse = "-"), " B =", cf$B, " seed", s, "\n")
  res <- rbind(res, run_instance(cf, s))
  write.csv(res, "results/scaling.csv", row.names = FALSE)    # saved after every run
}
cat("Saved: results/scaling.csv\n")
