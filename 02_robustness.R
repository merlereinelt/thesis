# =====================================================================
# 02  Robustness across value draws (Section 9.1)
#     Baseline parameters, 50 independently drawn value matrices
# =====================================================================
source("cb_core.R")
SOLVER <- "highs"
SEEDS  <- 1:50

one_seed <- function(seed) {
  inst <- make_instance(seed = seed)
  pl   <- planner_lp(inst, solver = SOLVER)
  plg  <- planner_lp(inst, solver = SOLVER, guarantee = TRUE)
  g    <- reduce_game(inst)
  ob   <- obedience_matrix(g)
  W    <- rowSums(g$U); ne <- pure_ne(g)
  hi   <- ce_lp(g, ob, "welfare_max", solver = SOLVER)
  lo   <- ce_lp(g, ob, "welfare_min", solver = SOLVER)
  mm   <- ce_lp(g, ob, "maximin",     solver = SOLVER)
  ml   <- ce_lp(g, ob, "welfare_max", solver = SOLVER, min_util = min(mm$utilities) - 1e-7)
  sec  <- security_levels(build_game(inst))["value", ]
  data.frame(seed = seed, z_lp = pl$z, z_guarantee = plg$z,
             planner_min = min(pl$utilities), planner_zero = sum(pl$utilities < 1e-9),
             W_best = hi$welfare, W_worst = lo$welfare, W_maximin = ml$welfare,
             t_maximin = min(mm$utilities), best_min = min(hi$utilities),
             worst_min = min(lo$utilities), n_ne = length(ne),
             W_bestNE = if (length(ne)) max(W[ne]) else NA,
             fairNE_min = if (length(ne)) max(apply(g$U[ne, , drop = FALSE], 1, min)) else NA,
             sec_min = min(sec), A_red = g$P,
             t_best = hi$time, t_mm = mm$time + ml$time)
}

res <- NULL
for (s in SEEDS) {
  cat("seed", s, "\n")
  res <- rbind(res, one_seed(s))
  write.csv(res, "results/robustness.csv", row.names = FALSE)   # saved after every seed
}

res$eff_best    <- res$W_best / res$z_lp
res$eff_worst   <- res$W_worst / res$z_lp
res$eff_maximin <- res$W_maximin / res$z_lp
res$best_vs_guarantee <- res$W_best / res$z_guarantee
write.csv(res, "results/robustness.csv", row.names = FALSE)

summ <- function(x) round(c(mean = mean(x, na.rm = TRUE), median = median(x, na.rm = TRUE),
                            min = min(x, na.rm = TRUE), max = max(x, na.rm = TRUE)), 4)
print(rbind(eff_best = summ(res$eff_best), eff_maximin = summ(res$eff_maximin),
            eff_worst = summ(res$eff_worst), best_vs_guarantee = summ(res$best_vs_guarantee)))
cat("Planner leaves a student without a course:", sum(res$planner_zero > 0), "of", nrow(res), "\n")
cat("Best CE equals guarantee planner (|diff| < 1e-3):",
    sum(abs(res$W_best - res$z_guarantee) < 1e-3, na.rm = TRUE), "\n")
cat("Rank of seed 7 (percentile of eff_best):",
    round(mean(res$eff_best <= res$eff_best[res$seed == 7]), 2), "\n")
cat("Maximin CE fairer than every pure NE:",
    sum(res$t_maximin > res$fairNE_min + 1e-6, na.rm = TRUE), "of",
    sum(!is.na(res$fairNE_min)), "seeds with a pure NE\n")
cat("Saved: results/robustness.csv\n")
