# =====================================================================
# 01  Baseline instance (Chapters 5, 6 and 8)
#     n = 4, m = 3, q = (2,1,1), B = 3, k = 2, seed 7
#     Runtime: about 1 minute (plus about 1 minute for CHECK_FULL)
# =====================================================================
source("cb_core.R")
SOLVER      <- "highs"
CHECK_FULL  <- TRUE    # also solve the full (unreduced) game, Section 8.1.1
RUN_LPSOLVE <- TRUE   # time the welfare-maximizing CE with lpSolve, Section 8.1.2 (slow)

inst <- make_instance()
cat("Value matrix V (Table 4):\n"); print(round(inst$V, 2))

# --- Chapter 6: planner with and without seat guarantee --------------
pl  <- planner_lp(inst, solver = SOLVER)
plg <- planner_lp(inst, solver = SOLVER, guarantee = TRUE)
cat("\nPlanner z* =", round(pl$z, 4), "\n");  print(pl$X)
cat("\nPlanner with seat guarantee z*_G =", round(plg$z, 4), "\n"); print(plg$X)

# --- Section 8.1.1: game and removal of strictly dominated bids ------
g_full <- build_game(inst)
t0     <- Sys.time()
g      <- reduce_game(inst, g_full)       # exact: same CE as the full game
t_red  <- as.numeric(difftime(Sys.time(), t0, units = "secs"))
ob     <- obedience_matrix(g)
cat("\nBids per student (full):   ", g_full$sizes, "  profiles:", g_full$P,
    "  obedience constraints:", sum(g_full$sizes * (g_full$sizes - 1)),
    "\nBids per student (reduced):", g$sizes, "  profiles:", g$P,
    "  obedience constraints:", ob$A$nrow, "  nonzeros:", length(ob$A$v),
    "\nTime for the reduction:", round(t_red, 2), "s\n")
cat("Remaining bids per student:\n")
for (i in seq_along(g$acts))
  cat("  Student", i, ":", apply(g$acts[[i]], 1, function(b) paste0("(", paste(b, collapse = ","), ")")), "\n")

# --- Sections 8.2 to 8.4: equilibria ---------------------------------
W     <- rowSums(g$U)
ne    <- pure_ne(g)
ce_hi <- ce_lp(g, ob, "welfare_max", solver = SOLVER)
ce_lo <- ce_lp(g, ob, "welfare_min", solver = SOLVER)
ce_mm <- ce_lp(g, ob, "maximin",     solver = SOLVER)
ce_ml <- ce_lp(g, ob, "welfare_max", solver = SOLVER,      # highest welfare among maximin CE
               min_util = min(ce_mm$utilities) - 1e-7)
sec   <- security_levels(g_full)   # guarantee against every bid of the others (full game)

# --- Summary table (Table 6 in Section 8.2) --------------------------
row <- function(name, u, time = NA)
  data.frame(benchmark = name, welfare = sum(u), efficiency = sum(u) / pl$z,
             u1 = u[1], u2 = u[2], u3 = u[3], u4 = u[4], min_u = min(u), seconds = time)
ne_best <- ne[which.max(W[ne])]
ne_fair <- ne[which.max(apply(g$U[ne, , drop = FALSE], 1, min))]
tab <- rbind(
  row("Planner",                     pl$utilities,  pl$time),
  row("Planner with seat guarantee", plg$utilities, plg$time),
  row("Welfare-maximizing CE",       ce_hi$utilities, ce_hi$time),
  row("Maximin CE",                  ce_ml$utilities, ce_mm$time + ce_ml$time),
  row("Welfare-minimizing CE",       ce_lo$utilities, ce_lo$time),
  row("Best pure NE (welfare)",      g$U[ne_best, ]),
  row("Fairest pure NE (min u)",     g$U[ne_fair, ]))
cat("\n"); print(tab, digits = 4, row.names = FALSE)

cat("\nNumber of pure NE:", length(ne), "\n")
for (b in ne) cat("  ", describe_profile(g, b), "  u =", round(g$U[b, ], 3),
                  "  W =", round(W[b], 4), "\n")
cat("Best CE minus best pure NE (welfare):", signif(ce_hi$welfare - W[ne_best], 3), "\n")
cat("Cost of fairness (12): Delta eta =",
    round(ce_hi$welfare / pl$z - ce_ml$welfare / pl$z, 4), "\n")
cat("Security levels (full game):", round(sec["value", ], 4), "\n")

# profiles with the largest probabilities in each CE (for Section 8.3)
show_support <- function(name, r, top = 8) {
  s <- r$support[order(-r$pi[r$support])][seq_len(min(top, length(r$support)))]
  cat("\n", name, " (", length(r$support), " profiles with positive probability)\n", sep = "")
  for (b in s) cat(sprintf("  %.3f  ", r$pi[b]), describe_profile(g, b),
                   "  u =", round(g$U[b, ], 2), "\n")
}
show_support("Welfare-maximizing CE", ce_hi)
show_support("Maximin CE",            ce_ml)
show_support("Welfare-minimizing CE", ce_lo)

# --- Section 8.1.2: verification -------------------------------------
cat("\nChecks (all should be TRUE):\n")
cat(" obedience satisfied     :", all(sapply(list(ce_hi, ce_lo, ce_ml),
                                   function(r) ce_violation(ob, r$pi) < 1e-8)), "\n")
cat(" every pure NE is a CE   :", all(sapply(ne, function(b) {
                                   pi <- numeric(g$P); pi[b] <- 1; ce_violation(ob, pi) < 1e-12 })), "\n")
cat(" best CE >= best NE      :", ce_hi$welfare >= W[ne_best] - 1e-9, "\n")
cat(" CE >= security levels   :", all(sapply(list(ce_hi, ce_lo, ce_ml),
                                   function(r) all(r$utilities >= sec["value", ] - 1e-8))), "\n")
# Example 3.1 (Chicken): welfare-maximizing CE must equal 21/2 (Section 4.2.2)
g2  <- list(sizes = c(2, 2), P = 4, stride = c(1, 2),
            prof = unname(as.matrix(expand.grid(1:2, 1:2))),
            U = cbind(c(6, 7, 2, 0), c(6, 2, 7, 0)))
cat(" Example 3.1 gives 21/2  :",
    abs(ce_lp(g2, obedience_matrix(g2), "welfare_max", solver = SOLVER)$welfare - 10.5) < 1e-8, "\n")

if (CHECK_FULL) {   # the full game must give the same values (Section 8.1.1)
  ob_full <- obedience_matrix(g_full)
  f_hi <- ce_lp(g_full, ob_full, "welfare_max", solver = SOLVER)
  f_lo <- ce_lp(g_full, ob_full, "welfare_min", solver = SOLVER)
  cat(" full game, same best CE :", abs(f_hi$welfare - ce_hi$welfare) < 1e-6,
      sprintf("(full game: %.1f s)", f_hi$time), "\n")
  cat(" full game, same worst CE:", abs(f_lo$welfare - ce_lo$welfare) < 1e-6, "\n")
  f_mm <- ce_lp(g_full, ob_full, "maximin", solver = SOLVER)
  cat(" full game, same maximin :", abs(min(f_mm$utilities) - min(ce_mm$utilities)) < 1e-6, "\n")
}

if (RUN_LPSOLVE) {  # solver comparison (Section 8.1.2)
  r_lp <- ce_lp(g, ob, "welfare_max", solver = "lpsolve")
  cat("\nWelfare-maximizing CE: HiGHS", round(ce_hi$time, 2), "s, lpSolve",
      round(r_lp$time, 2), "s, same value:", abs(r_lp$welfare - ce_hi$welfare) < 1e-6, "\n")
}

write.csv(tab, "results/baseline_benchmarks.csv", row.names = FALSE)
cat("\nSaved: results/baseline_benchmarks.csv\n")
