# =====================================================================
# 06  Attempt with seven students (Section 9.3)
#     One instance (seed 1), B = 2, baseline courses.
#     Only the removal of dominated bids is run here. Building the
#     obedience constraints for this instance needed more than 19 GB of
#     memory on a computer with 8 GB (read from the macOS Activity
#     Monitor) and was aborted after more than 45 minutes.
# =====================================================================
source("cb_core.R")

inst <- make_instance(n = 7, m = 3, q = c(2, 1, 1), B = 2, k = 2, seed = 1)

invisible(gc(reset = TRUE))
t0 <- Sys.time()
g  <- reduce_game(inst)
t_red <- as.numeric(difftime(Sys.time(), t0, units = "secs"))

cat("Full game:              ", 10^7, "profiles\n")
cat("After removing dominated bids:", g$P, "profiles\n")
cat("Ratio to median for six students:", round(g$P / 413343, 1), "\n")
cat("Time for the reduction:  ", round(t_red, 1), "s\n")
print(gc())   # column "max used": peak memory of R during the reduction

# The next step is NOT run here, as it exceeds the memory of an 8 GB computer:
# ob <- obedience_matrix(g)