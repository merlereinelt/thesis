# =====================================================================
# Course-bidding game: instance, planner LP, game construction, CE LPs
# Solver-agnostic core. Solvers: "glpk" (Rglpk), "highs", "lpsolve".
# =====================================================================

suppressPackageStartupMessages({
  library(slam)      # simple_triplet_matrix
})

# ---------------------------------------------------------------------
# 1. Instance generation
# ---------------------------------------------------------------------
make_instance <- function(n = 4, m = 3, q = c(2, 1, 1), B = 3, k = 2,
                          seed = 7, V = NULL, vmin = 1, vmax = 10) {
  if (is.null(V)) {
    set.seed(seed)
    V <- matrix(runif(n * m, vmin, vmax), n, m)   # column-major, as in Table 4
  }
  list(n = n, m = m, q = q,
       B = rep(B, length.out = n), k = rep(k, length.out = n), V = V)
}

# ---------------------------------------------------------------------
# 2. Planner LP (problem (4), with seat guarantee (13))
# ---------------------------------------------------------------------
planner_lp <- function(inst, solver = "highs", guarantee = FALSE) {
  # guarantee = TRUE adds sum_j x_ij >= 1 for every student (seat guarantee, Section 6.2)
  n <- inst$n; m <- inst$m
  nv <- n * m                                   # x_ij, index (j-1)*n + i
  idx <- function(i, j) (j - 1) * n + i
  rows_i <- rows_j <- vals <- c(); r <- 0
  for (i in 1:n) { r <- r + 1
    for (j in 1:m) { rows_i <- c(rows_i, r); rows_j <- c(rows_j, idx(i, j)); vals <- c(vals, 1) } }
  for (j in 1:m) { r <- r + 1
    for (i in 1:n) { rows_i <- c(rows_i, r); rows_j <- c(rows_j, idx(i, j)); vals <- c(vals, 1) } }
  dir <- rep("<=", r); rhs <- c(inst$k, inst$q)
  if (guarantee) {
    if (sum(inst$q) < n) return(list(z = NA, X = NULL, utilities = rep(NA, n), time = NA))
    for (i in 1:n) { r <- r + 1
      for (j in 1:m) { rows_i <- c(rows_i, r); rows_j <- c(rows_j, idx(i, j)); vals <- c(vals, 1) } }
    dir <- c(dir, rep(">=", n)); rhs <- c(rhs, rep(1, n))
  }
  A <- simple_triplet_matrix(rows_i, rows_j, vals, nrow = r, ncol = nv)
  sol <- solve_lp(obj = as.vector(inst$V), A = A, dir = dir, rhs = rhs,
                  ub = rep(1, nv), max = TRUE, solver = solver)
  X <- matrix(sol$x, n, m)
  list(z = sol$obj, X = X, utilities = rowSums(inst$V * X), time = sol$time)
}

# ---------------------------------------------------------------------
# 3. Action sets (eq. (14)) and payoffs (eqs. (15), (16))
# ---------------------------------------------------------------------
action_set <- function(m, B, k) {
  grid <- as.matrix(expand.grid(rep(list(0:B), m)))
  ok <- rowSums(grid) <= B & rowSums(grid > 0) <= k
  A <- grid[ok, , drop = FALSE]
  dimnames(A) <- NULL
  A[order(rowSums(A > 0), -A %*% (B + 1)^((m - 1):0)), , drop = FALSE]
}

build_game <- function(inst, acts = NULL) {
  n <- inst$n; m <- inst$m
  if (is.null(acts)) acts <- lapply(1:n, function(i) action_set(m, inst$B[i], inst$k[i]))
  sizes <- sapply(acts, nrow)
  P <- prod(sizes)
  # profile index -> action index per player (first player varies fastest)
  prof <- as.matrix(expand.grid(lapply(sizes, seq_len)))
  dimnames(prof) <- NULL
  U <- matrix(0, P, n)
  for (j in 1:m) {
    bids <- matrix(sapply(1:n, function(i) acts[[i]][prof[, i], j]), nrow = P)   # P x n
    for (i in 1:n) {
      bi <- bids[, i]
      H  <- rowSums(bids > bi)
      Tn <- rowSums(bids == bi)
      p  <- ifelse(bi == 0, 0,
            ifelse(H >= inst$q[j], 0,
            ifelse(H + Tn <= inst$q[j], 1, (inst$q[j] - H) / Tn)))
      U[, i] <- U[, i] + inst$V[i, j] * p
    }
  }
  list(acts = acts, sizes = sizes, prof = prof, U = U, P = P,
       stride = cumprod(c(1, sizes))[1:n])
}

# ---------------------------------------------------------------------
# 3b. Iterated elimination of strictly dominated bids (pure strategies).
#     A CE never puts probability on a strictly dominated action, so the
#     CE set (and every LP value) of the reduced game equals the original.
#     Returns the reduced game; $kept lists surviving rows of each action set.
# ---------------------------------------------------------------------
reduce_game <- function(inst, game = build_game(inst)) {
  n <- length(game$sizes)
  Tn <- lapply(1:n, function(i) array(game$U[, i], dim = game$sizes))
  alive <- lapply(game$sizes, seq_len)
  repeat {
    changed <- FALSE
    for (i in 1:n) {
      sub <- do.call(`[`, c(list(Tn[[i]]), alive, list(drop = FALSE)))
      Ui  <- matrix(aperm(sub, c(i, setdiff(1:n, i))), nrow = length(alive[[i]]))
      keep <- rep(TRUE, nrow(Ui))
      for (r in seq_len(nrow(Ui))) for (s in seq_len(nrow(Ui)))
        if (s != r && keep[s] && all(Ui[r, ] < Ui[s, ])) { keep[r] <- FALSE; break }
      if (!all(keep)) { alive[[i]] <- alive[[i]][keep]; changed <- TRUE }
    }
    if (!changed) break
  }
  red <- build_game(inst, acts = Map(function(a, k) a[k, , drop = FALSE], game$acts, alive))
  red$kept <- alive
  red
}

# ---------------------------------------------------------------------
# 4. Obedience constraints (eq. (7)) in sparse triplet form
#    Row (i, r, s), r != s:  sum_{b: b_i = r} pi(b) [u_i(r,b_-i) - u_i(s,b_-i)] >= 0
#    Optional pruning of zero coefficients keeps the matrix small.
# ---------------------------------------------------------------------
obedience_matrix <- function(game, drop_zero = TRUE) {
  n <- length(game$sizes); P <- game$P
  ri <- list(); ci <- list(); vv <- list(); row <- 0
  meta <- list()
  for (i in 1:n) {
    Ai <- game$sizes[i]; st <- game$stride[i]
    for (r in 1:Ai) {
      cols <- which(game$prof[, i] == r)
      ur <- game$U[cols, i]
      for (s in 1:Ai) if (s != r) {
        dev <- cols + (s - r) * st
        d <- ur - game$U[dev, i]
        keep <- if (drop_zero) d != 0 else rep(TRUE, length(d))
        row <- row + 1
        ri[[row]] <- rep(row, sum(keep)); ci[[row]] <- cols[keep]; vv[[row]] <- d[keep]
        meta[[row]] <- c(i, r, s)
      }
    }
  }
  list(A = simple_triplet_matrix(c(integer(0), unlist(ri)), c(integer(0), unlist(ci)), c(numeric(0), unlist(vv)), nrow = row, ncol = P),
       meta = do.call(rbind, meta))
}

# ---------------------------------------------------------------------
# 5. Generic LP wrapper
# ---------------------------------------------------------------------
solve_lp <- function(obj, A, dir, rhs, max = TRUE, ub = NULL, lb = NULL,
                     solver = "glpk", free_last = FALSE) {
  t0 <- Sys.time()
  nv <- length(obj)
  if (solver == "glpk") {
    bounds <- NULL
    if (!is.null(ub) || free_last) {
      bounds <- list()
      if (!is.null(ub)) bounds$upper <- list(ind = seq_len(nv), val = ub)
      if (free_last) bounds$lower <- list(ind = nv, val = -Inf)
    }
    res <- Rglpk::Rglpk_solve_LP(obj, A, dir, rhs, bounds = bounds, max = max)
    out <- list(x = res$solution, obj = res$optimum, status = res$status)
  } else if (solver == "highs") {
    lhs_h <- ifelse(dir == ">=", rhs, ifelse(dir == "==", rhs, -Inf))
    rhs_h <- ifelse(dir == "<=", rhs, ifelse(dir == "==", rhs, Inf))
    lower <- rep(0, nv); if (free_last) lower[nv] <- -Inf
    upper <- if (is.null(ub)) rep(Inf, nv) else ub
    Am <- Matrix::sparseMatrix(i = A$i, j = A$j, x = A$v, dims = c(A$nrow, A$ncol))
    res <- highs::highs_solve(L = obj, lower = lower, upper = upper, A = Am,
                              lhs = lhs_h, rhs = rhs_h, maximum = max)
    out <- list(x = res$primal_solution, obj = res$objective_value, status = res$status)
  } else if (solver == "lpsolve") {
    stopifnot(!free_last)
    dense <- cbind(A$i, A$j, A$v)
    if (!is.null(ub)) { # add x <= ub as rows
      k0 <- A$nrow
      dense <- rbind(dense, cbind(k0 + seq_len(nv), seq_len(nv), 1))
      dir <- c(dir, rep("<=", nv)); rhs <- c(rhs, ub)
    }
    res <- lpSolve::lp(if (max) "max" else "min", obj, dense.const = dense,
                       const.dir = dir, const.rhs = rhs)
    out <- list(x = res$solution, obj = res$objval, status = res$status)
  }
  out$time <- as.numeric(difftime(Sys.time(), t0, units = "secs"))
  out
}

# ---------------------------------------------------------------------
# 6. CE LPs: welfare max (9), welfare min, maximin (10)
# ---------------------------------------------------------------------
ce_lp <- function(game, obed, objective = c("welfare_max", "welfare_min", "maximin"),
                  solver = "glpk", min_util = NULL) {
  objective <- match.arg(objective)
  P <- game$P; n <- ncol(game$U)
  A <- obed$A
  W <- rowSums(game$U)
  if (objective %in% c("welfare_max", "welfare_min")) {
    A_all <- rbind(A, simple_triplet_matrix(rep(1, P), 1:P, rep(1, P), 1, P))
    dir <- c(rep(">=", A$nrow), "==")
    rhs <- c(rep(0, A$nrow), 1)
    # optional fairness floor: u_i(pi) >= min_util  (used for the frontier)
    if (!is.null(min_util)) {
      Ut <- as.simple_triplet_matrix(t(game$U))
      A_all <- rbind(A_all, Ut); dir <- c(dir, rep(">=", n)); rhs <- c(rhs, rep(min_util, n))
    }
    sol <- solve_lp(W, A_all, dir, rhs, max = objective == "welfare_max", solver = solver)
    pi <- sol$x
  } else {
    # variables (pi, t);  u_i(pi) - t >= 0
    A0 <- simple_triplet_matrix(A$i, A$j, A$v, nrow = A$nrow, ncol = P + 1)
    Ut <- as.simple_triplet_matrix(cbind(t(game$U), -1))
    sumrow <- simple_triplet_matrix(rep(1, P), 1:P, rep(1, P), 1, P + 1)
    A_all <- rbind(A0, Ut, sumrow)
    dir <- c(rep(">=", A$nrow), rep(">=", n), "==")
    rhs <- c(rep(0, A$nrow), rep(0, n), 1)
    sol <- solve_lp(c(rep(0, P), 1), A_all, dir, rhs, max = TRUE, solver = solver,
                    free_last = solver != "lpsolve")
    pi <- sol$x[1:P]
  }
  pi[abs(pi) < 1e-12] <- 0
  list(pi = pi, welfare = sum(pi * W), utilities = colSums(pi * game$U),
       support = which(pi > 1e-9), status = sol$status, time = sol$time)
}

# maximal violation of the obedience constraints (should be ~0)
ce_violation <- function(obed, pi) {
  lhs <- as.vector(slam::matprod_simple_triplet_matrix(obed$A, matrix(pi, ncol = 1)))
  max(0, -min(lhs))
}

# ---------------------------------------------------------------------
# 7. Pure Nash equilibria by enumeration
# ---------------------------------------------------------------------
pure_ne <- function(game) {
  n <- ncol(game$U); P <- game$P
  is_ne <- rep(TRUE, P)
  for (i in 1:n) {
    st <- game$stride[i]; Ai <- game$sizes[i]
    base <- (seq_len(P) - 1) - (game$prof[, i] - 1) * st     # index with a_i = 1, 0-based
    best <- rep(-Inf, P)
    for (s in 1:Ai) best <- pmax(best, game$U[base + (s - 1) * st + 1, i])
    is_ne <- is_ne & (game$U[, i] >= best - 1e-12)
  }
  which(is_ne)
}

# ---------------------------------------------------------------------
# 8. Security (maxmin) levels: max_{a_i} min_{a_-i} u_i(a_i, a_-i)
#    Every CE gives player i at least this value (lower bound on u_i(pi)).
# ---------------------------------------------------------------------
security_levels <- function(game) {
  sapply(seq_along(game$sizes), function(i) {
    worst <- tapply(game$U[, i], game$prof[, i], min)
    c(value = max(worst), action = which.max(worst))
  })
}

# ---------------------------------------------------------------------
# 9. Efficiency-fairness frontier: max W(pi) s.t. pi in CE, u_i(pi) >= t
# ---------------------------------------------------------------------
ce_frontier <- function(game, obed, t_grid, solver = "glpk") {
  do.call(rbind, lapply(t_grid, function(t) {
    r <- ce_lp(game, obed, "welfare_max", solver = solver, min_util = t)
    c(t = t, welfare = r$welfare, r$utilities, time = r$time)
  }))
}

describe_profile <- function(game, idx) {
  sapply(seq_along(game$acts), function(i)
    paste0("(", paste(game$acts[[i]][game$prof[idx, i], ], collapse = ","), ")"))
}
