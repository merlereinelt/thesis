# =====================================================================
# 04  Figures for Chapter 9 (reads results/scaling.csv from 03_scaling.R)
#     Saves PDF (for LaTeX) and PNG (preview) to figures/
# =====================================================================
suppressPackageStartupMessages(library(ggplot2))

d <- read.csv("results/scaling.csv")
d$eff_best  <- d$W_best    / d$z_lp
d$eff_mm    <- d$W_maximin / d$z_lp
d$eff_worst <- d$W_worst   / d$z_lp

pal <- c("#2a78d6", "#eb6834", "#1baf7a")
shp <- c(16, 17, 15)
theme_thesis <- theme_minimal(base_size = 11) +
  theme(panel.grid.minor = element_blank(), legend.position = "bottom",
        legend.title = element_blank(), strip.text = element_text(face = "bold"))

# mean and interquartile range across seeds
summ <- function(df, x, cols, labels) {
  out <- do.call(rbind, lapply(seq_along(cols), function(k) {
    a <- aggregate(df[[cols[k]]], list(series = df$series, x = df[[x]]),
                   function(v) c(mean = mean(v, na.rm = TRUE),
                                 lo = unname(quantile(v, .25, na.rm = TRUE)),
                                 hi = unname(quantile(v, .75, na.rm = TRUE))))
    data.frame(series = a$series, x = a$x, as.data.frame(a[[3]]), measure = labels[k])
  }))
  out$measure <- factor(out$measure, levels = labels)
  out
}
save_fig <- function(p, name, w = 8, h = 4) {
  ggsave(file.path("figures", paste0(name, ".pdf")), p, width = w, height = h)
  ggsave(file.path("figures", paste0(name, ".png")), p, width = w, height = h, dpi = 150, bg = "white")
}
dodge <- position_dodge(width = .25)   # avoids overlapping error bars
eff_plot <- function(e, xlab) {
  ggplot(e, aes(x, mean, colour = measure, shape = measure)) +
    geom_hline(yintercept = 1, linetype = "dashed", colour = "grey40", linewidth = .4) +
    geom_errorbar(aes(ymin = lo, ymax = hi), width = .15, linewidth = .4, position = dodge) +
    geom_line(linewidth = .7, position = dodge) + geom_point(size = 2.4, position = dodge) +
    scale_colour_manual(values = pal) + scale_shape_manual(values = shp) +
    scale_y_continuous(labels = function(v) paste0(round(100 * v), "%")) +
    labs(x = xlab, y = expression("Efficiency " * eta)) + theme_thesis
}
lv_eff <- c("Welfare-maximizing CE", "Maximin CE", "Welfare-minimizing CE")

# --- Figure: efficiency against number of students --------------------
st <- subset(d, series %in% c("students_B2", "students_B3"))
st$series <- ifelse(st$series == "students_B2", "Bid budget B = 2", "Bid budget B = 3")
p1 <- eff_plot(summ(st, "n", c("eff_best", "eff_mm", "eff_worst"), lv_eff), "Number of students n") +
  facet_wrap(~ series, scales = "free_x") + scale_x_continuous(breaks = 2:6)
save_fig(p1, "fig_efficiency_students")

# --- Figure: worst-off student against number of students -------------
lv_min <- c("Planner", "Welfare-maximizing CE", "Maximin CE")
u <- summ(st, "n", c("planner_min", "best_min", "t_maximin"), lv_min)
p2 <- ggplot(u, aes(x, mean, colour = measure, shape = measure)) +
  geom_errorbar(aes(ymin = lo, ymax = hi), width = .15, linewidth = .4, position = dodge) +
  geom_line(linewidth = .7, position = dodge) + geom_point(size = 2.4, position = dodge) +
  facet_wrap(~ series, scales = "free_x") + scale_x_continuous(breaks = 2:6) +
  scale_colour_manual(values = c("grey35", pal[1:2])) + scale_shape_manual(values = c(4, 16, 17)) +
  labs(x = "Number of students n", y = "Utility of the worst-off student") + theme_thesis
save_fig(p2, "fig_minutility_students")

# --- Figure: efficiency against total capacity ------------------------
ca <- subset(d, series == "capacity"); ca$series <- "Capacity"
p3 <- eff_plot(summ(ca, "Q", c("eff_best", "eff_mm", "eff_worst"), lv_eff),
               "Total number of seats Q (n = 4)") + scale_x_continuous(breaks = sort(unique(ca$Q)))
save_fig(p3, "fig_efficiency_capacity", w = 5, h = 4)

# --- Figure: LP size and solver time (B = 2 series), two separate panels
cb <- subset(d, series == "students_B2")
sz <- rbind(data.frame(n = cb$n, what = "Full game", v = cb$A_full),
            data.frame(n = cb$n, what = "After removing dominated bids", v = cb$A_red))
sz <- aggregate(v ~ n + what, sz, median)
sz$what <- factor(sz$what, levels = c("Full game", "After removing dominated bids"))
p4 <- ggplot(sz, aes(n, v, colour = what, shape = what)) +
  geom_line(linewidth = .7) + geom_point(size = 2.4) + scale_y_log10() +
  scale_colour_manual(values = pal[1:2]) + scale_shape_manual(values = shp[1:2]) +
  labs(x = "Number of students n", y = "Bid profiles = LP variables (log scale)") + theme_thesis
save_fig(p4, "fig_lpsize_students", w = 4.5, h = 4)

rt <- rbind(data.frame(n = cb$n, what = "Welfare-maximizing CE", v = cb$t_best),
            data.frame(n = cb$n, what = "Maximin CE", v = cb$t_mm))
rt <- aggregate(v ~ n + what, rt, median)
rt$what <- factor(rt$what, levels = c("Welfare-maximizing CE", "Maximin CE"))
p5 <- ggplot(rt, aes(n, v, colour = what, shape = what)) +
  geom_line(linewidth = .7) + geom_point(size = 2.4) + scale_y_log10() +
  scale_colour_manual(values = pal[1:2]) + scale_shape_manual(values = shp[1:2]) +
  labs(x = "Number of students n", y = "Solver time in seconds (log scale)") + theme_thesis
save_fig(p5, "fig_runtime_students", w = 4.5, h = 4)

cat("Saved figures to figures/\n")
