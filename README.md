# Linear Programming and Correlated Equilibria for Strategic Course Bidding

R code for my bachelor thesis at the University of St. Gallen.
The code computes the planner's optimal assignment and the correlated equilibria of a stylized
course-bidding game as linear programs, and reproduces all numerical results, tables and figures
of Chapters 6, 8 and 9.

## Requirements
- R 4.4.1
- Packages: `highs`, `Matrix`, `slam`, `ggplot2`
  ```r
  install.packages(c("highs", "Matrix", "slam", "ggplot2"))
  ```

## Files
| File | Content | Used in |
|---|---|---|
| `cb_core.R` | All functions: instances, planner LP, bidding game, removal of dominated bids, obedience constraints, CE LPs, pure NE. Loaded by the other scripts. | – |
| `01_baseline.R` | Baseline instance (seed 7): planner with and without seat guarantee, all equilibria, check on the full game | Tables 4–6, Chapters 6 and 8 |
| `02_robustness.R` | 50 value matrices with the baseline parameters | Table 7, Section 9.1 |
| `03_scaling.R` | Varying the number of students, the number of seats and the bid budget | Sections 9.2 and 9.3 |
| `04_figures.R` | Figures from `results/scaling.csv` | Figures 2–5 |
| `06_seven_students.R` | Single attempt with seven students | Section 9.3 |
| `run_all.R` | Runs scripts 02 to 04 one after the other | – |
| `results/` | Output of the scripts as CSV files | – |
| `figures/` | Figures as PDF | – |

## How to reproduce
1. Open the folder as an RStudio project (or set it as the working directory).
2. Run `01_baseline.R` (about 1 minute, plus up to 1 minute for the check on the full game).
3. Run `run_all.R` for Chapter 9. `03_scaling.R` takes several hours because of the instances
   with six students.
4. `06_seven_students.R` needs far more than 8 GB of memory and is included only to document
   the attempt described in Section 9.3.

All scripts use the solver HiGHS and fixed seeds, so the results are identical on every run;
only the measured computation times differ between machines.
Results are saved after every instance, so an interrupted run loses nothing.

## Use of AI
The code was developed with the assistance of Claude (Anthropic). All code was checked,
run and verified by the author.

## Software used
- R Core Team (2024). R: A language and environment for statistical computing (Version 4.4.1).
  R Foundation for Statistical Computing.
- Huangfu, Q. & Hall, J. A. J. (2018). Parallelizing the dual revised simplex method.
  *Mathematical Programming Computation*, 10(1), 119–142. (HiGHS)
- Schwendinger, F. & Schumacher, D. (2026). highs: 'HiGHS' optimization solver
  (Version 1.12.0-3). CRAN.
- Wickham, H. (2016). ggplot2: Elegant graphics for data analysis. Springer.
