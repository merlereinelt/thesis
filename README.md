# Linear Programming and Correlated Equilibria for Strategic Course Bidding

R code for my bachelor thesis at the University of St. Gallen.
The code computes the planner's optimal assignment and the correlated equilibria of a stylized
course-bidding game as linear programs, and reproduces all tables and figures of Chapters 6, 8 and 9.

## Requirements
- R <version>
- Packages: `highs`, `Matrix`, `slam`, `ggplot2`
  ```r
  install.packages(c("highs", "Matrix", "slam", "ggplot2"))
  ```

## Files
| File | Content | Used in |
|---|---|---|
| `cb_core.R` | All functions: instances, planner LP, bidding game, removal of dominated bids, obedience constraints, CE LPs, pure NE, security levels. Loaded by the other scripts. | – |
| `01_baseline.R` | Baseline instance (seed 7): planner with and without seat guarantee, all equilibria, checks | Tables <4–6>, Chapters 6 and 8 |
| `02_robustness.R` | 50 value matrices with the baseline parameters | Table <7>, Section 9.1 |
| `03_scaling.R` | Varying the number of students, course capacities and the bid budget | Sections 9.2 and 9.3 |
| `04_figures.R` | Figures from `results/scaling.csv` | Figures <…> |
| `run_all.R` | Runs scripts 02 to 04 one after the other | – |
| `results/` | Output of the scripts as CSV files | – |
| `figures/` | Figures as PDF | – |

## How to reproduce
1. Open the folder as an RStudio project (or set it as the working directory).
2. Run `01_baseline.R` (about 1 minute; up to 1 more minute for the check on the full game).
3. Run `run_all.R` for Chapter 9. `03_scaling.R` takes several hours because of the instances with six students.

All scripts use the solver HiGHS and fixed seeds, so the results are identical on every run;
only the measured computation times differ between machines.
Results are saved after every instance, so an interrupted run loses nothing.

## Use of AI
The code was developed with the assistance of Claude (Anthropic). All code was checked,
run and verified by the author.

## Software used
R Core Team (<Jahr>). R: A language and environment for statistical computing.
Huangfu, Q. & Hall, J. A. J. (2018). Parallelizing the dual revised simplex method.
*Mathematical Programming Computation*, 10, 119–142. (HiGHS)# thesis
