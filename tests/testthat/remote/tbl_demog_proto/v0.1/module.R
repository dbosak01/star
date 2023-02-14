library(dplyr)
library(tibble)
library(tidyr)
library(common)
library(reporter)

pth <- dirname(Sys.path())

# Get Data ----------------------------------------------------------------



# Intialize ards
init_ards(studyid = "abc",
          tableid = "01", adsns = c("adsl", "advs"),
          population = "safety population",
          time = "SCREENING", where = "saffl = TRUE")

# Prepare data
dat <- mtcars
dat$trt <- c(rep("A", 16), rep("B", 16))


# Perform Analysis --------------------------------------------------------



# Perform mpg analysis
mpgdf <- dat |>
    select(mpg, trt) |>
    group_by(trt) |>
    summarize(n = n(),
              mean = mean(mpg),
              std = sd(mpg),
              median = median(mpg),
              min = min(mpg),
              max = max(mpg)) |>
    mutate(analvar = "mpg") |>
    ungroup() |>
    add_ards(statvars = v(n, mean, std, median, min, max),
             statdesc = c("N", "Mean", "Std", "Median", "Min", "Max"),
             anal_var = "mpg", trtvar = "trt") |>
    transmute(analvar, trt,
              n = sprintf("%d", n),
              mean_sd = sprintf("%.1f (%.2f)", mean, std),
              median = sprintf("%.1f", median),
              min_max = sprintf("%.1f-%.1f", min, max)) |>
    pivot_longer(c(n, mean_sd, median, min_max),
                 names_to = "label", values_to = "stats") |>
    pivot_wider(names_from = trt,
                values_from = c(stats)) |>
    transmute(analvar, label = c("N", "Mean (Std)", "Median", "Min-Max"),
              trtA = A, trtB = B)

mpgdf



# Get population counts
trt_pop <- count(dat, trt) |> deframe()

# Perform cyl analysis
cyldf <- dat |>
    mutate(denom = trt_pop[paste0(dat$trt)]) |>
    group_by(cyl, trt, denom) |>
    summarize(cnt = n()) |>
    mutate(analvar = "cyl", label = paste(cyl, "Cylinder"),  pct = denom / cnt) |>
    ungroup() |>
    add_ards(statvars = v(cnt, denom, pct), statdesc = "label",
             anal_var = "cyl", trtvar = "trt") |>
    pivot_wider(names_from = trt,
                values_from = c(cnt, pct)) |>
    transmute(analvar, label,
              trtA = sprintf("%1d (%5.2f%%)", cnt_A, pct_A),
              trtB = sprintf("%1d (%5.2f%%)", cnt_B, pct_B),)

cyldf


# Combine analyses
final <- bind_rows(mpgdf, cyldf)
final

# Get ards
ards <- get_ards()
ards


# Write out files
saveRDS(final, file.path(pth, "output", "final.rds"))
saveRDS(ards, file.path(pth, "output", "ards.rds"))

# Generate Report ---------------------------------------------------------



# Create report
tbl <- create_table(final) |>
    titles("Table " %p% tableNum, "MTCARS Sample Table") |>
    footnotes("Motor Trend 1972")

print(pth)
rpth <- file.path(pth, "output", reportName)
print(rpth)

rpt <- create_report(rpth,
                     output_type = "RTF", font = 'Arial') |>
    add_content(tbl)

write_report(rpt)


