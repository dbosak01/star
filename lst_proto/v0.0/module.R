
library(common)
library(reporter)

pth <- dirname(Sys.path())

# Get Data ----------------------------------------------------------------

# Prepare data
dat <- mtcars


# Perform Analysis --------------------------------------------------------



# Write out files
saveRDS(dat, file.path(pth, "output", "final.rds"))


# Generate Report ---------------------------------------------------------



# Create report
tbl <- create_table(dat) |>
    titles("Listing " %p% listingNum, "MTCARS Listing") |>
    footnotes("Motor Trend 1972")

print(pth)
rpth <- file.path(pth, "output", reportName)
print(rpth)

rpt <- create_report(rpth,
                     output_type = "RTF", font = 'Arial') |>
    add_content(tbl)

write_report(rpt)


