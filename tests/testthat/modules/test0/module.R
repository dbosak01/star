
library(oncvis)

dc1 <- oncvis::connect_to_data(data_source = "c:/Projects/Amgen/OncVis/oncvis-master/tests/testthat/testdata/sas/stat_data",
                               type        = oncvis::dctype$sas7bdat)

# Create a reporting object for adding Tables, Figures, and Listings
rpt1 <- oncvis::create_report(data_connection = dc1)

# Add a single TFL
rpt1 <- oncvis::add_tfl(report = rpt1,
                        tfl    = oncvis::tflname$t_disp,
                        filter = list(adsl = "saffl == 'Y'")
)

# Export the report to a folder
#oncvis::export(rpt1, output_folder = "/userdata/cfda/data_science/projects/oncvis/demo/output")

oncvis::export(rpt1, output_folder = "c:/Projects/Amgen/OncVis/oncvis-master/tests/testthat/results")
