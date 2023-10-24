library(rmarkdown)
Sys.setenv(RSTUDIO_PANDOC = "C:/Program Files/RStudio/bin/pandoc-2.2.1")

setwd("C:/Users/raymart.biasbas/Documents/Proyekto/LD Segmentation/LD Player Segmentation")
rmarkdown::render("LDPlayerSegmentation.Rmd", params = list(symbol='IBM'))


library(logr)

# Open the log
log_open("LDSegmentation.log")

# Print text to the log
log_print("LDSegmentationData")

# Print a dataframe to the log
log_print(ld_full)

# Close the log
log_close()