# test URL
slide_url <- getCatalog("provgigapath") |>
    dplyr::filter(
        level == "slide_level" &
        filename == paste0(
            "TCGA-OR-A5JJ-01Z-00-DX1.",
            "459B5DFE-47B1-426F-B009-7664C1B6FEEC.csv.gz"
        )
    ) |>
    getFileURLs()

pg <- ProvGiga(slide_url, tumorType = "TCGA_ACC")
expect_inherits(pg, "ProvGiga")
expect_equal(pg@level, "slide_level")

# import
data <- import(pg)
expect_inherits(data, "data.frame")
expect_equal(nrow(data), 1)
expect_true("tumorType" %in% names(data))

# check output
output <- capture.output(show(pg))
expect_true(any(grepl("TCGA_ACC", output)))
