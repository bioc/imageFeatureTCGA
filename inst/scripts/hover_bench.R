# timings for functions
library(imageFeatureTCGA)

hov_json_file <- getCatalog("hovernet", "json") |>
    dplyr::filter(
        filename == paste0(
            "TCGA-23-1121-01Z-00-DX1.",
            "E2F25441-32C3-46BF-A845-CB4FA787E8CB.json.gz"
        )
    ) |>
    getFileURLs()

dest_json <- file.path(tempdir(), basename(hov_json_file))
download.file(hov_json_file, destfile = dest_json)

# HoverJSON
microbenchmark::microbenchmark(
    HoverNet(dest_json) |> import(),
    times = 1L
)
## 40.55 seconds

## remove non-function code
source("~/gh/ImageAnalysisR/share_function/import_json.R")

# json_to_SpatialExperiment
microbenchmark::microbenchmark(
    json_to_SpatialExperiment(dest_json),
    times = 1L
)
## 559 secs
