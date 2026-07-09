# URL test
json_url <- getCatalog("hovernet", "json") |>
    dplyr::filter(
        filename == paste0(
            "TCGA-VG-A8LO-01A-01-DX1.",
            "B39A4D64-82A1-4A04-8AB6-918F3058B83B.json.gz"
        )
    ) |>
    getFileURLs()

# check JSON
hn <- HoverNet(json_url)
expect_inherits(hn, "HoverNetJSON")
expect_false(hn@contours)

# Import
spe <- import(hn)
expect_inherits(spe, "SpatialExperiment")
expect_true(ncol(spe) > 0)

cd <- SummarizedExperiment::colData(spe)
expect_true(all(c("cell_id", "x", "y", "type") %in% names(cd)))

sc <- SpatialExperiment::spatialCoords(spe)
expect_equal(nrow(sc), ncol(spe))

# Type map in metadata
expect_true("type_map" %in% names(S4Vectors::metadata(spe)))

# Contours
hn_cont <- HoverNet(json_url, contours = TRUE)
spe_cont <- import(hn_cont)
expect_true("contours" %in% names(S4Vectors::metadata(spe_cont)))

# # Test .TYPE_MAP
# expect_equal(nrow(.TYPE_MAP), 6)
# expect_equal(.TYPE_MAP$type, 0:5)
# expect_true(all(c("label", "R", "G", "B") %in% names(.TYPE_MAP)))
#
# # Check labels
# expected_labels <- c("nolabe", "neopla", "inflam", "connec", "necros", "no-neo")
# expect_equal(.TYPE_MAP$label, expected_labels)
#
# # Check RGB
# expect_true(all(.TYPE_MAP$R >= 0 & .TYPE_MAP$R <= 255))
# expect_true(all(.TYPE_MAP$G >= 0 & .TYPE_MAP$G <= 255))
# expect_true(all(.TYPE_MAP$B >= 0 & .TYPE_MAP$B <= 255))
