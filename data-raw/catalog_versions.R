catalog_versions <- tibble::tribble(
    ~ version, ~ zenodoid,
    "1.0.0", "17981132",
    "1.1.0", "20821588",
    "1.1.1", "20859455",
    "1.1.2", "21284640"
)

usethis::use_data(catalog_versions, overwrite = TRUE, internal = TRUE)
