.BASE_URL <- "https://nyu1.osn.mghpcc.org"
.OSN_BUCKET_NAME <- "waldronlab-image-features"
.CATALOG_COL_TYPES <- "ccccccccccccccccccccccddc"

#' @title Download the catalog of available HoVerNet and ProvGigaPath files
#'
#' @description The `getCatalog` function retrieves a catalog of all available
#'   HoVerNet and ProvGigaPath files, including filenames, sizes, pipelines
#'   used, tumor types, and data levels.
#'
#' @param pipeline `character()` One or both "hovernet" and/or "provgigapath"
#'   specifying which pipeline(s) to include in the catalog. Default includes
#'   both.
#'
#' @param format `character()` One or more of "csv", "thumb", "h5ad", "geojson",
#'   or "json" specifying which file formats to include in the catalog. Default
#'   includes all.
#'
#' @param redownload `logical(1L)` Whether to redownload the catalog file even
#'   if it is already cached locally. Default is `FALSE`.
#'
#' @returns `getCatalog`: A `tibble` containing the full catalog of available
#'   files for the specified pipeline(s).
#'
#' @examplesIf interactive()
#' ## Get the full catalog of available files
#' getCatalog(pipeline = c("hovernet", "provgigapath"), format = "h5ad")
#' @export
getCatalog <-
    function(
        pipeline = c("hovernet", "provgigapath"),
        format = c("csv", "thumb", "h5ad", "geojson", "json"),
        redownload = FALSE
    )
{
    pipeline <- match.arg(pipeline, several.ok = TRUE)
    format <- match.arg(format, several.ok = TRUE)
    catalog <- .download_catalog(redownload = redownload) |>
        readr::read_tsv(col_types = .CATALOG_COL_TYPES)
    in_pipe <- catalog[["pipeline"]] %in% pipeline
    in_format <- catalog[["format"]] %in% format
    catalog[in_pipe & in_format, ]
}

.CATALOG_BASE_URL <- "https://zenodo.org"

#' @importFrom httr2 request req_headers req_perform resp_body_json
.download_catalog <- function(redownload) {
    resp <- paste(
        .CATALOG_BASE_URL,
        "api/records/20821588",
        sep = "/"
    ) |>
        request() |>
        req_headers(
            Accept = "application/json"
        ) |>
        req_perform() |>
        resp_body_json()

    .cache_url_files(
        resp[["files"]][[1L]][[c("links", "self")]], redownload = redownload
    )
}

#' @rdname getCatalog
#'
#' @param catalog A `tibble` as returned by `getCatalog()`.
#'
#' @returns `getFileURLs`: A `character()` vector of full URLs for the files
#'   listed in the provided catalog.
#'
#' @examplesIf interactive()
#' ## Get file URLs from the catalog
#' getCatalog(pipeline = "hovernet", format = "h5ad") |>
#'     dplyr::slice(1:10) |>
#'     getFileURLs()
#' @export
getFileURLs <- function(catalog) {
    paste(.BASE_URL, .OSN_BUCKET_NAME, catalog[["fullpath"]], sep = "/")
}
