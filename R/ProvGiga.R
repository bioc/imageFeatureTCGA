#' @name ProvGiga
#'
#' @aliases ProvGiga-class ProvGigaCSV-class
#'
#' @title Import ProvGiga slide-level data into a Bioconductor class object
#'
#' @description The `ProvGiga` class represents ProvGiga slide-level CSV files
#'   containing embeddings for histopathology images. It extends the `TENxFile`
#'   class from the `TENxIO` package, allowing for efficient handling of large
#'   CSV files. The class includes slots to specify the output class when
#'   importing the data, either `SpatialExperiment` or
#'   `SpatialFeatureExperiment`, the tumor type, and whether the resource is a
#'   URL.
#'
#' @slot tumorType `character(1)` specifying the tumor type associated with the
#'   `ProvGiga` data.
#'
#' @slot level `character(1)` specifying the level of ProvGiga data to import.
#'   Must be one of `"slide_level"` or `"tile_level"`. If not provided, the
#'   level is inferred from the file path or URL.
#'
#' @slot is_url `logical(1)` indicating whether the resource is a URL.
#'
#' @importClassesFrom TENxIO TENxFile
#' @importFrom methods new is
#'
#' @exportClass ProvGiga
setClass(
    Class = "ProvGiga",
    contains = c("TENxFile", "VIRTUAL"),
    slots = c(
        tumorType = "character",
        level = "character",
        is_url = "logical"
    )
)

#' @exportClass ProvGigaCSV
.ProvGigaCSV <- setClass(
    Class = "ProvGigaCSV",
    contains = "ProvGiga"
)

#' @rdname ProvGiga
#'
#' @description The `ProvGiga` constructor function creates an instance of the
#'   `ProvGiga` class. The `resource` argument can be either a file path or URL
#'   to a ProvGiga CSV file. The `tumorType` parameter specifies the tumor
#'   type associated with the ProvGiga data.
#'
#' @param resource `character(1)` the file path or URL to the ProvGiga CSV file,
#'   or a `TENxFile` object.
#'
#' @param level `character(1)` specifying the level of ProvGiga data to import.
#'   Must be one of `"slide_level"` or `"tile_level"`. If not provided, the
#'   level is inferred from the file path or URL.
#'
#' @param is_url `logical(1)` indicating whether the `resource` is a URL. If not
#'   provided, it is inferred from the `resource` value.
#'
#' @param tumorType `character(1)` specifying the tumor type associated with the
#'   `ProvGiga` data. Required if `resource` is a local file (file path).
#'
#' @details The `ProvGiga` constructor function can import file paths, URLs, and
#'   `TENxFile` objects. If a local file path is provided, the `tumorType`
#'   parameter must be specified to indicate the tumor type associated with the
#'   ProvGiga data. If a URL is provided, the tumor type is inferred from the
#'   URL structure.
#'
#' @importFrom BiocBaseUtils isScalarCharacter
#' @importFrom BiocIO path
#' @importFrom TENxIO TENxFile
#' @importFrom methods is
#'
#' @returns * `ProvGiga`: An object of class `ProvGiga`.
#' * `import`: A `tibble` containing slide-level embeddings along with slide
#'   names and tumor type.
#'
#' @export
ProvGiga <- function(
    resource,
    level = c("slide_level", "tile_level"),
    is_url = TRUE,
    tumorType = NA_character_
) {
    stopifnot(
        "'resource' must be a file path, URL, or of class 'TENxFile'" =
            isScalarCharacter(resource) || is(resource, "TENxFile")
    )
    if (!is(resource, "TENxFile"))
        resource <- TENxIO::TENxFile(resource)
    filename <- path(resource)
    if (missing(is_url))
        is_url <- .is_url(filename)

    if (missing(level)) {
        levels <- vapply(
            level,
            function(x) any(
                grepl(
                    pattern = x,
                    x = strsplit(filename, .Platform$file.sep)[[1L]]
                )
            ),
            logical(1L)
        )
        if (!any(levels)) {
            warning(
                "'level' could not be inferred from the file path. ",
                "Defaulting to 'slide_level'."
            )
            level <- match.arg(level)
        } else {
            level <- level[levels]
        }
    } else {
        level <- match.arg(level)
    }

    .ProvGigaCSV(
        resource, is_url = is_url, tumorType = tumorType, level = level
    )
}

#' @rdname ProvGiga
#'
#' @section `show`: The `show` method for `ProvGiga` objects displays
#'   information about the object, including the resource path and tumor type.
#'
#' @param object An object of class `ProvGiga`.
#'
#' @importFrom methods setMethod show callNextMethod
#'
#' @exportMethod show
setMethod("show", "ProvGiga", function(object) {
    callNextMethod()
    cat("tumorType:", object@tumorType, "\n")
    cat("level:", object@level, "\n")
})

#' @rdname ProvGiga
#'
#' @section `import`: The `import` method for `ProvGiga` objects reads the
#'   ProvGiga CSV file and extracts slide-level embeddings along with the slide
#'   names and tumor type. The embeddings are returned as a `tibble` with
#'   columns for slide names, tumor type, and embedding values.
#'
#' @inheritParams BiocIO::import
#'
#' @importFrom BiocIO import path
#' @importFrom utils read.table
#'
#' @author Ilaria B., Marcel R.
#'
#' @examplesIf interactive()
#' ## Importing a slide_level ProvGiga CSV file from a local path
#' slide_prov_url <-
#'     getCatalog(pipeline = "provgigapath", format = "csv") |>
#'     dplyr::filter(
#'         level == "slide_level" &
#'             filename == paste0(
#'                 "TCGA-OR-A5JJ-01Z-00-DX1.",
#'                 "459B5DFE-47B1-426F-B009-7664C1B6FEEC.csv.gz"
#'             )
#'     ) |>
#'     getFileURLs()
#'
#' slide_file <- file.path(tempdir(), basename(slide_prov_url))
#' download.file(slide_prov_url, destfile = slide_file)
#'
#' ProvGiga(slide_file, level = "slide_level", tumorType = "TCGA_ACC") |>
#'     import()
#'
#' ## Importing a slide_level ProvGiga CSV file from a URL
#' ProvGiga(slide_prov_url, tumorType = "TCGA_ACC") |>
#'     import()
#'
#' ## Import tile_level ProvGiga CSV file from a URL
#' tile_prov_url <-
#'     getCatalog(pipeline = "provgigapath", format = "csv") |>
#'     dplyr::filter(
#'         level == "tile_level" &
#'         filename == paste0(
#'             "TCGA-AA-3556-01Z-00-DX1.",
#'             "63a74b91-44e8-4ffd-8737-bcf6992183c3.csv.gz"
#'         )
#'     ) |>
#'     getFileURLs()
#'
#' ProvGiga(tile_prov_url, tumorType = "TCGA_COAD") |>
#'     import()
#' @exportMethod import
setMethod("import", "ProvGigaCSV", function(con, format, text, ...) {
    prov_path <- path(con)
    tumorType <- con@tumorType

    args <- list(...)
    redownload <- args[["redownload"]] %||% FALSE
    args <- args[names(args) != "redownload"]

    if (con@is_url)
        prov_path <- .cache_url_files(prov_path, redownload)

    .import_level <- switch(
        con@level,
        slide_level = .import_slide_level,
        tile_level = .import_tile_level
    )

    do.call(
        .import_level,
        list(
            prov_path = prov_path,
            tumorType = tumorType,
            fileName = basename(prov_path)
        ) |> c(args)
    )
})
