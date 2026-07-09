.TYPE_MAP <- data.frame(
    type = seq(0L, 5L),
    label = c("nolabe", "neopla", "inflam", "connec", "necros", "no-neo"),
    R = c(0L, 255L, 0L, 0L, 255L, 255L),
    G = c(0L, 0L, 255L, 0L, 255L, 165L),
    B = c(0L, 0L, 0L, 255L, 0L, 0L)
)

#' @name HoverNet
#'
#' @aliases HoverNet-class HoverNetJSON-class HoverNetH5AD-class
#' @aliases HoverNetPNG-class
#'
#' @title Import Hovernet JSON, H5AD, and PNG files into Bioconductor classes
#'
#' @description The `HoverNet` virtual class and its subclasses represent
#'   different file formats used in the HoverNet cell segmentation and
#'   classification pipeline for histopathology images. The `HoverNet`
#'   constructor function creates instances of the appropriate subclass based on
#'   the file format of the provided resource. The `import` methods for each
#'   subclass read the respective file formats and represent the data as either
#'   a `SpatialExperiment` or `SpatialFeatureExperiment` object, depending on
#'   the specified output class.
#'
#' @details The `HoverNetJSON` class represents Hovernet JSON files used for
#'   cell segmentation and classification in histopathology images. It extends
#'   the `TENxFile` class from the `TENxIO` package, allowing for efficient
#'   handling of large JSON files. The class includes a slot to indicate whether
#'   cell contours should be included in the metadata when importing the data.
#'   As well as a slot to specify the output class when importing the data,
#'   either `SpatialExperiment` or `SpatialFeatureExperiment`.
#'   The `HoverNetH5AD` class represents Hovernet H5AD files, which contain
#'   similar data but in a different format. The `HoverNetPNG` class represents
#'   PNG thumbnail images of the whole-slide images used in HoverNet.
#'
#' @slot contours `logical(1)` indicating whether to include cell contours in
#'   the metadata of the resulting `SpatialExperiment` or
#'   `SpatialFeatureExperiment` object.
#'
#' @slot outClass `character(1)` specifying the output class when importing the
#'   data. One of `"SpatialExperiment"` or `"SpatialFeatureExperiment"`.
#'
#' @slot is_url `logical(1)` indicating whether the resource is a URL.
#'
#' @importClassesFrom TENxIO TENxFile
#' @importFrom methods new is
#'
#' @exportClass HoverNet
setClass(
    Class = "HoverNet",
    contains = c("TENxFile", "VIRTUAL"),
    slots = c(
        outClass = "character",
        is_url = "logical"
    )
)

.validHoverNet <- function(object) {
    out <- TRUE
    if (!object@is_url && !file.exists(path(object)))
        out <- "The specified HoverNet file does not exist"
    out
}

S4Vectors::setValidity2("HoverNet", .validHoverNet)

#' @exportClass HoverNetJSON
.HoverNetJSON <- setClass(
    Class = "HoverNetJSON",
    contains = "HoverNet",
    slots = c(
        contours = "logical"
    )
)

#' @exportClass HoverNetH5AD
.HoverNetH5AD <- setClass(
    Class = "HoverNetH5AD",
    contains = "HoverNet"
)

#' @rdname HoverNet
#'
#' @description The `HoverNetJSON` constructor function creates an instance
#'   of the `HoverNetJSON` class. The `resource` argument can be either a
#'   file path or URL to a Hovernet JSON file. The `contours` parameter
#'   is optional and can be used to include cell contours in the metadata.
#'   The `outClass` parameter specifies the output class when importing
#'   the data, either `SpatialExperiment` or `SpatialFeatureExperiment`.
#'
#' @param resource `character(1)` the file path or URL to the Hovernet JSON
#'   file.
#'
#' @param contours `logical(1)` whether to include cell contours in the metadata
#'   of the resulting `SpatialExperiment` or `SpatialFeatureExperiment` object.
#'   Default is `FALSE` and used only when importing "JSON" data.
#'
#' @param outClass `character(1)` specifying the output class when importing
#'   either "JSON" or "H5AD" data into a Bioconductor class object. Must be one
#'   of `"SpatialExperiment"` (default) or `"SpatialFeatureExperiment"`.
#'
#' @details The `HoverNetJSON` constructor function can import file paths
#'   and URLs. Remote files are automatically cached using `BiocFileCache`
#'   when the `import` method is called. This allows for efficient handling
#'   of large JSON files without the need to download them manually.
#'
#' @importFrom BiocIO import path
#' @importFrom TENxIO TENxFile
#' @importFrom BiocBaseUtils isScalarLogical isScalarCharacter
#'
#' @author Ilaria B., Sehyun O., Marcel R.
#'
#' @returns * `HoverNetJSON`: An object of class `HoverNetJSON`
#' * `import,HoverNetJSON-method`: An object of class `SpatialExperiment` or
#'   `SpatialFeatureExperiment` containing the cell data and spatial
#'   coordinates extracted from the Hovernet JSON file
#' * `import,HoverNetH5AD-method`: An object of class `SpatialExperiment` or
#'   `SpatialFeatureExperiment` containing the cell data and spatial
#'   coordinates extracted from the Hovernet H5AD file
#' * `import,HoverNetPNG-method`: A PNG image as an RGB array as given by
#'   `png::readPNG`.
#'
#' @export
HoverNet <- function(
    resource,
    contours = FALSE,
    outClass = c("SpatialExperiment", "SpatialFeatureExperiment")
) {
    stopifnot(
        isScalarLogical(contours),
        isScalarCharacter(resource) || is(resource, "TENxFile")
    )
    path_extract <- if (is(resource, "TENxFile")) path else I
    path_res <- path_extract(resource)
    is_url <- .is_url(path_res)
    isJSON <- grepl("\\.json(\\.gz)?$", path_res, TRUE)
    isH5AD <- grepl("\\.h5ad(\\.gz)$", path_res, TRUE)
    isPNG <- grepl("\\.png$", path_res, TRUE)
    if (!is(resource, "TENxFile"))
        resource <- TENxIO::TENxFile(resource)
    outClass <- match.arg(outClass)
    if (isJSON)
        .HoverNetJSON(
            resource, contours = contours, outClass = outClass, is_url = is_url
        )
    else if (isH5AD)
        .HoverNetH5AD(
            resource, outClass = outClass, is_url = is_url
        )
    else if (isPNG)
        .HoverNetPNG(resource, is_url = is_url)
    else
        stop(
            "Unsupported file format. Provide a JSON or H5AD file for HoverNet."
        )
}

#' @rdname HoverNet
#'
#' @section `show`: The `show` method for `HoverNetJSON` objects displays the
#'   `resource`, `contours`, and `outClass` slots and vaules.
#'
#' @param object An object of class `HoverNetJSON`.
#'
#' @importFrom methods setMethod show callNextMethod
#'
#' @exportMethod show
setMethod("show", "HoverNetJSON", function(object) {
    callNextMethod()
    cat(
        "contours: ", object@contours, "\n",
        "outClass: ", object@outClass, "\n",
        sep = ""
    )
})

#' @rdname HoverNet
#'
#' @section `import`: The import method for `HoverNetJSON` reads the JSON
#'   file and represents the data as either a `SpatialExperiment` or
#'   `SpatialFeatureExperiment` object. It extracts cell centroid coordinates,
#'   cell types, and type probabilities, and optionally includes cell contours
#'   in the metadata. The resulting `SpatialExperiment` object contains the cell
#'   data in the `colData` slot and spatial coordinates in the `spatialCoords`
#'   slot of the object.
#'
#' @inheritParams BiocIO::import
#'
#' @importFrom BiocIO import path
#' @importFrom BiocBaseUtils checkInstalled
#' @importFrom rjsoncons j_query
#' @importFrom S4Vectors metadata metadata<-
#'
#' @examplesIf interactive()
#' ## Manual download and local file input
#' hov_json_file <-
#'     getCatalog("hovernet", "json") |>
#'     dplyr::filter(
#'         filename == paste0(
#'             "TCGA-VG-A8LO-01A-01-DX1.",
#'             "B39A4D64-82A1-4A04-8AB6-918F3058B83B.json.gz"
#'         )
#'     ) |>
#'     getFileURLs()
#'
#' dest_json <- file.path(tempdir(), basename(hov_json_file))
#' download.file(hov_json_file, destfile = dest_json)
#'
#' HoverNet(dest_json, outClass = "SpatialExperiment") |>
#'     import()
#'
#' ## Direct URL input (with caching)
#' HoverNet(hov_json_file, outClass = "SpatialExperiment") |>
#'     import()
#'
#' ## Import as SpatialFeatureExperiment
#' library(SpatialFeatureExperiment)
#' HoverNet(dest_json, outClass = "SpatialFeatureExperiment") |>
#'     import()
#' @exportMethod import
setMethod("import", "HoverNetJSON", function(con, format, text, ...) {
    json_path <- path(con)

    if (con@is_url)
        json_path <- .cache_url_files(json_path)

    jmespath_query_simple <- "nuc.*.{
        x: centroid[0],
        y: centroid[1],
        type: type,
        type_prob: type_prob
    }"

    cell_ids <- j_query(json_path, "nuc | keys(@)", as = "R")

    # Extract the cell data values using the simplified query
    cell_data_list <- j_query(
        json_path,
        jmespath_query_simple,
        as = "R"
    )

    # Add the cell_id column
    cells <- dplyr::bind_rows(cell_data_list) |>
        dplyr::mutate(cell_id = cell_ids, .before = 1)

    # Join with labels/colors
    cells <- dplyr::left_join(cells, .TYPE_MAP, by = "type")

    # Build assay
    assay_data <- matrix(0, nrow = 0, ncol = nrow(cells))

    outClass <- con@outClass
    FUN <- if (identical(outClass, "SpatialExperiment")) {
        SpatialExperiment::SpatialExperiment
    } else if (identical(outClass, "SpatialFeatureExperiment")) {
        checkInstalled("SpatialFeatureExperiment")
        SpatialFeatureExperiment::SpatialFeatureExperiment
    }
    out <- FUN(
        assays = list(counts = assay_data),
        colData = cells,
        spatialCoords = as.matrix(cells[, c("x", "y")]),
        spatialCoordsNames = NULL
    )
    if (con@contours) {
        contour_list <- j_query(
            json_path,
            "nuc.*.contour",
            as = "R"
        )
        metadata(out)$contours <- contour_list
    }
    metadata(out)$type_map <- .TYPE_MAP
    out
})

#' @rdname HoverNet
#'
#' @section `import`: The import method for `HoverNetH5AD` reads the H5AD file
#'   and represents the data as either a `SpatialExperiment` or
#'   `SpatialFeatureExperiment` object. It extracts cell centroid coordinates,
#'   cell types, mean intensity, and nearest neighbor distance. The resulting
#'   `SpatialExperiment` object contains the cell data in the `colData` slot and
#'   spatial coordinates in the `spatialCoords` slot of the object.
#'
#' @importFrom SummarizedExperiment assay<- assays rowData colData
#'
#' @examplesIf interactive()
#' hov_h5ad_file <-
#'     getCatalog("hovernet", "h5ad") |>
#'     dplyr::filter(
#'         filename == paste0(
#'             "TCGA-VG-A8LO-01A-01-DX1.",
#'             "B39A4D64-82A1-4A04-8AB6-918F3058B83B.h5ad.gz"
#'         )
#'     ) |>
#'     getFileURLs()
#' dest_h5ad <- file.path(tempdir(), basename(hov_h5ad_file))
#' download.file(hov_h5ad_file, destfile = dest_h5ad)
#'
#' HoverNet(dest_h5ad, outClass = "SpatialExperiment") |>
#'     import()
#' @exportMethod import
setMethod("import", "HoverNetH5AD", function(con, format, text, ...) {
    h5ad_path <- path(con)

    if (con@is_url)
        h5ad_path <- .cache_url_files(h5ad_path)

    BiocBaseUtils::checkInstalled("anndataR")
    res <- anndataR::read_h5ad(path = h5ad_path, as = "SingleCellExperiment")
    scoords <- SingleCellExperiment::reducedDim(res, "spatial")
    colnames(scoords) <- c("x_centroid", "y_centroid")

    assay(res, "mean_intensity", withDimnames = FALSE) <-
        matrix(colData(res)[["mean_intensity"]], nrow = 1L)
    assay(res, "nnd", withDimnames = FALSE) <-
        matrix(colData(res)[["nearest_neighbor_distance"]], nrow = 1L)

    res <- SpatialExperiment::SpatialExperiment(
        assays = assays(res),
        colData = colData(res),
        rowData = rowData(res),
        spatialCoords = scoords
    )

    if (identical(con@outClass, "SpatialFeatureExperiment"))
        res <- SpatialFeatureExperiment::toSpatialFeatureExperiment(res)

    res
})

#' @rdname HoverNet
#'
#' @exportClass HoverNetPNG
.HoverNetPNG <- setClass(
    Class = "HoverNetPNG",
    contains = "HoverNet"
)

#' @rdname HoverNet
#'
#' @examples
#' ## Import HoverNetPNG thumbnail from URL
#' hov_png_url <-
#'     getCatalog("hovernet", "thumb") |>
#'     dplyr::filter(
#'         filename == paste0(
#'             "TCGA-VG-A8LO-01A-02-DX2.",
#'             "9B58474C-DAC0-4D45-B13C-0A1EA9E1BC32.png"
#'         )
#'     ) |>
#'     getFileURLs()
#'
#' HoverNet(hov_png_url) |>
#'     import()
#' @exportMethod import
setMethod("import", "HoverNetPNG", function(con, format, text, ...) {
    png_path <- path(con)

    if (con@is_url)
        png_path <- .cache_url_files(png_path)

    BiocBaseUtils::checkInstalled("png")
    png::readPNG(png_path, ...)
})
