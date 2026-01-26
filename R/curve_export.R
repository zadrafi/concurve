#' Export Consonance Data for Power BI
#'
#' Exports consonance function data in formats optimized for Power BI
#' visualization and analysis.
#'
#' @param data A concurve object or intervals data frame.
#' @param file Output file path. Supports .csv, .xlsx, or .json extensions.
#' @param format Output format: "csv" (default), "excel", or "json".
#'   If NULL, inferred from file extension.
#' @param include_metadata Logical. If TRUE, includes additional metadata columns
#'   useful for Power BI DAX measures. Default is TRUE.
#' @param pivot Logical. If TRUE, creates a pivoted format with separate columns
#'   for lower and upper bounds at each confidence level. Default is FALSE.
#'
#' @return Invisibly returns the exported data frame.
#'
#' @details
#' This function prepares consonance function data for import into Power BI.
#' The exported data includes:
#' \itemize{
#'   \item Confidence levels and corresponding intervals
#'   \item P-values and S-values
#'   \item Interval widths for precision analysis
#' }
#'
#' When \code{include_metadata = TRUE}, adds columns for:
#' \itemize{
#'   \item \code{ci_label}: Human-readable confidence level (e.g., "95\% CI")
#'   \item \code{significance}: Whether interval excludes null value
#' }
#'
#' @examples
#' \dontrun{
#' # Generate consonance data
#' model <- lm(mpg ~ wt, data = mtcars)
#' result <- curve_gen(model, "wt")
#'
#' # Export to CSV for Power BI
#' export_for_powerbi(result[[1]], "consonance_data.csv")
#'
#' # Export with metadata
#' export_for_powerbi(result[[1]], "consonance_data.csv", include_metadata = TRUE)
#'
#' # Export to Excel
#' export_for_powerbi(result[[1]], "consonance_data.xlsx", format = "excel")
#' }
#'
#' @seealso [curve_table()] for formatted interval tables
#' @seealso [curve_snowflake()] for Snowflake database integration
#'
#' @export
export_for_powerbi <- function(data, file,
                                format = NULL,
                                include_metadata = TRUE,
                                pivot = FALSE) {

  # Extract data frame if concurve object
  if (is.list(data) && !is.data.frame(data)) {
    df <- data[[1]]  # Get intervals dataframe
  } else if (is.data.frame(data)) {
    df <- data
  } else {
    stop("Error: 'data' must be a concurve object or data frame")
  }

  # Infer format from file extension if not specified
  if (is.null(format)) {
    ext <- tolower(tools::file_ext(file))
    format <- switch(ext,
      "csv" = "csv",
      "xlsx" = "excel",
      "xls" = "excel",
      "json" = "json",
      stop("Unknown file extension. Please specify format explicitly.")
    )
  }

  # Add metadata columns
  if (include_metadata) {
    df$ci_label <- paste0(round(df$intrvl.level * 100, 1), "% CI")
    df$interval_text <- paste0("[", round(df$lower.limit, 4), ", ", round(df$upper.limit, 4), "]")

    # Detect if ratio measure (all positive bounds suggests ratio)
    is_ratio <- all(df$lower.limit > 0, na.rm = TRUE)
    null_value <- if (is_ratio) 1 else 0
    df$excludes_null <- !(df$lower.limit <= null_value & df$upper.limit >= null_value)
  }

  # Pivot if requested
  if (pivot) {
    # Create wide format suitable for some Power BI visuals
    df_wide <- df[, c("intrvl.level", "lower.limit", "upper.limit", "pvalue", "svalue")]
    df_wide$ci_pct <- round(df_wide$intrvl.level * 100, 1)
    df <- df_wide
  }

  # Export based on format
  if (format == "csv") {
    utils::write.csv(df, file, row.names = FALSE)
    message("Exported ", nrow(df), " rows to: ", file)

  } else if (format == "excel") {
    if (!requireNamespace("openxlsx", quietly = TRUE)) {
      warning("Package 'openxlsx' not installed. Falling back to CSV format.")
      file <- sub("\\.xlsx?$", ".csv", file)
      utils::write.csv(df, file, row.names = FALSE)
      message("Exported ", nrow(df), " rows to: ", file)
    } else {
      wb <- openxlsx::createWorkbook()
      openxlsx::addWorksheet(wb, "Consonance Data")
      openxlsx::writeData(wb, "Consonance Data", df)
      openxlsx::saveWorkbook(wb, file, overwrite = TRUE)
      message("Exported ", nrow(df), " rows to: ", file)
    }

  } else if (format == "json") {
    if (!requireNamespace("jsonlite", quietly = TRUE)) {
      stop("Package 'jsonlite' required for JSON export. Please install it.")
    }
    json_data <- jsonlite::toJSON(df, pretty = TRUE, auto_unbox = TRUE)
    writeLines(json_data, file)
    message("Exported ", nrow(df), " rows to: ", file)
  }

  invisible(df)
}
