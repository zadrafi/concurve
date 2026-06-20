#' Construct Consonance Functions from Snowflake Query Results
#'
#' Connects to a Snowflake database and constructs consonance distributions
#' from query results containing point estimates and confidence intervals.
#'
#' @param conn A DBI connection object to Snowflake database.
#' @param query SQL query string that returns columns for estimate, lower, and upper bounds.
#' @param estimate_col Name of the column containing point estimates. Default is "estimate".
#' @param lower_col Name of the column containing lower bounds. Default is "lower".
#' @param upper_col Name of the column containing upper bounds. Default is "upper".
#' @param conf.level Confidence level of the input intervals. Default is 0.95.
#' @param steps Number of consonance levels to compute. Default is 1000.
#' @param cores Number of cores for parallel computation.
#' @param table Logical. If TRUE (default), includes a summary table.
#'
#' @return A list with class "concurve" containing the intervals dataframe,
#'   density dataframe, and optionally a summary table.
#'
#' @details
#' This function requires the DBI and odbc packages to be installed.
#' It connects to Snowflake, executes the provided query, and constructs
#' consonance functions from the results.
#'
#' @examples
#' \dontrun{
#' library(DBI)
#' library(odbc)
#'
#' # Connect to Snowflake
#' conn <- dbConnect(odbc::odbc(),
#'   Driver = "Snowflake",
#'   Server = "your_account.snowflakecomputing.com",
#'   Database = "your_database",
#'   Schema = "your_schema",
#'   UID = "your_username",
#'   PWD = "your_password"
#' )
#'
#' # Query with estimate and CI columns
#' query <- "SELECT estimate, lower_ci, upper_ci FROM results WHERE analysis_id = 1"
#' result <- curve_snowflake(conn, query,
#'   estimate_col = "estimate",
#'   lower_col = "lower_ci",
#'   upper_col = "upper_ci"
#' )
#'
#' ggcurve(result[[1]], type = "c")
#'
#' dbDisconnect(conn)
#' }
#'
#' @seealso [curve_snowflake_batch()] for batch processing
#' @seealso [curve_rev()] for constructing curves from published intervals
#' @seealso [export_for_powerbi()] for exporting results
#'
#' @export
curve_snowflake <- function(conn, query,
                            estimate_col = "estimate",
                            lower_col = "lower",
                            upper_col = "upper",
                            conf.level = 0.95,
                            steps = 1000,
                            cores = getOption("mc.cores", 1L),
                            table = TRUE) {

 # Check for required packages
 if (!requireNamespace("DBI", quietly = TRUE)) {
    stop("Package 'DBI' is required for Snowflake connectivity. Please install it.")
  }
  if (!requireNamespace("odbc", quietly = TRUE)) {
    stop("Package 'odbc' is required for Snowflake connectivity. Please install it.")
  }

 # Validate connection
  if (!DBI::dbIsValid(conn)) {
    stop("Error: Invalid database connection")
 }

  # Execute query
  result <- tryCatch(
    DBI::dbGetQuery(conn, query),
    error = function(e) {
      stop("Error executing query: ", e$message)
    }
  )

  # Validate columns exist
  required_cols <- c(estimate_col, lower_col, upper_col)
  missing_cols <- setdiff(required_cols, names(result))
  if (length(missing_cols) > 0) {
    stop("Error: Missing columns in query result: ", paste(missing_cols, collapse = ", "))
  }

  # Extract values
 point <- result[[estimate_col]][1]
  LL <- result[[lower_col]][1]
  UL <- result[[upper_col]][1]

  # Delegate to curve_rev
  curve_rev(
    point = point,
    LL = LL,
    UL = UL,
    conf.level = conf.level,
    type = "c",
    measure = "mean",
    steps = steps,
    cores = cores,
    table = table
  )
}


#' Batch Process Multiple Snowflake Queries for Consonance Functions
#'
#' Executes multiple queries against a Snowflake database and constructs
#' consonance distributions for each result set.
#'
#' @param conn A DBI connection object to Snowflake database.
#' @param queries A named list of SQL query strings.
#' @param estimate_col Name of the column containing point estimates.
#' @param lower_col Name of the column containing lower bounds.
#' @param upper_col Name of the column containing upper bounds.
#' @param conf.level Confidence level of the input intervals. Default is 0.95.
#' @param steps Number of consonance levels to compute. Default is 1000.
#' @param cores Number of cores for parallel computation.
#' @param table Logical. If TRUE (default), includes summary tables.
#'
#' @return A named list of concurve objects, one for each query.
#'
#' @examples
#' \dontrun{
#' queries <- list(
#'   "Model A" = "SELECT estimate, lower_ci, upper_ci FROM results WHERE model = 'A'",
#'   "Model B" = "SELECT estimate, lower_ci, upper_ci FROM results WHERE model = 'B'"
#' )
#'
#' results <- curve_snowflake_batch(conn, queries,
#'   estimate_col = "estimate",
#'   lower_col = "lower_ci",
#'   upper_col = "upper_ci"
#' )
#'
#' # Plot comparison
#' plot_compare(results[["Model A"]][[1]], results[["Model B"]][[1]])
#' }
#'
#' @seealso [curve_snowflake()] for single query processing
#' @seealso [plot_compare()] for comparing results
#'
#' @export
curve_snowflake_batch <- function(conn, queries,
                                   estimate_col = "estimate",
                                   lower_col = "lower",
                                   upper_col = "upper",
                                   conf.level = 0.95,
                                   steps = 1000,
                                   cores = getOption("mc.cores", 1L),
                                   table = TRUE) {

  if (!is.list(queries)) {
    stop("Error: 'queries' must be a named list of SQL query strings")
  }

  if (is.null(names(queries))) {
    names(queries) <- paste0("Query_", seq_along(queries))
  }

  results <- lapply(names(queries), function(name) {
    message("Processing: ", name)
    tryCatch(
      curve_snowflake(
        conn = conn,
        query = queries[[name]],
        estimate_col = estimate_col,
        lower_col = lower_col,
        upper_col = upper_col,
        conf.level = conf.level,
        steps = steps,
        cores = cores,
        table = table
      ),
      error = function(e) {
        warning("Failed to process '", name, "': ", e$message)
        NULL
      }
    )
  })

  names(results) <- names(queries)
  results <- results[!sapply(results, is.null)]

  class(results) <- c("list", "concurve_batch")
  return(results)
}


# R CMD check
utils::globalVariables(c("estimate_col", "lower_col", "upper_col"))
