#!/usr/bin/env Rscript
# =============================================================================
# scan_bare_imports.R
#
# Guards against a specific class of bug: a function from a package listed in
# DESCRIPTION's `Imports:` field is called as a bare name (no `pkg::` prefix)
# and is not covered by a NAMESPACE `importFrom()`/`import()` directive.
#
# Listing a package under `Imports` only makes it a build/installation-time
# dependency -- it does NOT put that package's functions on the search path
# inside this package's namespace. Unless the call is qualified (`pkg::fn()`)
# or explicitly imported via roxygen `@importFrom`, it will fail at runtime
# with "could not find function" the moment a caller doesn't happen to have
# that package separately attached via library(). This is exactly what
# happened with `bcaboot::bcajack()` being called as bare `bcajack()`.
#
# Packages that ship with base R and are attached by default in every R
# session (base, methods, datasets, utils, grDevices, graphics, stats) are
# excluded from this check, since bare calls to their exports are always
# safe at runtime (R CMD check may still emit a NOTE, but that's cosmetic).
#
# Usage:
#   Rscript tools/scan_bare_imports.R
#
# Exit status: 0 if clean, 1 if offending bare calls are found.
# =============================================================================

pkg_root <- getwd()
desc_path <- file.path(pkg_root, "DESCRIPTION")
ns_path <- file.path(pkg_root, "NAMESPACE")

if (!file.exists(desc_path) || !file.exists(ns_path)) {
  stop(
    "Could not find DESCRIPTION/NAMESPACE in '", pkg_root, "'. ",
    "Run this script from the package root, e.g.:\n",
    "  Rscript tools/scan_bare_imports.R",
    call. = FALSE
  )
}

# ---- Parse DESCRIPTION Imports ---------------------------------------------

desc <- read.dcf(desc_path)
imports_field <- if ("Imports" %in% colnames(desc)) desc[1, "Imports"] else ""
imports <- trimws(strsplit(imports_field, ",")[[1]])
imports <- imports[nzchar(imports)]
imports <- gsub("\\s*\\(.*\\)\\s*$", "", imports) # drop version constraints

default_pkgs <- c("base", "methods", "datasets", "utils", "grDevices", "graphics", "stats")
risky_imports <- setdiff(imports, default_pkgs)

# Names exported by any always-attached default package are always safe to
# call bare, even if some other Imports package happens to export a
# same-named function too (e.g. dplyr::setdiff()/intersect(), which are S3
# generics for base::setdiff()/intersect()).
default_exports <- unique(unlist(lapply(default_pkgs, function(pkg) {
  tryCatch(getNamespaceExports(asNamespace(pkg)), error = function(e) character(0))
})))

if (length(risky_imports) == 0) {
  cat("No non-default Imports packages to check. OK.\n")
  quit(status = 0, save = "no")
}

# ---- Parse NAMESPACE for existing importFrom()/import() coverage -----------
#
# NAMESPACE syntax is valid R call syntax, and roxygen2 packs multiple
# @importFrom tags for the same package into a single (often multi-line)
# importFrom(pkg, fn1, fn2, ...) call -- so this must be parsed as R code
# rather than matched line-by-line with a regex.

ns_calls <- tryCatch(parse(ns_path), error = function(e) NULL)

imp_from_list <- list()
full_import_pkgs <- character(0)

for (cl in as.list(ns_calls)) {
  if (!is.call(cl) || !is.symbol(cl[[1]])) next
  fn <- as.character(cl[[1]])
  args <- as.list(cl)[-1]

  if (fn == "importFrom" && length(args) >= 2) {
    pkg <- as.character(args[[1]])
    funs <- vapply(args[-1], as.character, character(1))
    imp_from_list[[length(imp_from_list) + 1]] <- data.frame(
      pkg = pkg, fun = funs, stringsAsFactors = FALSE
    )
  } else if (fn == "import" && length(args) >= 1) {
    full_import_pkgs <- c(full_import_pkgs, vapply(args, as.character, character(1)))
  }
}

imp_from_df <- if (length(imp_from_list) == 0) {
  data.frame(pkg = character(0), fun = character(0), stringsAsFactors = FALSE)
} else {
  do.call(rbind, imp_from_list)
}

# ---- Collect exported function names for each risky Imports package -------

get_exports <- function(pkg) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    warning("Package '", pkg, "' is not installed; skipping its exports.", call. = FALSE)
    return(character(0))
  }
  tryCatch(getNamespaceExports(asNamespace(pkg)), error = function(e) character(0))
}

exports_list <- setNames(lapply(risky_imports, get_exports), risky_imports)

all_pairs <- do.call(rbind, lapply(names(exports_list), function(p) {
  fn <- exports_list[[p]]
  if (length(fn) == 0) {
    return(NULL)
  }
  data.frame(pkg = p, fun = fn, stringsAsFactors = FALSE)
}))

if (is.null(all_pairs) || nrow(all_pairs) == 0) {
  cat("Could not resolve exports for any risky Imports package (are they installed?). Skipping scan.\n")
  quit(status = 0, save = "no")
}

# ---- Gather this package's own function/object names (never flag these) ---

r_files <- list.files(file.path(pkg_root, "R"), pattern = "\\.[Rr]$", full.names = TRUE)

own_names <- character(0)
for (f in r_files) {
  exprs <- tryCatch(parse(f, keep.source = FALSE), error = function(e) NULL)
  if (is.null(exprs)) next
  for (e in as.list(exprs)) {
    if (is.call(e) && length(e) >= 2 && is.symbol(e[[1]]) &&
      as.character(e[[1]]) %in% c("<-", "=", "<<-") &&
      is.symbol(e[[2]])) {
      own_names <- c(own_names, as.character(e[[2]]))
    }
  }
}
own_names <- unique(own_names)

# Calls whose arguments are never evaluated as R code (plotmath expressions,
# quoted formulas, etc.) -- bare "function-looking" symbols inside these are
# not actually function calls at runtime.
quoting_calls <- c("quote", "expression", "bquote", "substitute")

# ---- Scan a single file using real parse data (avoids string/comment noise)-

scan_file <- function(path) {
  pd <- tryCatch(
    utils::getParseData(parse(path, keep.source = TRUE)),
    error = function(e) NULL
  )
  if (is.null(pd) || nrow(pd) == 0) {
    return(NULL)
  }

  calls <- pd[pd$token == "SYMBOL_FUNCTION_CALL", ]
  if (nrow(calls) == 0) {
    return(NULL)
  }

  flagged <- vector("list", nrow(calls))

  for (i in seq_len(nrow(calls))) {
    row <- calls[i, ]
    fn_name <- row$text

    if (fn_name %in% own_names) next

    # Names also exported by an always-attached default package are safe to
    # call bare (base wins the lookup at runtime).
    if (fn_name %in% default_exports) next

    matching_pkgs <- unique(all_pairs$pkg[all_pairs$fun == fn_name])
    if (length(matching_pkgs) == 0) next

    # Already covered by an existing full import() or importFrom()?
    covered <- any(matching_pkgs %in% full_import_pkgs) ||
      any(imp_from_df$fun == fn_name & imp_from_df$pkg %in% matching_pkgs)
    if (covered) next

    # Already namespace-qualified (pkg::fn / pkg:::fn)? Siblings of this call
    # symbol within the same expr will include an NS_GET / NS_GET_INT token.
    siblings <- pd[pd$parent == row$parent, ]
    if (any(siblings$token %in% c("NS_GET", "NS_GET_INT"))) next

    # Walk up the ancestor chain: if this call is (transitively) an argument
    # to quote()/expression()/bquote()/substitute(), it's never evaluated
    # (e.g. plotmath like expression(paste(italic(p), "-value"))).
    #
    # Parse-tree shape: a SYMBOL_FUNCTION_CALL is wrapped in its own "expr"
    # node, which is the first child of the node representing the full call.
    # So: climb to the call node, then repeatedly ask whether the enclosing
    # call's function slot (the lowest-id "expr" child of each ancestor,
    # which wraps that call's SYMBOL_FUNCTION_CALL) is a quoting function.
    skip <- FALSE
    wrap_id <- row$parent
    cur_call_id <- if (length(wrap_id) == 1 && wrap_id != 0) {
      pd$parent[pd$id == wrap_id]
    } else {
      integer(0)
    }
    visited <- integer(0)
    while (length(cur_call_id) == 1 && cur_call_id != 0) {
      outer_id <- pd$parent[pd$id == cur_call_id]
      if (length(outer_id) != 1 || outer_id == 0 || outer_id %in% visited) break
      visited <- c(visited, outer_id)

      outer_expr_children <- pd[pd$parent == outer_id & pd$token == "expr", ]
      if (nrow(outer_expr_children) > 0) {
        fn_slot_id <- outer_expr_children$id[which.min(outer_expr_children$id)]
        fn_call <- pd[pd$parent == fn_slot_id & pd$token == "SYMBOL_FUNCTION_CALL", ]
        if (nrow(fn_call) > 0 && fn_call$text[1] %in% quoting_calls) {
          skip <- TRUE
          break
        }
      }
      cur_call_id <- outer_id
    }
    if (skip) next

    flagged[[i]] <- data.frame(
      file = basename(path),
      line = row$line1,
      call = fn_name,
      candidate_pkgs = paste(matching_pkgs, collapse = "/"),
      stringsAsFactors = FALSE
    )
  }

  flagged <- Filter(Negate(is.null), flagged)
  if (length(flagged) == 0) {
    return(NULL)
  }
  do.call(rbind, flagged)
}

all_flags <- do.call(rbind, lapply(r_files, scan_file))

if (is.null(all_flags) || nrow(all_flags) == 0) {
  cat(
    "OK: no unqualified calls to functions from non-default Imports packages",
    "(", paste(risky_imports, collapse = ", "), ").\n"
  )
  quit(status = 0, save = "no")
}

all_flags <- unique(all_flags)
all_flags <- all_flags[order(all_flags$file, all_flags$line), ]

cat("Found", nrow(all_flags), "unqualified call(s) to Imports-package functions:\n\n")
print(all_flags, row.names = FALSE)
cat(
  "\nEach call above must be namespace-qualified (pkg::fn()) or covered by an\n",
  "@importFrom/@import roxygen directive (regenerate NAMESPACE afterwards).\n",
  "Otherwise it will fail with 'could not find function' unless the calling\n",
  "session happens to already have that package attached via library().\n",
  sep = ""
)
quit(status = 1, save = "no")
