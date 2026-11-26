#########################################################
## Python export (Eidolon miniatures + full artifacts)
##
## Reads the R `.RData` datasets (mini in data/, full in harbinger/) and writes
## Python-native artifacts, mirroring the pytspredit/tspredit/tspredbench layout:
##   - mini   -> CSV (tidy long: series + the frame's own columns) for pyharbinger
##   - full   -> CSV.GZ (same schema) hosted in this repo under python/
##   - manifest.json {kind, columns, series_mini, n_series_full, full_url}
##
## Usage: Rscript etl/py_export.R  [dataset_name]   (from the united repo root)
##   with no argument, exports every dataset in data/.
#########################################################

suppressMessages(library(jsonlite))

RAW_BASE <- "https://raw.githubusercontent.com/cefet-rj-dal/united/refs/heads/main/python"
PYHARB_DATA <- "C:/Git/dal/pyharbinger/src/pyharbinger/data/united"
FULL_OUT <- "python"

dir.create(PYHARB_DATA, recursive = TRUE, showWarnings = FALSE)
dir.create(FULL_OUT, recursive = TRUE, showWarnings = FALSE)

# Flatten a named list of data frames into one tidy long data frame with a
# leading `series` column. All frames in a dataset share the same columns.
flatten_series <- function(obj) {
  stopifnot(is.list(obj), !is.data.frame(obj))
  names_vec <- names(obj)
  if (is.null(names_vec)) names_vec <- as.character(seq_along(obj))
  parts <- lapply(seq_along(obj), function(i) {
    df <- as.data.frame(obj[[i]], stringsAsFactors = FALSE)
    cbind(series = names_vec[i], df, stringsAsFactors = FALSE)
  })
  # Align to the union of columns (series may differ in optional columns).
  all_cols <- unique(unlist(lapply(parts, colnames)))
  parts <- lapply(parts, function(df) {
    missing <- setdiff(all_cols, colnames(df))
    for (m in missing) df[[m]] <- NA
    df[, all_cols, drop = FALSE]
  })
  do.call(rbind, parts)
}

load_one <- function(path, name) {
  e <- new.env()
  load(path, envir = e)
  get(name, envir = e)
}

export_dataset <- function(name) {
  mini <- load_one(file.path("data", paste0(name, ".RData")), name)
  full <- load_one(file.path("harbinger", paste0(name, ".RData")), name)

  mini_df <- flatten_series(mini)
  full_df <- flatten_series(full)

  # mini -> pyharbinger CSV
  utils::write.csv(mini_df, file.path(PYHARB_DATA, paste0(name, ".csv")),
                   row.names = FALSE, na = "")
  # full -> gzipped CSV hosted here
  gz <- gzfile(file.path(FULL_OUT, paste0(name, ".csv.gz")), "w")
  utils::write.csv(full_df, gz, row.names = FALSE, na = "")
  close(gz)

  list(
    kind = "frame_list",
    columns = colnames(as.data.frame(mini[[1]])),
    series_mini = if (is.null(names(mini))) as.character(seq_along(mini)) else names(mini),
    n_series_full = length(full),
    full_rows = nrow(full_df),
    full_url = sprintf("%s/%s.csv.gz", RAW_BASE, name)
  )
}

args <- commandArgs(trailingOnly = TRUE)
all_names <- sub("\\.RData$", "", list.files("data", pattern = "\\.RData$"))
targets <- if (length(args) >= 1) args[[1]] else all_names

manifest_path <- file.path(PYHARB_DATA, "manifest.json")
manifest <- if (file.exists(manifest_path)) jsonlite::read_json(manifest_path) else list()

for (nm in targets) {
  message("==> ", nm)
  manifest[[nm]] <- export_dataset(nm)
  cat(sprintf("    mini rows=%d  full rows=%d  series_full=%d\n",
      nrow(flatten_series(load_one(file.path("data", paste0(nm, ".RData")), nm))),
      manifest[[nm]]$full_rows, manifest[[nm]]$n_series_full))
}

jsonlite::write_json(manifest, manifest_path, auto_unbox = TRUE, pretty = TRUE)
message("manifest: ", manifest_path, " (", length(manifest), " datasets)")
