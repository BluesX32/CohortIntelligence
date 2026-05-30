# anomaly.R
# Unsupervised ML pipeline for CohortIntelligence.
# Requires: umap, cluster, isotree (all in Suggests).
# Pure-R spectral fallback in run_umap() -- no Python required.

# ---------------------------------------------------------------------------
# UMAP
# ---------------------------------------------------------------------------

#' Run UMAP dimensionality reduction on a patient feature matrix
#'
#' @param feature_matrix Numeric matrix or tibble with `subject_id` column
#'   plus numeric feature columns (from [build_feature_matrix()]`$wide`).
#' @param n_components Integer. Output dimensions. Default 2.
#' @param n_neighbors Integer. UMAP n_neighbors parameter. Default 15.
#' @param min_dist Numeric. UMAP min_dist parameter. Default 0.1.
#' @param metric Character. Distance metric. Default `"euclidean"`.
#' @param random_state Integer. Random seed. Default 42.
#' @param method Character. `"umap"` uses the `umap` package;
#'   `"spectral"` is a pure-R spectral embedding fallback.
#'
#' @return tibble(subject_id, umap_1, umap_2)
#' @export
run_umap <- function(feature_matrix,
                      n_components = 2L,
                      n_neighbors  = 15L,
                      min_dist     = 0.1,
                      metric       = "euclidean",
                      random_state = 42L,
                      method       = c("umap","spectral")) {
  method <- match.arg(method)

  if (!requireNamespace("umap", quietly = TRUE)) {
    message("Package 'umap' not installed; falling back to spectral method.")
    method <- "spectral"
  }

  mat <- .prepare_feature_mat(feature_matrix)
  ids <- mat$ids
  X   <- mat$X

  set.seed(random_state)

  coords <- if (method == "umap") {
    cfg <- umap::umap.defaults
    cfg$n_components <- as.integer(n_components)
    cfg$n_neighbors  <- as.integer(n_neighbors)
    cfg$min_dist     <- min_dist
    cfg$metric       <- metric
    cfg$random_state <- as.integer(random_state)
    res <- umap::umap(X, config = cfg)
    res$layout
  } else {
    .spectral_embed(X, k = as.integer(n_components))
  }

  colnames(coords) <- paste0("umap_", seq_len(ncol(coords)))
  tibble::tibble(subject_id = ids, tibble::as_tibble(coords))
}

# ---------------------------------------------------------------------------
# Clustering
# ---------------------------------------------------------------------------

#' Cluster patients based on UMAP coordinates
#'
#' @param umap_coords tibble(subject_id, umap_1, umap_2) from [run_umap()].
#' @param method Clustering method: `"kmeans"`, `"hierarchical"`, or
#'   `"hdbscan"` (requires the `dbscan` package).
#' @param k Integer number of clusters. For `"kmeans"` and `"hierarchical"`:
#'   required (or auto-selected by gap statistic if `NULL` and `cluster` is
#'   available). For `"hdbscan"`: ignored.
#' @param min_cluster_size Integer. Minimum cluster size for hdbscan. Default 5.
#'
#' @return tibble(subject_id, cluster_id, cluster_label)
#' @export
run_clustering <- function(umap_coords,
                            method           = c("kmeans","hierarchical","hdbscan"),
                            k                = NULL,
                            min_cluster_size = 5L) {
  method <- match.arg(method)

  coords <- as.matrix(dplyr::select(umap_coords, -subject_id))
  ids    <- umap_coords$subject_id

  if (method == "hdbscan") {
    if (!requireNamespace("dbscan", quietly = TRUE)) {
      message("Package 'dbscan' not installed; falling back to kmeans.")
      method <- "kmeans"
    }
  }

  cluster_ids <- switch(method,
    kmeans = {
      k <- .resolve_k(coords, k)
      set.seed(42L)
      km <- stats::kmeans(coords, centers = k, nstart = 25L)
      km$cluster
    },
    hierarchical = {
      k   <- .resolve_k(coords, k)
      hc  <- stats::hclust(stats::dist(coords), method = "ward.D2")
      stats::cutree(hc, k = k)
    },
    hdbscan = {
      res <- dbscan::hdbscan(coords, minPts = as.integer(min_cluster_size))
      res$cluster  # 0 = noise
    }
  )

  # recode noise points (hdbscan cluster 0) to -1
  cluster_ids[cluster_ids == 0L] <- -1L

  tibble::tibble(
    subject_id    = ids,
    cluster_id    = as.integer(cluster_ids),
    cluster_label = dplyr::case_when(
      cluster_ids == -1L ~ "Noise",
      TRUE               ~ paste0("Cluster ", cluster_ids)
    )
  )
}

# ---------------------------------------------------------------------------
# Isolation Forest
# ---------------------------------------------------------------------------

#' Compute anomaly scores using Isolation Forest
#'
#' @param feature_matrix Numeric matrix or tibble with `subject_id` column.
#' @param n_trees Integer. Number of trees. Default 100.
#' @param sample_frac Numeric in (0,1]. Fraction of data per tree. Default 0.25.
#' @param random_state Integer. Random seed. Default 42.
#'
#' @return tibble(subject_id, anomaly_score) with scores in the range 0 to 1.
#' @export
run_isolation_forest <- function(feature_matrix,
                                  n_trees      = 100L,
                                  sample_frac  = 0.25,
                                  random_state = 42L) {
  if (!requireNamespace("isotree", quietly = TRUE)) {
    rlang::abort("Package 'isotree' is required for run_isolation_forest().")
  }

  mat <- .prepare_feature_mat(feature_matrix)
  set.seed(random_state)

  model  <- isotree::isolation.forest(
    mat$X,
    ntrees      = as.integer(n_trees),
    sample_size = max(2L, as.integer(sample_frac * nrow(mat$X))),
    nthreads    = 1L
  )
  scores <- predict(model, mat$X)

  tibble::tibble(subject_id = mat$ids, anomaly_score = as.numeric(scores))
}

# ---------------------------------------------------------------------------
# Full pipeline convenience wrapper
# ---------------------------------------------------------------------------

#' Run the complete unsupervised ML pipeline
#'
#' Convenience wrapper that calls [run_umap()], [run_clustering()], and
#' [run_isolation_forest()] in sequence.
#'
#' @param feature_matrix Output of [build_feature_matrix()]`$wide`.
#' @param umap_args Named list of additional arguments forwarded to [run_umap()].
#' @param cluster_args Named list forwarded to [run_clustering()].
#' @param isoforest_args Named list forwarded to [run_isolation_forest()].
#'
#' @return List with `$umap`, `$clusters`, `$anomaly`, `$merged`.
#' @export
run_full_ml_pipeline <- function(feature_matrix,
                                  umap_args      = list(),
                                  cluster_args   = list(),
                                  isoforest_args = list()) {
  message("Running UMAP...")
  umap_result <- do.call(run_umap, c(list(feature_matrix = feature_matrix), umap_args))

  message("Running clustering...")
  clusters <- do.call(run_clustering, c(list(umap_coords = umap_result), cluster_args))

  message("Running isolation forest...")
  anomaly <- do.call(run_isolation_forest, c(list(feature_matrix = feature_matrix), isoforest_args))

  merged <- umap_result |>
    dplyr::left_join(clusters, by = "subject_id") |>
    dplyr::left_join(anomaly,  by = "subject_id")

  list(umap = umap_result, clusters = clusters, anomaly = anomaly, merged = merged)
}

# ---------------------------------------------------------------------------
# Internal helpers
# ---------------------------------------------------------------------------

.prepare_feature_mat <- function(feature_matrix) {
  if (is.data.frame(feature_matrix)) {
    ids <- feature_matrix$subject_id
    X   <- as.matrix(dplyr::select(feature_matrix, -dplyr::any_of("subject_id")))
  } else {
    ids <- seq_len(nrow(feature_matrix))
    X   <- feature_matrix
  }
  X[is.na(X)] <- 0
  # Replace column medians for remaining NA (shouldn't happen after above but safe)
  for (j in seq_len(ncol(X))) {
    bad <- is.na(X[, j])
    if (any(bad)) X[bad, j] <- stats::median(X[!bad, j], na.rm = TRUE)
  }
  storage.mode(X) <- "double"
  list(ids = ids, X = X)
}

.resolve_k <- function(coords, k) {
  if (!is.null(k)) return(as.integer(k))
  if (!requireNamespace("cluster", quietly = TRUE)) return(3L)
  # Gap statistic to pick k in [2, 8]
  gap <- cluster::clusGap(coords,
    FUNcluster = function(x, k) list(cluster = stats::kmeans(x, k, nstart = 10)$cluster),
    K.max = min(8L, nrow(coords) - 1L),
    B = 20L
  )
  cluster::maxSE(gap$Tab[, "gap"], gap$Tab[, "SE.sim"], method = "Tibs2001SEmax")
}

.spectral_embed <- function(X, k = 2L) {
  # Simple spectral embedding via eigenvectors of the Laplacian.
  # Used as fallback when 'umap' is not available.
  n     <- nrow(X)
  sigma <- stats::median(stats::dist(X))
  if (sigma == 0) sigma <- 1
  W     <- exp(-as.matrix(stats::dist(X))^2 / (2 * sigma^2))
  diag(W) <- 0
  D     <- diag(rowSums(W))
  L     <- D - W
  ev    <- eigen(L, symmetric = TRUE)
  # Smallest eigenvectors (excluding first trivial one)
  idx   <- order(ev$values)[seq(2L, k + 1L)]
  ev$vectors[, idx, drop = FALSE]
}
