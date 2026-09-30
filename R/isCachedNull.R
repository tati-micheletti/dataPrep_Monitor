#' Detect whether a `Cache()`-wrapped function's return value represents NULL
#'
#' `reproducible::Cache()` can't attach its own metadata attributes
#' (`newCache`/`tags`) to a genuine `NULL` -- it's a singleton in R and
#' can't hold attributes -- so when the wrapped function legitimately
#' returns `NULL` (e.g. `buildHabitatSpeciesYear()`/
#' `buildLandscapeSpeciesYear()`'s "too few presences -- skipping" paths),
#' `Cache()` silently coerces it into the literal character string
#' `"NULL"` instead, with the Cache metadata attached to that string. A
#' plain `is.null(result)` check misses this coerced form entirely,
#' letting a bogus placeholder get `saveRDS()`'d as if it were real data
#' -- confirmed as the actual root cause of a real crash, 2026-09-30 (see
#' DECISIONS.md).
#'
#' @param x The (possibly `Cache()`-wrapped) return value to check.
#' @return Logical.
isCachedNull <- function(x) {
  # Value-only comparison (== , not identical()) -- the real Cache()-coerced
  # artifact always carries .Cache/tags/callInCache attributes attached to
  # the "NULL" string, so an attribute-sensitive identical() check would
  # never actually match it.
  is.null(x) || (is.character(x) && length(x) == 1 && !is.na(x) && x == "NULL")
}
