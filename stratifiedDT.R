#' @title Stratified random sampling
#' 
#' @description Applies a linear stretch to any multiband raster \code{link{stack}} or \code{link{brick}}
#' 
#' @param group The grouping column(s). Can be a character vector or the numeric positions of the columns.
#' @param size  The desired sample size. Can be a decimal (proportionate by group) or an integer (same number of samples per group).
#' @param select A named list with optional subsetting statements.
#' @param replace Logical. Should sampling be done with or without replacement.
#' @param bothSets Logical. Should a list be returned. Useful when setting up a “testing” and “training” sampling setup.
#' @return a Raster* object 
#' @author mrdwab source:https://gist.github.com/933ffeaa7a1d718bd10a.git
#' @import data.table

stratifiedDT <- function(indt, group, size, select = NULL, 
                         replace = FALSE, keep.rownames = FALSE,
                         bothSets = FALSE) {
  if (is.numeric(group)) group <- names(indt)[group]
  if (!is.data.table(indt)) indt <- as.data.table(
    indt, keep.rownames = keep.rownames)
  if (is.null(select)) {
    indt <- indt
  } else {
    if (is.null(names(select))) stop("'select' must be a named list")
    if (!all(names(select) %in% names(indt)))
      stop("Please verify your 'select' argument")
    temp <- vapply(names(select), function(x)
      indt[[x]] %in% select[[x]], logical(nrow(indt)))
    indt <- indt[rowSums(temp) == length(select), ]
  }
  df.table <- indt[, .N, by = group]
  df.table
  if (length(size) > 1) {
    if (length(size) != nrow(df.table))
      stop("Number of groups is ", nrow(df.table),
           " but number of sizes supplied is ", length(size))
    if (is.null(names(size))) {
      stop("size should be entered as a named vector")
    } else {
      ifelse(all(names(size) %in% do.call(
        paste, df.table[, group, with = FALSE])),
        n <- merge(
          df.table, 
          setnames(data.table(names(size), ss = size), 
                   c(group, "ss")), by = group),
        stop("Named vector supplied with names ",
             paste(names(size), collapse = ", "),
             "\n but the names for the group levels are ",
             do.call(paste, c(unique(
               df.table[, group, with = FALSE]), collapse = ", "))))
    }
  } else if (size < 1) {
    n <- df.table[, ss := round(N * size, digits = 0)]
  } else if (size >= 1) {
    if (all(df.table$N >= size) || isTRUE(replace)) {
      n <- cbind(df.table, ss = size)
    } else {
      message(
        "Some groups\n---",
        do.call(paste, c(df.table[df.table$N < size][, group, with = FALSE], 
                         sep = ".", collapse = ", ")),
        "---\ncontain fewer observations",
        " than desired number of samples.\n",
        "All observations have been returned from those groups.")
      n <- cbind(df.table, ss = pmin(df.table$N, size))
    }
  }
  setkeyv(indt, group)
  setkeyv(n, group)
  indt[, .RNID := sequence(nrow(indt))]
  out1 <- indt[indt[n, list(
    .RNID = sample(.RNID, ss, replace)), by = .EACHI]$`.RNID`]
  
  if (isTRUE(bothSets)) {
    out2 <- indt[!.RNID %in% out1$`.RNID`]
    indt[, .RNID := NULL]
    out1[, .RNID := NULL]
    out2[, .RNID := NULL]
    list(SAMP1 = out1, SAMP2 = out2)
  } else {
    indt[, .RNID := NULL]
    out1[, .RNID := NULL][]
  }
}