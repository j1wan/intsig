#' Strip and Split
#'
#' strip the first and last elements, then split the rest
#' @param l a vector
#' @param n.split the number of bins to split into
#' @param n.strip the number (or proportion) of elements to strip at each end
#' @export
#'
StripAndSplit <- function(l, n.split = 10, n.strip = 0) {
  if (n.strip < 1)
    n.strip <- floor(length(l) * n.strip)
  l <- l[(n.strip + 1):(length(l) - n.strip)]
  n <- length(l)
  m <- floor(n/n.split)
  return(lapply(1:n.split, function(i) l[((i-1)*m+1):(i*m)]))
}


#' @export
#'
mid.w1 <- function(intervals, win.size) {
  n = length(intervals)
  s = (n - win.size) %/% 2
  return(intervals[(s+1):(s+win.size)])
}

#' @export
#'
mid.w2 <- function(intervals, win.size) {
  n = length(intervals)
  m = n %/% 2
  return(list(intervals[(m - win.size+1):m], intervals[(m+1):(m+win.size)]))
}


#' @export
#'
SelfDist <- function(dataset, win.size, sample.size) {
  candidates <- which(dataset$n.post > 2*win.size)
  targets <- sample(candidates, min(length(candidates), sample.size))
  data <- dataset[targets, ]

  tau <- lapply(data$intervals, function(x) {
    mid.w2(as.integer(unlist(strsplit(x, '-'))), win.size)
  })

  P <- lapply(tau, function(x) lapply(x, KDE.int))
  d.self <- sapply(P, function(P.u) JSDist(P.u[[1]], P.u[[2]], .param.int$dx))

  return(d.self)
}

#' @export
#'
RefDist <- function(dataset, win.size, sample.size) {
  # ramdon sample 1000 target users
  candidates <- which(dataset$n.post > win.size)
  targets <- sample(candidates, min(length(candidates), sample.size))
  data <- dataset[targets, ]

  # get inter-event times
  tau <- lapply(data$intervals, function(x) {
    mid.w1(as.integer(unlist(strsplit(x, '-'))), win.size)
  })

  # calculate interval signature for each user
  P <- lapply(tau, KDE.int)

  # calculate reference distance
  d.ref <- outer(1:length(P), 1:length(P), Vectorize(function(u, v) {
    if (u < v)
      return(JSDist(P[[u]], P[[v]], .param.int$dx))
    return(NA)
  }))

  return(d.ref[!is.na(d.ref)])
}

#' @export
FalseAcceptRate <- function(d.ref) {
  prob <- density(d.ref, na.rm = TRUE, from = 0, to = 1, n = 1000)$y / 1000
  return(cumsum(prob))
}

#' @export
FalseRejectRate <- function(d.self) {
  prob <- density(d.self, na.rm = TRUE, from = 0, to = 1, n = 1000)$y / 1000
  return(rev(cumsum(rev(prob))))
}

#' @export
EqualErrorRate <- function(d.self, d.ref) {
  far <- FalseAcceptRate(d.ref)
  frr <- FalseRejectRate(d.self)
  threshold <- which.min(abs(far-frr))
  return(min(far[threshold], frr[threshold]))
}
