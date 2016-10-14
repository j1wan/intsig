#' IntSig
#'
#' Kernel Density Estimation for Log of Inter-event Times, This function receives
#' @param intervals a vector of inter-event times in seconds
#' @export
#' @examples
#' intervals = c(1,5,2,3,15,2,354374,124135,352,234,14,5331)
#' KDE.int(intervals)
#'
IntSig <- function(intervals) {
  intervals[intervals == 0] <- 1
  x <- log2(intervals)
  h <- bw.nrd0(x)

  # thresholding
  if (h < 0.5) h <- 0.5 else if (h > 1) h <- 1

  w <- 1/pnorm(0, mean = x, sd = h, lower.tail = F)

  # P <- density(x, from = .param.int$from, to = .param.int$to, n = .param.int$n, bw = h, weights = w/length(x), kernel = 'g')
  P <- density(x, from = param.intsig$from, to = param.intsig$to, n = param.intsig$n, bw = h, kernel = 'g')
  P$y[P$x < 0] <- 0
  return(P$y)
}


#' @export
.kTimeLog <- log2(c(1, 60, 3600, 3600*24, 3600*24*7, 3600*24*30, 3600*24*365))

#' @export
.kTimeLab <- c('1sec', '1min', '1hr', '1d', '1w', '1m', '1yr')


#' HistInt
#'
#' blabla
#' @param intervals a vector of inter-event times
#' @export
#'
HistInt <- function(intervals, main = '', ...) {
  intervals[intervals==0] = 1
  hist(
    log(intervals, 2),
    xlim = log2(c(1, 3600*24*365)),
    ylim = c(0, 0.3),
    breaks = param.intsig$from:param.intsig$to,
    probability = T,
    main = main,
    axes = F,
    border = NA,
    col='gray',
    xaxs='i', #, yaxs='i'
    ...
  )
  axis(2, ...)
  axis(1, .kTimeLog, .kTimeLab, ...)
}

#' @export
LineIntSig <- function(intsig, col = rgb(1, 0, 0, 1/2), ...) {
  lines(x = param.intsig$x, y = intsig, col = col, ...)
}

#' @export
PlotIntSig <- function(intervals, ...) {
  HistInt(intervals, ...)
  LineIntSig(IntSig(intervals), ...)
}


#' Timestamp plotting
#'
#' Plot timestamps with vertical lines
#' @param UNIX timestamps/epochs of an event series
#' @export
PlotEvent <- function(timestamps, alpha = 1/4, ...) {
  #timestamps <- as.POSIXct(timestamps, origin = '1970-01-01')
  plot(NULL,
       xlim = range(timestamps),
       ylim = c(0, 1),
       axes = F,
       ylab = '',
       xaxs = 'i',
       yaxs = 'i',
       ...)

  abline(v = timestamps, col = rgb(20/255, 135/255, 200/255, alpha = alpha), ...)
  box(...)
}
