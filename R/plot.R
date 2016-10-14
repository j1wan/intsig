#' HistInterval
#'
#' blabla
#' @param intervals a vector of inter-event times
#' @export
#'
HistInterval <- function(intervals, main='', ...){ # TODO: add log
  intervals[intervals==0] = 1
  hist(
    log(intervals, 2),
    xlim = log2(c(1, 3600*24*365)),
    ylim=c(0, 0.3),
    breaks=0:30, probability=T,
    main=main,
    axes=F,
    border=NA,
    col='gray', xaxs='i', #, yaxs='i'
    ...
  )
  axis(2, ...)
  axis(1, .kTimeAt, .kTimeLab, ...)
}

#' @export
.kTimeAt <- log2(c(1, 60, 3600, 3600*24, 3600*24*7, 3600*24*30, 3600*24*365))

#' @export
.kTimeLab <- c('1sec', '1min', '1hr', '1d', '1w', '1m', '1yr')



#' @export
.kYears <- 2000:2020

#' @export
.date.iso.str <- as.vector(outer(.kYears, 01:12, function(y, m) paste(y, m, '01', sep = '-')))

#' @export
.date.abb.str <- as.vector(outer(.kYears, 01:12, function(y, m) paste(month.abb[m], sprintf('%02d', y%%1000))))

#' @export
.date.uts <- as.numeric(as.POSIXct(.date.iso.str))

#' @export
.window.label <- parse(text = paste0('italic(W) [', 1:10, ']'))

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
# PlotEvent(1000000000 + sample.int(500000000, 500), main = 'AA')
