#' @export
#'
param.intsig <- within(list(from = -4, to = 28, n = 501), {
  x = seq(from, to, length.out = n)
  dx = (to - from)/(n - 1)
})


