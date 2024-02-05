fround <- function(x, dig = 2, bm = ""){
  format(round(x, digits = dig), nsmall = dig, big.mark = bm)
}