
rescaleRGB <- function(matrix) {
  minMatrix = min(matrix)
  maxMatrix = max(matrix)
  return ((matrix- minMatrix + 1e-6) / (-minMatrix + maxMatrix + 1e-5))
}


# Reference https://stackoverflow.com/questions/31800687/how-to-get-a-pixel-matrix-from-grayscale-image-in-r
pixmatplot <- function (img, ...) {
  d <- dim(img)
  xcoord <- t(expand.grid(1:d[1], 1:d[2]))
  xcoord <- t(xcoord/d)
  par(mar = rep(1, 4))
  plot(0, 0, type = "n", xlab = "", ylab = "", axes = FALSE,
       xlim = c(0, 1), ylim = c(0, 1), ...)
  rect(xcoord[, 2L] - 1/d[2L], 1 - (xcoord[, 1L] - 1/d[1L]),
       xcoord[, 2L], 1 - xcoord[, 1L], col = img, border = "transparent")
}

