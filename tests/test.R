
library(RGraphics)
for (i in ls("package:RGraphics", pattern="^figure")) {
  cat(i, "\n")
  postscript(paste("RGraphicsTest", i, ".ps", sep=""),
             fonts=c("mono", "sans", "serif", "symbol"))
  try(do.call(i, list()))
  dev.off()
}

