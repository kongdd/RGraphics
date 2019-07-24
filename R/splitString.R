#' Split text into multiple lines
#' 
#' Splits a single string into multiple lines (by inserting line breaks) so that 
#' the output will fit within the current viewport.
#' 
#' @param text The string to split.
#' 
#' @examples 
#' figure8.20()
#' 
#' @export
splitString <- function(text) {
    strings <- strsplit(text, " ")[[1]]
    newstring <- strings[1]
    linewidth <- stringWidth(newstring)
    gapwidth <- stringWidth(" ")
    availwidth <- convertWidth(unit(1, "npc"), "in", valueOnly=TRUE) 
    # npg: Normalised Parent Coordinates (the default)
    for (i in 2:length(strings)) {
        width <- stringWidth(strings[i])
        if (convertWidth(linewidth + gapwidth + width, "in", valueOnly=TRUE) < availwidth) {
            sep <- " "
            linewidth <- linewidth + gapwidth + width
        } else {
            sep <- "\n"
            linewidth <- width
        }
        newstring <- paste(newstring, strings[i], sep=sep)
    }
    newstring
}

#' splitTextGrob
#' 
#' @inheritParams splitString
#' @param ... Arguments passed to the [grob](grid::grid.grob()) function.
#' 
#' @export
splitTextGrob <- function(text, ...) {
    grob(text=text, cl="splitText", ...)
}

#' @export 
drawDetails.splitText <- function(x, recording) {
    grid.text(splitString(x$text),
              x=0, y=1, just=c("left", "top")) 
}

