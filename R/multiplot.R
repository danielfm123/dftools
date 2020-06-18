#' Print multiple plots inside one
#'
#' Take multiple ggplot objects, print them together as you wish and storage the result
#'  as pdf file.
#' @usage multiplot(..., plotlist=NULL, file="", cols=1, layout=NULL)
#' @param ... single ggplot plots separated by comas.
#' @param plotlist a list of ggplot plots.
#' @param file a character with the file's names that would be storaged.
#' @param cols the number of columns of the final layout. Number of rows will
#' adapt to it.
#' @param layout a matrix with a custom layout.
#' @details Storagement is made by dev.print(pdf,file), so it follows its ways to change
#'  directories.
#' @author Martin Vicencio
#' @return It returns the multiple plot of classes \code{"gtable","gTree","grob","gDesc"}
#' @example examples\multiplotex.R
#' @export
multiplot<- function(..., plotlist=NULL, file="", cols=1, layout=NULL) {
  safeLibrary(grid)
  safeLibrary(ggplot2)
  safeLibrary(gridExtra)
  
  plots <- c(list(...), plotlist)
  
  numPlots = length(plots)
  
  if (is.null(layout)) {
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }
  
  if (numPlots==1) {
    print(plots[[1]])
    
  } else {
    grid.arrange(arrangeGrob(grobs = plots,layout_matrix = layout))
    }
  if(file!=""){dev.print(pdf,file)}
  
}