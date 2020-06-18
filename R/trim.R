#' errases t
#'
#' Remove the spaces of text, except the spaces between words.
#' @param text character to be trimed.
#' @usage trim(text)
#' @return Returns a character without spaces at the begin and the end.
#' @example examples\trimex.R
#' @export
#' @author Daniel Fischer
trim <- function (text) {
  if (class(text) != "character") {
    stop("the input must be a character")
  }
  gsub("^\\s+|\\s+$", "", text)
}
