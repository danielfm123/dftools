#' Cleanup spaces on a text
#'
#' Remove the spaces of text, except the spaces between words.
#' @param text character cleaned.
#' @usage cleanSpaces(text)
#' @return Returns a character without spaces at the begin and the end and replaces multiples spaces by one.
#' @example examples\trimex.R
#' @export
#' @author Daniel Fischer
cleanSpaces <- function (text) {
  if (class(text) != "character") {
    stop("the input must be a character")
  }
  text = gsub("^\\s+|\\s+$", "", text)
  gsub("\\s+", " ", text)
}
