#' internal function for SQL methods
#'
#' It takes general queries and returns specific ones by replacing keywords in the first.
#' @param query character, it contain the query structure and the keywords between @'s.
#' @param param character vector, it contain the keywords that would be replaced in query.
#' @usage sqlGsub(query, param=c())
#' @details param must be indexed by the keyword.
#' @return It returns a character to be used in a SQL query.
#' @author Daniel Fischer
#' @examples
#' q="SELECt @col@ FROM @table@ WHERE @cond@"
#' p=c(col="year",table="FIFAWorldCup",cond="winner LIKE Brazil")
#' sqlGsub(q,p)

sqlGsub = function(query, param = c()){
  for(p in names(param) ){
    query = gsub(paste0("@",p,"@"), param[p],query,ignore.case = T)
  }
  return(query)
}

