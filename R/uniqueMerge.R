#' verges dataframes avoiding duplicates
#'
#'Merges data frames or data tables by columns without all possible combination of rows. So if there are
#'N rows on a table that matches with M rows on the other, this function would only merge the first with
#'the first, second with second, and go on... Finaly it would be |M-N| not merged rows.
#'@usage uniqueMerge(x,y,by=intersect(names(x),names(y)),
#'by.x=by,by.y=by,all=T,all.x=all,all.y=all,...)
#'@param x,y data frames, or data tables to be coerced to one.
#'@param by,by.x,by.y specifications of the columns used for merging.
#'@param all,all.x,all.y  logicals, TRUE by default. Adds not matching rows to the output
#'filling the fields with NAs
#'@param ... arguments to be passed to or from methods. As sort, suffixes or incomparables.
#'@details This function calls merge so it admits all the ways to use that function but without the
#' combination for matching rows.
#'@author Daniel Fischer
#'@example examples\uniqueMergeex.R
#'@export

uniqueMerge = function(x,y,by = intersect(names(x), names(y)), by.x = by,by.y = by,all=T,all.x = all, all.y = all,...){
  if("data.table" %in% class(x) & "data.table" %in% class(y)){
    x[,key_merge := 1:nrow(.SD),by=by]
    y[,key_merge := 1:nrow(.SD),by=by]
  }else{
    x = x %>% group_by(c(by.x)) %>% mutate(key_merge = row_number()) %>% ungroup()
    y = y %>% group_by(c(by.y)) %>% mutate(key_merge = row_number()) %>% ungroup()
  }
  tabla = merge(x,y,by.x = c(by.x,"key_merge"),by.y = c(by.y,"key_merge"),all.x = all.x, all.y = all.y,...)
  tabla$key_merge = NULL
  return(tabla)
}

