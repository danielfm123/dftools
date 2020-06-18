#' Create a table from scratch, guessing the table schema
#'
#'
#' @param df a data frame
#' @param dbcon an RPostgres connection to the redshift server
#' @param table_name the name of the table to create
#' @param wlm_slots amount of WLM slots to use for this bulk load http://docs.aws.amazon.com/redshift/latest/dg/tutorial-configuring-workload-management.html
#' @param sortkeys Column or columns to sort the table by
#' @param sortkey_style Sortkey style, can be compound or interleaved http://docs.aws.amazon.com/redshift/latest/dg/t_Sorting_data-compare-sort-styles.html
#' @param distkey Distkey column, can only be one, if chosen the table is distributed among clusters according to a hash of this column's value.
#' @param distkey_style Distkey style, can be even or all, for the key distribution use the distkey parameter. http://docs.aws.amazon.com/redshift/latest/dg/t_Distributing_data.html
#' @param compression Add encoding for columns whose compression algorithm is easy to guess, for the rest you should upload it to Redshift and run ANALYZE COMPRESSION
#'
#' @examples
#' library(DBI)
#'
#' a=data.frame(a=seq(1,10000), b=seq(10000,1))
#'
#'\dontrun{
#' con <- dbConnect(RPostgres::Postgres(), dbname="dbname",
#' host='my-redshift-url.amazon.com', port='5439',
#' user='myuser', password='mypassword',sslmode='require')
#'
#' rs_create_table(df=a, dbcon=con, table_name='testTable',
#' bucket="my-bucket", split_files=4)
#'
#' }
#' @export
rs_create_table = function(
    df,
    server_name,
    table_name=deparse(substitute(df)),
    split_files,
    wlm_slots=1,
    sortkeys,
    sortkey_style='compound',
    distkey,
    distkey_style='even',
    compression=T
    )
  {
  
  safeLibrary(aws.s3)
  safeLibrary(utils)
  safeLibrary(data.table)
  safeLibrary(dplyr)
  safeLibrary(future)
  safeLibrary(future.apply)
  safeLibrary(R.utils)

  tableSchema = rs_create_statement(df, table_name = table_name, sortkeys=sortkeys,
  sortkey_style = sortkey_style, distkey=distkey, distkey_style = distkey_style,
  compression = compression)

  dbcon = sqlGetConn(server_name)
  
  queryStmt(dbcon, tableSchema)
  # result = s3ToRedshift(dbcon, table_name, bucket, prefix, region, access_key, secret_key, iam_role_arn,staging=F)
  
  if(any(c("expression","character") %in% class(server_name)) & !"Pool" %in%  class(dbcon)){sqlClose(dbcon)}
  
  return(result)

}
