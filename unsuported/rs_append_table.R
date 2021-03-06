#' Append to redshift table
#'
#' Upload a table to S3 and then load it with redshift appending data to the existing table
#' The table on redshift has to have the same structure and column ordering to work correctly.
#'
#' @param df a data frame
#' @param server_name an RPostgres connection to the redshift server
#' @param table_name the name of the table to replace
#' @param split_files optional parameter to specify amount of files to split into. If not specified will look at amount of slices in Redshift to determine an optimal amount.
#' @param keys athis optional vector contains the variables by which to upsert. If not defined, the upsert becomes an append.
#' @param bucket the name of the temporary bucket to load the data. Will look for AWS_BUCKET_NAME on environment if not specified.
#' @param region the region of the bucket. Will look for AWS_DEFAULT_REGION on environment if not specified.
#' @param iam_role_arn an iam role arn with permissions fot the bucket. Will look for AWS_IAM_ROLE_ARN on environment if not specified. This is ignoring access_key and secret_key if set.
#' @param wlm_slots amount of WLM slots to use for this bulk load http://docs.aws.amazon.com/redshift/latest/dg/tutorial-configuring-workload-management.html
#' @param treads number of threads used to compress the csv files
#' @examples
#' library(DBI)
#'
#' a=data.frame(a=seq(1,10000), b=seq(10000,1))
#' n=head(a,n=5000)
#' n$b=n$a
#' nx=rbind(n, data.frame(a=seq(99999:104000), b=seq(104000:99999)))
#'
#'\dontrun{
#' con <- dbConnect(RPostgres::Postgres(), dbname="dbname",
#' host='my-redshift-url.amazon.com', port='5439',
#' user='myuser', password='mypassword',sslmode='require')
#'
#' rs_append_table(df=nx, dbcon=con, table_name='testTable',
#' bucket="my-bucket", split_files=4, keys=c('a'))
#'
#'}
#' @export
rs_append_table = function(
  df,
  server_name,
  table_name,
  keys,
  split_files,
  bucket="tmp-metricarts",
  region="us-east-1",
  iam_role_arn="",
  wlm_slots=1,
  threads = 0
)
{

  safeLibrary(aws.s3)
  safeLibrary(utils)
  safeLibrary(data.table)
  safeLibrary(dplyr)
  safeLibrary(future)
  safeLibrary(future.apply)
  safeLibrary(R.utils)
  
  if(!inherits(df, 'data.frame')){
    warning("The df parameter must be a data.frame or an object compatible with it's interface")
    return(FALSE)
  }
  numRows = nrow(df)

  if(numRows == 0){
    warning("Empty dataset provided, will not try uploading")
    return(FALSE)
  }

  print(paste0("The provided data.frame has ", numRows, ' rows'))

  dbcon = sqlGetConn(server_name)
  
  if(missing(split_files)){
    split_files = splitDetermine(dbcon)
  }
  split_files = pmin(split_files, numRows)
  
  # Upload data to S3
  prefix = uploadToS3(df, bucket, split_files, region, threads)

  if(wlm_slots>1){
    queryStmt(dbcon,paste0("set wlm_query_slot_count to ", wlm_slots));
  }

  result = tryCatch({
    stageTable=s3ToRedshift(dbcon, table_name, bucket, prefix, region, iam_role_arn,staging=F)
  }, error = function(e){
    stop("Failed to Load Data")
  }, finally = {
    deletePrefix(prefix, bucket, split_files, region)
  })
  
  if(any(c("expression","character") %in% class(server_name)) & !"Pool" %in%  class(dbcon)){sqlClose(dbcon)}
  
  return (result)
}
