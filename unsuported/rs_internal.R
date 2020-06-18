# Internal utility functions used by the redshift tools
uploadToS3 = function (data, bucket, split_files, region, threads){
  safeLibrary(future)
  safeLibrary(future.apply)
  # safeLibrary(purrr)
  safeLibrary(dplyr)
  safeLibrary(data.table)
  safeLibrary(R.utils)
  safeLibrary(httr)
  start_time = Sys.time()
  safeLibrary(parallel)
  aws.signature::use_credentials()
  prefix=paste0(sample(letters,50,replace = T),collapse = "")
  
  proxy = Sys.getenv("http_proxy")
  suppressWarnings({  Sys.unsetenv(c("http_proxy","https_proxy")) })
  
  if(!bucket_exists(bucket, region=region,use_https=F)){
    stop("Bucket does not exist")
  }
  
  print("Preparing data...")
  toSave = mutate_if(data,is.factor,as.character) %>% 
    mutate_if(is.character,enc2utf8)
  toSave = suppressWarnings(split(toSave, seq(1:split_files)))
  toSave = lapply(1:split_files, function(i){
    list(tmpFile = tempfile(),
         gzFile = tempfile(),
         split = toSave[[i]],
         s3Name = paste(bucket, "/", prefix, ".", formatC(i, width = 4, format = "d", flag = "0"), sep=""))
  })
  print("Creating CSV files...")
  setDTthreads(0)
  void = lapply(toSave, function(saved){
    fwrite(saved[["split"]], saved[["tmpFile"]], na='', row.names=F, quote=T,dateTimeAs = "write.csv")
  })
  
  print("Compressing CSV files...")
  if(threads == 1){
    plan(sequential)
  }else{
    plan(multisession, workers = min(detectCores(),split_files) )
  }
  void = future_lapply(toSave, function(saved){
    gzip(saved[["tmpFile"]],destname=saved[["gzFile"]])
  })
  
  print("Uploading compressed data...")
  plan(multisession)
  void = lapply(toSave, function(saved){
    # print(paste("Uploading", saved[["tmpFile"]], "to" ,saved[["s3Name"]]))
    # future({
    put_object(file = saved[["gzFile"]], object = saved[["s3Name"]], bucket = "",  region=region, use_https = F,httr::use_proxy(""))
    suppressWarnings(file.remove(saved[["tmpFile"]],saved[["gzFile"]]))
    return(saved[["s3Name"]])
    # })
  })
  
  Sys.setenv("http_proxy" = proxy, "https_proxy" = proxy) 
  
  # void = map(void,value)
  end_time = Sys.time()
  print(paste("Data uploaded to S3 in",format(end_time - start_time)))
  return(prefix)
}

deletePrefix = function(prefix, bucket, split_files, region){
  print('Deleting Temporary Files...')
  safeLibrary("aws.s3")
  safeLibrary(future)
  safeLibrary(future.apply)
  # safeLibrary(purrr)
  plan(multiprocess, workers = split_files)
  aws.signature::use_credentials()
  prev_reg=Sys.getenv('AWS_DEFAULT_REGION')
  Sys.setenv( 'AWS_DEFAULT_REGION'=region)

  proxy = Sys.getenv("http_proxy")
  suppressWarnings({  Sys.unsetenv(c("http_proxy","https_proxy")) })
  
  result = lapply(1:split_files,function(i){
    s3Name=paste(prefix, ".", formatC(i, width = 4, format = "d", flag = "0"), sep="")
    #print(paste("Deleting", s3Name))
    # future({
    delete_object(s3Name, bucket, region=region, use_https = F,httr::use_proxy(""))
    # })
  })
  Sys.setenv("http_proxy" = proxy, "https_proxy" = proxy) 
  
  # void = map(result,value)
  Sys.setenv( 'AWS_DEFAULT_REGION'=prev_reg)
}

queryDo = function(dbcon, query){
  safeLibrary("DBI")
  dbGetQuery(dbcon, query)
}

queryStmt = function(dbcon, query){
  safeLibrary("DBI")
  if(inherits(dbcon, 'JDBCConnection')){
    RJDBC::dbSendUpdate(dbcon, query)
  }else{
    dbExecute(dbcon, query)
  }
}

splitDetermine = function(dbcon){
  print("Getting number of slices from Redshift")
  slices = queryDo(dbcon,"select count(*) from stv_slices")
  slices[1] = round(slices[1])
  if(slices[1] < 16){ # Use more if low number of slices
    split_files = 16
  }else{
    split_files = unlist(slices[1])
  }
  print(sprintf("%s slices detected, will split into %s files", slices, split_files))
  return(split_files)
}


s3ToRedshift = function(dbcon, table_name, bucket, prefix, region, iam_role_arn,staging=T){
  aws.signature::use_credentials()
  start_time = Sys.time()
  if(staging){
    # Create temporary table for staging data
    stageTable=paste0(sample(letters,16),collapse = "")
    queryStmt(dbcon, sprintf("create temp table %s (like %s)", stageTable, table_name))
  }else{
    stageTable = table_name
  }
  print("Copying data from S3 into Redshift")
  copyStr = "copy %s from 's3://%s/%s.' region '%s' csv gzip ignoreheader 1 emptyasnull COMPUPDATE FALSE %s"
  
  # Use IAM Role if available
  if (nchar(iam_role_arn) > 0) {
    credsStr = sprintf("iam_role '%s'", iam_role_arn)
  } else {
    if(Sys.getenv('AWS_SESSION_TOKEN') == ""){
      credsStr = sprintf("credentials 'aws_access_key_id=%s;aws_secret_access_key=%s'", 
                         Sys.getenv('AWS_ACCESS_KEY_ID'), 
                         Sys.getenv('AWS_SECRET_ACCESS_KEY'))
    }else{
      credsStr = sprintf("credentials 'aws_access_key_id=%s;aws_secret_access_key=%s;token=%s'", 
                         Sys.getenv('AWS_ACCESS_KEY_ID'), 
                         Sys.getenv('AWS_SECRET_ACCESS_KEY'),
                         Sys.getenv('AWS_SESSION_TOKEN'))
    }
  }
  statement = sprintf(copyStr, stageTable, bucket, prefix, region, credsStr)
  tryCatch({
    queryStmt(dbcon,statement)
    end_time = Sys.time()
    print(paste("Data injected to Redshift in",format(end_time - start_time)))
  },
  error = function(e){
    stl_load_errors = sqlGetQuery(dbcon, "select * from stl_load_errors where filename like 's3://tmp-metricarts/@prefix@%'", c(prefix = prefix))
    print(stl_load_errors)
  })
  
  return(stageTable)
}
