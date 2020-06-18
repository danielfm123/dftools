#' Encrypt Csv file
#' 
#' Encrypt Csv files
#' @param input input file
#' @param output output csv file
#' @param encrypt_columns columns to encrypt
#' @param algorithm encryption algorithm
#' @param chunk_size batch size
#' @param ... arguments pass to read_delim.
#' @example encryptDelim("iris.csv","iris2.csv",encrypt_columns = "Species", chunk_size = 10,col_names = TRUE, delim = ",")
#' @author Daniel Fischer
#' @export

encryptDelim = function(input,
                      output,
                      encrypt_columns,
                      algorithm = "sha256",
                      chunk_size = 1e5,
                      ...) {

  void = suppressWarnings(file.remove(output))
  
  encriptador = function(dataset,pos){
    print(paste("procesando registro",pos))
    dataset[,encrypt_columns] = map_dfr(dataset[,encrypt_columns],
                                        function(col) {
                                          col2 = paste0("entel",col)
                                          map_chr(col2,digest,algorithm)
                                          })
    write_csv(dataset,output,col_names = pos< chunk_size,append = T)
  }  
  void = read_delim_chunked(input,
                            chunk_size = chunk_size,
                            callback = encriptador,
                            progress = F,
                            ...)
  
}


