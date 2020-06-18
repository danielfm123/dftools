#' @rdname SQLserverconnection
#' @export
sqlClose = function(server_name){
  dbDisconnect(server_name)
}