#' Fetch universe of accepted tickers
#'
#' @return List of currently accepted tickers
#' @export
#'
#' @import httr
#'
#' @examples
#' \dontrun{
#' ticker_universe()
#' }
ticker_universe <- function() {
  TICKER_UNIVERSE_URL = paste0("https://numerai-signals-public-data.s3-us-west-2.amazonaws.com","/latest_universe.csv")
  tickers <- strsplit(content(GET(TICKER_UNIVERSE_URL),as="text"),"\n")[[1]][-1]
  return(data.frame(numerai_ticker=tickers))
}


#' Download CSV file with historical targets and ticker universe
#' @param file_path CSV file with predictions that will get uploaded
#' @return List of currently accepted tickers
#' @export
#'
#' @import httr
#'
#' @examples
#' \dontrun{
#' download_validation_data(file_path = "signals_historical_targets.csv")
#' }
download_validation_data <- function(file_path = "signals_historical_targets.csv") {
  HISTORICAL_DATA_URL = paste0("https://numerai-signals-public-data.s3-us-west-2.amazonaws.com","/signals_train_val_bbg.csv")
  httr::GET(HISTORICAL_DATA_URL, progress(),write_disk(file_path, overwrite = TRUE))
  return(file_path)
}