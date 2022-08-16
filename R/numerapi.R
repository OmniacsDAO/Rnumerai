#' List of available data files
#' @param round_num Tournament round you are interested in, defaults to the current round.
#' @return list of filenames
#'
#' @export
#'
#' @importFrom jsonlite fromJSON
#'
#' @examples
#' \dontrun{
#' list_datasets(round_num=328)
#' }
list_datasets <- function(round_num=NA) 
{
    qcon <- initialize_queries(auth=FALSE)
    con <- qcon[[1]]
    qry <- qcon[[2]]
    result <- fromJSON(con$exec(qry$queries$list_datasets,list(round = round_num)))$data$listDatasets
    return(result)
}


#' Download specified file for the given round.
#' @param filename File to be downloaded, defaults to live data
#' @param dest_path File path where the file should be stored, Defaults to current directory with filename parameter
#' @param round_num Tournament round you are interested in, defaults to the current round.
#' @return list of filenames
#'
#' @export
#'
#' @importFrom jsonlite fromJSON
#' @importFrom httr GET
#'
#' @examples
#' \dontrun{
#' download_dataset(round_num=328)
#' }
download_dataset <- function(filename="v2/numerai_live_data.csv", dest_path = NA, round_num=NA) 
{
    qcon <- initialize_queries(auth=FALSE)
    con <- qcon[[1]]
    qry <- qcon[[2]]
    result <- fromJSON(con$exec(qry$queries$download_dataset,list(filename = filename, round = round_num)))$data$dataset
    if(is.na(dest_path)) dest_path <- basename(filename)
    httr::GET(result, progress(),write_disk(dest_path, overwrite = TRUE))
    return(result)
}