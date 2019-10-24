#' Gets the Numerai Password
#'
#' @return Your Numerai Password, if set
#' @export
#' @examples
#' \dontrun{
#' get_password()
#' }
get_password <- function() {
  env <- Sys.getenv("NUMERAIPASS")
  if (!identical(env, "")) return(env)

  if (!interactive()) {
    stop("Please set the environment variable NUMERAIPASS to your Numerai Password", call. = FALSE)
  }

  message("Please enter your Numerai Password and press enter:")

  pass <- readline(": ")

  if (identical(pass, "")) {
    stop("Invalid Password", call. = FALSE)
  }

  message("Updating NUMERAIPASS environment variable.")
  Sys.setenv(NUMERAIPASS = pass)

  return(pass)
}

#' Sets the Numerai Password
#'
#' @name set_password
#' @param pass The Numerai Password
#' @return A boolean TRUE if the password was successfully set
#' @export
#' @examples
#' \dontrun{
#' set_password("abcdefghijklmnop")
#' }
set_password <- function(pass) {
  if (identical(pass, "")) {
    stop("Invalid Password", call. = FALSE)
  }

  Sys.setenv(NUMERAIPASS = pass)

  return(TRUE)
}

#' Gets the Numerai Public ID
#'
#' @return Your Numerai Public ID, if set
#' @export
#' @examples
#' \dontrun{
#' get_public_id()
#' }
get_public_id <- function() {
  env <- Sys.getenv("NUMERAIID")
  if (!identical(env, "")) return(env)

  if (!interactive()) {
    stop("Please set the environment variable NUMERAIID to your Numerai Public ID", call. = FALSE)
  }

  message("Please enter your Numerai Public ID and press enter:")

  id <- readline(": ")

  if (identical(id, "")) {
    stop("Invalid Public ID", call. = FALSE)
  }

  message("Updating NUMERAIID environment variable.")
  Sys.setenv(NUMERAIID = id)

  return(id)
}

#' Sets the Numerai Public ID
#'
#' @name set_public_id
#' @param id The Numerai Public ID
#' @return A boolean TRUE if the ID was successfully set
#' @export
#' @examples
#' \dontrun{
#' set_public_id("abcdefghijklmnop")
#' }
set_public_id <- function(id) {
  if (identical(id, "")) {
    stop("Invalid Public ID", call. = FALSE)
  }

  Sys.setenv(NUMERAIID = id)

  return(TRUE)
}

#' Gets the Numerai API key
#'
#' @return Your Numerai API key, if set
#' @export
#' @examples
#' \dontrun{
#' get_api_key()
#' }
get_api_key <- function() {
  env <- Sys.getenv("NUMERAIKEY")
  if (!identical(env, "")) return(env)

  if (!interactive()) {
    stop("Please set the environment variable NUMERAIKEY to your Numerai API key", call. = FALSE)
  }

  message("Please enter your Numerai API key and press enter:")

  key <- readline(": ")

  if (identical(key, "")) {
    stop("Invalid API key", call. = FALSE)
  }

  message("Updating NUMERAIKEY environment variable.")
  Sys.setenv(NUMERAIKEY = key)

  return(key)
}

#' Sets the Numerai API key
#'
#' @name set_api_key
#' @param key The Numerai API key
#' @return A boolean TRUE if the key was successfully set
#' @export
#' @examples
#' \dontrun{
#' set_api_key("abcdefghijklmnop")
#' }
set_api_key <- function(key) {
  if (identical(key, "")) {
    stop("Invalid API key", call. = FALSE)
  }

  Sys.setenv(NUMERAIKEY = key)

  return(TRUE)
}

#' Function to run a raw GraphQL query on the API interface
#'
#' @name run_query
#' @param query The graphQL query to run on the API as a string in single quotes
#' @param id The public id of the Numerai application
#' @param key The Numerai API key
#' @return The parsed json content returned from the request
#' @export
#' @import httr
#' @examples
#' \dontrun{
#' ## Run Custom GraphQL code from R
#' custom_query <- "query queryname {
#' rounds (number:82) {
#' closeTime
#' }
#' }"
#' run_query(query=custom_query)$data
#' }
run_query <- function(query, id = get_public_id(), key = get_api_key())
{
	## httr's POST call to retrieve results from numerai graphql API
	raw <- POST(
					url = "https://api-tournament.numer.ai",
					body = list(query=query),
					encode="json",
					add_headers(
									Authorization=paste0("Token ",id,"$",key),
									"Content-type"="application/json",
									Accept="application/json"
								)
	)
	return(content(raw, "parsed"))
}

#' Function to download the Numerai Tournament data
#'
#' @name download_data
#' @param location The directory path in which to store the data
#' @param tournament The name of the tournament, Default is KAZUTSUGI and is not case-sensitive. Since at the moment the datasets are same for all tournaments this parameter can be left blank.
#' @return A list containing the training and tournament data objects
#' @export
#' @import lubridate
#' @import httr
#' @importFrom utils unzip
#' @importFrom utils read.csv
#' @examples
#' \dontrun{
#' ## Directory where data files and prediction files to be saved
#' ## Put custom directory path or use the current working directory
#' data_dir <- tempdir()
#'
#' ## Download data set for current competition
#' data <- download_data(data_dir,tournament="KAZUTSUGI")
#' data_train <- data$data_train
#' data_tournament <- data$data_tournament
#' }
download_data <- function(location = tempdir(),tournament="KAZUTSUGI")
{
	## Match tournament ID
	tournament_id <- match(tolower(tournament),tolower(c("BERNIE","","","KEN","CHARLES","FRANK","HILLARY","KAZUTSUGI")))
	if(is.na(tournament_id)) stop("Tournament Name doesn't match")

	## Get download link
	download_link_query <- paste0('{dataset(tournament:',tournament_id,')}')
	query_pass <- run_query(query=download_link_query)
	download_link <- query_pass$data$dataset

	message("Downloading Data...\n")

	## Download File
	filename <- file.path(location, paste0("numerai_dataset_", today(), ".zip"))
	result <- GET(download_link, write_disk(filename, overwrite = TRUE))

	## Unzip the file
	unzip(filename,overwrite = TRUE,exdir = location)
	message("Finished downloading data\nReading dataset now...")

	## Read and return the data set
	data_train <- read.csv(file.path(location, "numerai_training_data.csv"))
	data_tournament <- read.csv(file.path(location, "numerai_tournament_data.csv"))
	return(list(data_train=data_train,data_tournament=data_tournament))
}

#' Function to submit the Numerai Tournament predictions
#'
#' @name submit_predictions
#' @param submission The data frame of predictions to submit. This should have two columns named "id" & "prediction_kazutsugi"
#' @param location The location in which to store the predictions
#' @param tournament The name of the tournament, Default is Kazutsugi and is not case-sensitive
#' @return The submission id for the submission made
#' @export
#' @import lubridate
#' @import httr
#' @importFrom utils write.csv
#' @examples
#' \dontrun{
#' submission_id <- submit_predictions(submission_data,tournament="Kazutsugi")
#' }
submit_predictions <- function(submission, location = tempdir(),tournament="Kazutsugi")
{
	## Match tournament ID
	tournament_id <- match(tolower(tournament),tolower(c("BERNIE","","","KEN","CHARLES","FRANK","HILLARY","KAZUTSUGI")))
	if(is.na(tournament_id)) stop("Tournament Name doesn't match")
	if(!all(names(submission)==c("id","prediction_kazutsugi"))) stop("Column names should be id & prediction_kazutsugi")
	#names(submission)[2] <- paste0(names(submission)[2],"_",tolower(tournament))

	## Write out the file
	submission_filename <- file.path(location, paste0("submission_data_", today(), ".csv"))
	write.csv(submission, submission_filename, row.names = FALSE)

	## Get a slot on AWS for our submission
	aws_slot_query <- paste0('query aws_slot_query {
							submissionUploadAuth (filename : "submission_data.csv",tournament:',tournament_id,'){
								filename,
								url
							}
						}')
	query_pass <- run_query(query=aws_slot_query)

	## Upload the predictions
	mysubmission <- PUT(
							url = query_pass$data$submissionUploadAuth$url,
							body = upload_file(path = submission_filename)
						)

	## Register our submission and get evaluation for it
	register_submission_query <- paste0(
											'mutation register_submission_query {
												createSubmission (filename : "',query_pass$data$submissionUploadAuth$filename,'",tournament:',tournament_id,'){id}
											}'
										)
	query_pass <- run_query(query=register_submission_query)

	## If error
	if(!is.null(query_pass$errors[[1]]$message)) stop(query_pass$errors[[1]]$message)

	## Return submission id
	message(paste("Submitted Prediction with id",query_pass$data$createSubmission$id))
	return(query_pass$data$createSubmission$id)
}


#' Get information about a submission from a submission id
#'
#' @name status_submission_by_id
#' @param sub_id The id of the submission
#' @return A list containing information about the given submission id
#' @export
#' @examples
#' \dontrun{
#' status_submission_by_id(submission_id)
#' }
status_submission_by_id <- function(sub_id)
{
	sub_stat_query <- paste0(
								'query sub_stat_query {
									submissions (id : "',sub_id,'"){
										filename,
										validationCorrelation,
										liveCorrelation,
										round{
											number
										},
										selected,
										consistency,
										concordance {
											pending
											value
										}
									}
								}'
							)
	query_pass <- run_query(query=sub_stat_query)

	## If not evaluated yet
	if(is.null(query_pass$data$submissions[[1]]$concordance))
	{
		return("Not Scored Yet")
	}

	## If evaluated submission
	result <- list(
					Submission_ID = sub_id,
					Round_Number = query_pass$data$submissions[[1]]$round$number,
					Filename = query_pass$data$submissions[[1]]$filename,
					Selected = query_pass$data$submissions[[1]]$selected,
					Consistency = query_pass$data$submissions[[1]]$consistency,
					Concordance = ifelse(!query_pass$data$submissions[[1]]$concordance$pending,query_pass$data$submissions[[1]]$concordance$value,"Pending"),
					Validation_Correlation = query_pass$data$submissions[[1]]$validationCorrelation,
					Live_Correlation = query_pass$data$submissions[[1]]$liveCorrelation
					)
	return(result)
}

#' Get information about your username
#'
#' @name user_info
#' @return A list containing information about user
#' @export
#' @examples
#' \dontrun{
#' uinfo <- user_info()
#' names(uinfo)
#' uinfo$Latest_Submission
#' }
user_info <- function()
{
	user_info_query <-	'query user_info_query {
							user {
								id
								username
								email
								insertedAt
								status
								banned
								mfaEnabled
								availableNmr
								availableEth
								availableUsd
								assignedEthAddress
								customEthAddresses
							}
						}'

	query_pass <- run_query(query=user_info_query)

	result <- list(
						Email_Address = query_pass$data$user$email,
						Username = query_pass$data$user$username,
						ID = query_pass$data$user$id,
						Generated_Time = query_pass$data$user$insertedAt,
						Current_Status = query_pass$data$user$status,
						MFA_Enabled = query_pass$data$user$mfaEnabled,
						Banned = query_pass$data$user$banned,
						Assigned_ETH_Address = query_pass$data$user$assignedEthAddress,
						Custom_ETH_Address = unlist(query_pass$data$user$customEthAddresses),
						Balances = data.frame(USD=query_pass$data$user$availableUsd,NMR = query_pass$data$user$availableNmr)
					)
	return(result)
}

#' Get current round and it's closing time
#'
#' @name current_round
#' @param tournament The name of the tournament, Default is Kazutsugi and is not case-sensitive
#' @return Returns the current round number and it's closing times
#' @export
#' @examples
#' \dontrun{
#' current_round()
#' }
current_round <- function(tournament="Kazutsugi")
{
	## Match tournament ID
	tournament_id <- match(tolower(tournament),tolower(c("BERNIE","","","KEN","CHARLES","FRANK","HILLARY","KAZUTSUGI")))
	if(is.na(tournament_id)) stop("Tournament Name doesn't match")

	current_round = paste0('query current_round {
						rounds(number:0,tournament:',tournament_id,') {
							number
							closeTime
							closeStakingTime
						}
					}')
	query_pass <- run_query(query=current_round)
	return(c(Round_Number=query_pass$data$rounds[[1]]$number,Close_Time=query_pass$data$rounds[[1]]$closeTime,Close_Staking_Time=query_pass$data$rounds[[1]]$closeStakingTime))
}

#' Stake NMR
#'
#' @name stake_nmr
#' @param value The amount of NMR to stake
#' @param mfa_code The mfa code
#' @param password Your password
#' @return The transaction hash for stake made
#' @export
#' @examples
#' \dontrun{
#' stake_tx_hash <- stake_nmr(value = 1)
#' }
stake_nmr <- function(value, mfa_code = "", password = "")
{
	stake_query <- paste0(
							'mutation stake_query {
								v2Stake(code:"',mfa_code,'"
								password:"',password,'"
								value:"',value,'"
								){
									txHash
								}}'
							)
	query_pass <- run_query(query=stake_query)
	return(query_pass)
}

#' Release NMR
#'
#' @name release_nmr
#' @param value The amount of NMR to release
#' @param mfa_code The mfa code
#' @param password Your password
#' @return The transaction hash for release request
#' @export
#' @examples
#' \dontrun{
#' release_tx_hash <- release_nmr(value = 1)
#' }
release_nmr <- function(value, mfa_code = "", password = "")
{
	release_query <- paste0(
							'mutation release_query {
								v2ReleaseStakeRequest(code:"',mfa_code,'"
								password:"',password,'"
								value:"',value,'"
								){
									txHash
								}}'
							)
	query_pass <- run_query(query=release_query)
	return(query_pass)
}


#' Get Information for a Round Number
#'
#' @name round_stats
#' @param round_number Round Number for which information to fetch
#' @param tournament The name of the tournament, Default is Kazutsugi and is not case-sensitive
#' @return List containing general round information
#' @export
#' @examples
#' \dontrun{
#' round_stats(round_number=177)
#' }
round_stats <- function(round_number,tournament="Kazutsugi")
{
	## Match tournament ID
	tournament_id <- match(tolower(tournament),tolower(c("BERNIE","","","KEN","CHARLES","FRANK","HILLARY","KAZUTSUGI")))
	if(is.na(tournament_id)) stop("Tournament Name doesn't match")

	round_stats_query <- paste0(
									'query round_stats_query {
									rounds(number:',round_number,',tournament:',tournament_id,'){
										number
										tournament
										openTime
										resolvedGeneral
										resolvedStaking
										closeTime
										closeStakingTime
									}}'
								)
	query_pass <- run_query(query=round_stats_query)

	round_data <- query_pass$data$rounds[[1]]
	result_info <- data.frame(
								Round_Number = round_data$number,
								Tournament_Name = c("BERNIE","","","KEN","CHARLES","FRANK","HILLARY","KAZUTSUGI")[round_data$tournament],
								Open_Time = round_data$openTime,
								Close_Time = round_data$closeTime,
								Close_Staking_Time = ifelse(is.null(round_data$closeStakingTime),NA,round_data$closeStakingTime),
								If_Resolved = round_data$resolvedGeneral
  							)

	return(result_info)
}

#' Get Current leaderboard
#'
#' @name leaderboard
#' @return List containing leaderboard
#' @export
#' @examples
#' \dontrun{
#' leaderboard()
#' }
leaderboard <- function()
{
	leaderboard_query <- paste0(
									'query leaderboard_query {
									v2Leaderboard{
										bonusPerc
										nmrStaked
										prevRank
										rank
										reputation
										tier
										username
									}}'
								)
	query_pass <- run_query(query=leaderboard_query)

	result_info <- data.frame(
								Username = sapply(query_pass$data$v2Leaderboard,"[[",1),
								Tier = sapply(query_pass$data$v2Leaderboard,"[[",2),
								Reputation = sapply(query_pass$data$v2Leaderboard,"[[",3),
								Rank = sapply(query_pass$data$v2Leaderboard,"[[",4),
								Previous_Rank = sapply(query_pass$data$v2Leaderboard,"[[",5),
								NMR_Staked = sapply(query_pass$data$v2Leaderboard,"[[",6),
								Bonus_Percentage = sapply(query_pass$data$v2Leaderboard,"[[",7)
  							)

	return(result_info)
}
