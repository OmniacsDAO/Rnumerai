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
#' @param tournament The name of the tournament, Default is Nomi and is not case-sensitive. Since at the moment the datasets are same for all tournaments this parameter can be left blank.
#' @return A list containing the training and tournament data objects
#' @export
#' @importFrom lubridate today
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
#' data <- download_data(data_dir,tournament="Nomi")
#' data_train <- data$data_train
#' data_tournament <- data$data_tournament
#' }
download_data <- function(location = tempdir(),tournament="NOMI")
{
	## Match tournament ID
	tournament_id <- match(tolower(tournament),tolower(c("BERNIE","","","KEN","CHARLES","FRANK","HILLARY","NOMI")))
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
#' @param submission The data frame of predictions to submit. This should have two columns named "id" & "prediction"
#' @param location The location in which to store the predictions
#' @param tournament The name of the tournament, Default is Nomi and is not case-sensitive
#' @param model_id Target model UUID (required for accounts with multiple models)
#' @param prefix The prefix to use for the submission csv file
#' @return The submission id for the submission made
#' @export
#' @importFrom lubridate today
#' @import httr
#' @importFrom utils write.csv
#' @examples
#' \dontrun{
#' submission_id <- submit_predictions(submission_data,tournament="Nomi")
#' }
submit_predictions <- function(submission, location = tempdir(), tournament = "Nomi", model_id = NULL, prefix = tournament)
{

    if(is.null(model_id)) stop("Must provide a model_id")

    ## Read the Trigger ID env variable
    trigger_id = Sys.getenv("TRIGGER_ID")
    if (trigger_id == "") {
        trigger_id = NULL
    }

    ## Match tournament ID
    tournament_id <- match(tolower(tournament),tolower(c("NOMI","SIGNALS"))) + 7
    if(is.na(tournament_id)) stop("Tournament Name doesn't match")

    if(tournament_id == 8 && !all(names(submission)==c("id","prediction"))) stop("Column names should be id & prediction")

    ## Write out the file
    date <- gsub("-","_",Sys.Date())
    submission_filename <- file.path(location, paste0(prefix, "_submission_", date, ".csv"))
    write.csv(submission, submission_filename, row.names = FALSE)

    ## Get a slot on AWS for our submission
    if(tournament_id == 8){
        aws_slot_query <- paste0('query aws_slot_query {
							submissionUploadAuth (filename : "',paste0(prefix, "_submission_", date, ".csv"),'",
							                      tournament: ',tournament_id,',
							                      modelId:"',model_id,'"){
								filename,
								url
							}
						}')
    } else {
        aws_slot_query <- paste0('query aws_slot_query {
							submissionUploadSignalsAuth (filename : "',paste0(prefix, "_submission_", date, ".csv"),'",
							                      modelId:"',model_id,'"){
								filename,
								url
							}
						}')
    }

    query_pass <- run_query(query=aws_slot_query)

    if(tournament_id == 8) numerai_aws_url = query_pass$data$submissionUploadAuth$url else numerai_aws_url = query_pass$data$submissionUploadSignalsAuth$url


    ## Upload the predictions
    mysubmission <- httr::PUT(
        url = numerai_aws_url,
        body = upload_file(path = submission_filename)
    )

    if (mysubmission$status_code != 200) {
        stop(paste0("Uploading submission failed with status code ", mysubmission$status_code))
    }

    ## Register our submission and get evaluation for it
    if(tournament_id == 8){
        register_submission_query <- paste0(
            'mutation register_submission_query {
					      							createSubmission (filename : "',query_pass$data$submissionUploadAuth$filename,'",
					      							tournament: ',tournament_id,',
					      							triggerId: "',trigger_id,'",
					      							modelId:"',model_id,'"){id}
					      						}'
        )
    } else {
        register_submission_query <- paste0(
            'mutation register_submission_query {
					      							createSignalsSubmission (filename : "',query_pass$data$submissionUploadSignalsAuth$filename,'",
					      							triggerId: "',trigger_id,'",
					      							modelId:"',model_id,'"){id}
					      						}'
        )
    }

    query_pass <- run_query(query=register_submission_query)

    if(tournament_id == 8) query_id <- query_pass$data$createSubmission$id else query_id <- query_pass$data$createSignalsSubmission$id

    ## If error
    if(!is.null(query_pass$errors[[1]]$message)) stop(query_pass$errors[[1]]$message)


    ## Return submission id
    message(paste("Submitted Prediction with id", query_id))
    return(query_id)
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
										selected
									}
								}'
							)
	query_pass <- run_query(query=sub_stat_query)

	## If evaluated submission
	result <- list(
					Submission_ID = sub_id,
					Round_Number = query_pass$data$submissions[[1]]$round$number,
					Filename = query_pass$data$submissions[[1]]$filename,
					Selected = query_pass$data$submissions[[1]]$selected,
					Validation_Correlation = query_pass$data$submissions[[1]]$validationCorrelation,
					Live_Correlation = query_pass$data$submissions[[1]]$liveCorrelation
					)
	return(result)
}

#' Get information about your username
#'
#' @name user_info
#' @param model_id The id of the model
#' @return A list containing information about user
#' @export
#' @examples
#' \dontrun{
#' uinfo <- user_info()
#' names(uinfo)
#' uinfo$Latest_Submission
#' }
user_info <- function(model_id = NULL)
{
    .Deprecated("account_info")
	user_info_query <-	paste0('query user_info_query {
							user(modelId: "', model_id, '") {
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
							}
						}')

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

#' Get models associated with your account
#'
#' @name get_models
#' @return A list containing information about the models
#' @export
#' @examples
#' \dontrun{
#' models <- get_models()
#' }
get_models <- function() {
    models_query = 'query {
            account {
              models {
                id
                name
              }
            }
          }'

    query_pass <- run_query(query=models_query)

    model_list <- query_pass$data$account$models
    model_ids <- sapply(model_list, `[[`, 1)
    names(model_ids) <- sapply(model_list, `[[`, 2)


    return(model_ids)
}

#' Get information about your account
#'
#' @name account_info
#' @return A list containing information about account
#' @export
#' @examples
#' \dontrun{
#' ainfo <- account_info()
#' names(ainfo)
#' ainfo$Latest_Submission
#' }
account_info <- function()
{
    account_info_query <-	'query {
                            account {
                              username
                              walletAddress
                              availableNmr
                              email
                              id
                              mfaEnabled
                              status
                              insertedAt
                              models {
                                id
                                name
                                submissions {
                                  id
                                  filename
                                }
                                v2Stake {
                                  status
                                  txHash
                                }
                              }
                              apiTokens {
                                name
                                public_id
                                scopes
                              }
            }
          }'

    query_pass <- run_query(query=account_info_query)

    return(query_pass$data$account)
}

#' Get current round and it's closing time
#'
#' @name current_round
#' @param tournament The name of the tournament, Default is Nomi and is not case-sensitive
#' @return Returns the current round number and it's closing times
#' @export
#' @examples
#' \dontrun{
#' current_round()
#' }
current_round <- function(tournament="Nomi")
{
	## Match tournament ID
	tournament_id <- match(tolower(tournament),tolower(c("BERNIE","","","KEN","CHARLES","FRANK","HILLARY","NOMI")))
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
#' @param model_id The id of the model with which to stake
#' @param mfa_code The mfa code
#' @param password Your password
#' @return The transaction hash for stake made
#' @export
#' @examples
#' \dontrun{
#' stake_tx_hash <- stake_nmr(value = 1)
#' }
stake_nmr <- function(value, model_id = NULL, mfa_code = "", password = "")
{
	stake_query <- paste0(
							'mutation stake_query {
								v2Stake(code:"',mfa_code,'"
								password:"',password,'"
								value:"',value,'"
								modelId:"',model_id,'"
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
#' @param model_id The id of the model with which to stake
#' @param mfa_code The mfa code
#' @param password Your password
#' @return The transaction hash for release request
#' @export
#' @examples
#' \dontrun{
#' release_tx_hash <- release_nmr(value = 1)
#' }
release_nmr <- function(value, model_id = NULL, mfa_code = "", password = "")
{
	release_query <- paste0(
							'mutation release_query {
								v2ReleaseStakeRequest(code:"',mfa_code,'"
								password:"',password,'"
								value:"',value,'"
								modelId:"',model_id,'"
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
#' @param tournament The name of the tournament, Default is Nomi and is not case-sensitive
#' @return List containing general round information
#' @export
#' @examples
#' \dontrun{
#' round_stats(round_number=177)
#' }
round_stats <- function(round_number,tournament="Nomi")
{
	## Match tournament ID
	tournament_id <- match(tolower(tournament),tolower(c("BERNIE","","","KEN","CHARLES","FRANK","HILLARY","NOMI")))
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
								Tournament_Name = c("BERNIE","","","KEN","CHARLES","FRANK","HILLARY","KAZUTSUGI","NOMI")[round_data$tournament],
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
										stakedRank
									}}'
								)
	query_pass <- run_query(query=leaderboard_query)

	result_info <- data.frame(
								Username = sapply(query_pass$data$v2Leaderboard,"[[","username"),
								Tier = sapply(query_pass$data$v2Leaderboard,"[[","tier"),
								Staked_Rank = sapply(query_pass$data$v2Leaderboard,function(x) ifelse(is.null(x[["stakedRank"]]),NA,x[["stakedRank"]])),
								Reputation = sapply(query_pass$data$v2Leaderboard,"[[","reputation"),
								Rank = sapply(query_pass$data$v2Leaderboard,"[[","rank"),
								Previous_Rank = sapply(query_pass$data$v2Leaderboard,function(x) ifelse(is.null(x[["prevRank"]]),NA,x[["prevRank"]])),
								NMR_Staked = sapply(query_pass$data$v2Leaderboard,"[[","nmrStaked"),
								Bonus_Percentage = sapply(query_pass$data$v2Leaderboard,"[[","bonusPerc")
  							)

	return(result_info)
}

#' Get User Performance
#'
#' @name user_performance
#' @param user_name UserName for which performance metrics to get
#' @return Get User Performance
#' @export
#' @examples
#' \dontrun{
#' user_performance(user_name="theomniacs")
#' }
user_performance <- function(user_name="theomniacs")
{
	user_query <- paste0(
									'query user_query {
						         v2UserProfile(username:"',tolower(user_name),'"){
    										dailyUserPerformances {
                          corrRep
                          date
                          fncRep
                          mmcRep
                          payoutPending
                          payoutSettled
                          rank
                          stakeValue
    										}
    										dailySubmissionPerformances {
    											correlation
      										date
      										roundNumber
      										mmc
      										correlationWithMetamodel
      									}
      									totalStake
									  }
                  }'
								)
	query_pass <- run_query(query=user_query)

	user_performance <- data.frame(
	                            Date = sapply(query_pass$data$v2UserProfile$dailyUserPerformances, function(x) ifelse(is.null(x[["date"]]), NA, x[["date"]])),
	                            CORRRep = sapply(query_pass$data$v2UserProfile$dailyUserPerformances,  function(x) ifelse(is.null(x[["corrRep"]]), NA, x[["corrRep"]])),
	                            MMCRep = sapply(query_pass$data$v2UserProfile$dailyUserPerformances,  function(x) ifelse(is.null(x[["mmcRep"]]), NA, x[["mmcRep"]])),
	                            FNCRep = sapply(query_pass$data$v2UserProfile$dailyUserPerformances,  function(x) ifelse(is.null(x[["fncRep"]]), NA, x[["fncRep"]])),
	                            Rank = sapply(query_pass$data$v2UserProfile$dailyUserPerformances,  function(x) ifelse(is.null(x[["rank"]]), NA, x[["rank"]])),
								NMR_Staked = sapply(query_pass$data$v2UserProfile$dailyUserPerformances,function(x) ifelse(is.null(x[["stakeValue"]]),NA,x[["stakeValue"]]))
  							)
	submission_performance <- data.frame(
								Round_Number = sapply(query_pass$data$v2UserProfile$dailySubmissionPerformances,"[[","roundNumber"),
								Date = sapply(query_pass$data$v2UserProfile$dailySubmissionPerformances, function(x) ifelse(is.null(x[["date"]]), NA, x[["date"]])),
								Round_Correlation = sapply(query_pass$data$v2UserProfile$dailySubmissionPerformances,function(x) ifelse(is.null(x[["correlation"]]),NA,x[["correlation"]])),
								MMC = sapply(query_pass$data$v2UserProfile$dailySubmissionPerformances,function(x) ifelse(is.null(x[["mmc"]]),NA,x[["mmc"]])),
								Correlation_With_MM = sapply(query_pass$data$v2UserProfile$dailySubmissionPerformances,function(x) ifelse(is.null(x[["correlationWithMetamodel"]]),NA,x[["correlationWithMetamodel"]]))
  							)

	return(list(User_Performance = user_performance,
					Submission_Performance=submission_performance,
					Net_Earnings = query_pass$data$v2UserProfile$netEarnings,
					Historical_Net_Earnings_USD = query_pass$data$v2UserProfile$historicalNetUsdEarnings,
					Historical_Net_Earnings_NMR = query_pass$data$v2UserProfile$historicalNetNmrEarnings
				))

}

#' Get the performance of the user over time
#'
#' @name user_performance_data
#'
#' @param username A vector of one or more usernames
#' @param dates A vector of one or more dates to consider. If NULL, use all data
#' @param round_aggregate If TRUE, aggregate the submission data by round
#'
#' @export
#'
#' @import dplyr
#' @importFrom lubridate ymd_hms
#'
user_performance_data <- function(username, dates = NULL, round_aggregate = TRUE) {
    Reputation <- Average_Daily_Correlation <- Date <- MMC <- NULL
    Round_Correlation <- Correlation_With_MM <- Round_Number <- Username <- NULL

    ## Prepare data set and preformatiing
    data <- lapply(username, function(usr) {
        mylst <- user_performance(usr)
        mylst$Username <- usr

        return(mylst)
    })

    user_data <- lapply(data, function(x) {
        x$User_Performance %>%
            mutate_if(is.factor, as.character) %>%
            mutate(Username = x$Username)
    }) %>%
        bind_rows() %>%
        mutate(Date = as_date(ymd_hms(Date)))

    submission_data <- lapply(data, function(x) {
        x$Submission_Performance %>%
            mutate_if(is.factor, as.character) %>%
            mutate_at(vars(Round_Correlation:Correlation_With_MM), as.numeric) %>%
            mutate(Username = x$Username)
    }) %>%
        bind_rows() %>%
        mutate(Date = as_date(ymd_hms(Date)))

    if (round_aggregate) {
        submission_data <- submission_data %>%
            group_by(Round_Number, Username) %>%
            summarise(Date = max(Date),
                      Round_Correlation = mean(Round_Correlation, na.rm = TRUE),
                      MMC = mean(MMC, na.rm = TRUE),
                      Correlation_With_MM = mean(Correlation_With_MM, na.rm = TRUE)) %>%
            ungroup()
    }

    if (!is.null(dates)) {
        user_data <- user_data %>%
            filter(Date %in% dates)
        submission_data <- submission_data %>%
            filter(Date %in% dates)
    }

    return(list(user_data = user_data, submission_data = submission_data))
}

#' Get the valid dataset for a particular metric
#'
#' @name get_valid_data
#'
#' @param username A vector of one or more usernames
#' @param metric Based on the metric selected, get the correct data
#' @param merge If TRUE, merge the results into a single username
#' @param round_aggregate If TRUE, aggregate the submission data by round
#'
#' @import dplyr
#'
get_valid_data <- function(username, metric, merge = FALSE, round_aggregate = TRUE) {
    Date <- NMR_Staked <- Leaderboard_Bonus <- Payout_NMR <- NULL
    Reputation <- Rank <- Average_Daily_Correlation <- Round_Correlation <- MMC <- Correlation_With_MM <- NULL

    user_metrics <- c("Reputation", "Rank", "NMR_Staked", "Leaderboard_Bonus",
                      "Payout_NMR", "Average_Daily_Correlation")
    sub_metrics <- c("Round_Correlation", "MMC", "Correlation_With_MM")

    if (!(metric %in% c(user_metrics, sub_metrics))) {
        stop(paste0("Metric not found. Valid metrics are: ", paste(c(user_metrics, sub_metrics), collapse = ", ")))
    }

    all_data <- user_performance_data(username, round_aggregate = round_aggregate)

    if (metric %in% user_metrics) {
        time_data <- all_data$user_data

        if (merge) {
            time_data <- time_data %>%
                group_by(Date) %>%
                summarise(
                    NMR_Staked = sum(NMR_Staked),
                    Leaderboard_Bonus = sum(Leaderboard_Bonus),
                    Payout_NMR = sum(Payout_NMR),
                    Reputation = mean(Reputation),
                    Rank = mean(Rank),
                    Average_Daily_Correlation = mean(Average_Daily_Correlation),
                    Username = "multiple"
                )
        }
    } else {
        time_data <- all_data$submission_data

        if (merge) {
            time_data <- time_data %>%
                group_by(Date) %>%
                summarise(
                    Round_Correlation = mean(Round_Correlation),
                    MMC = mean(MMC),
                    Correlation_With_MM = mean(Correlation_With_MM),
                    Username = "multiple"
                )
        }
    }

    return(time_data)
}

#' Get the performance of the user over time
#'
#' @name performance_over_time
#'
#' @param username A vector of one or more usernames
#' @param metric A statistic, as a character vector.
#' @param merge If TRUE, combine the usernames into a single result
#' @param outlier_cutoff The absolute value above which points will be displayed
#' @param round_aggregate If TRUE, aggregate the submission data by round
#'
#' @export
#'
#' @import ggplot2
#' @import dplyr
#' @importFrom lubridate as_date
#'
performance_over_time <- function(username, metric, merge = FALSE, outlier_cutoff = if (round_aggregate) 0 else 0.0125, round_aggregate = TRUE)
{
    Relevant <- `.` <- NULL

    time_data <- get_valid_data(username, metric, merge = merge, round_aggregate = round_aggregate) %>%
        mutate(Relevant = .[[metric]])

    outlier_data <- time_data %>%
        filter(abs(Relevant) >= outlier_cutoff)

    myx <- "Date"
    if (round_aggregate && "Round_Number" %in% names(time_data)) myx <- "Round_Number"

    p1 <- ggplot(data = time_data, aes_string(x = myx, y = metric, colour = "Username")) +
        geom_smooth() +
        geom_point(data = outlier_data, size = 2) +
        ylab(tools::toTitleCase(gsub("_", " ", metric))) +
        xlab(tools::toTitleCase(gsub("_", " ", myx))) +
        theme_bw()

    if (!round_aggregate) {
        p1 <- p1 + scale_x_date(date_breaks = "1 months", date_labels = "%b %y")
    }

    return(p1)
}

#' Get the performance of the user as a distribution
#'
#' @name performance_distribution
#'
#' @param username A vector of one or more usernames
#' @param metric A statistic, as a character vector.
#' @param merge If TRUE, combine the usernames into a single result
#' @param round_aggregate If TRUE, aggregate the submission data by round
#'
#' @export
#'
#' @import ggplot2
#' @import dplyr
#'
performance_distribution <- function(username, metric, merge = FALSE, round_aggregate = TRUE)
{
    Username <- Relevant <- `.` <- Label <- NULL

    hist_data <- get_valid_data(username, metric, merge = merge, round_aggregate = round_aggregate) %>%
        mutate(Relevant = .[[metric]])

    hist_avg <- hist_data %>%
        group_by(Username) %>%
        summarise(Relevant = mean(Relevant, na.rm = TRUE)) %>%
        mutate(Label = paste0("Avg: ", round(Relevant, digits = 4)))

    step_size <- function(metric) {
        if (metric %in% c("Average_Daily_Correlation", "Round_Correlation")) {
            function(y) seq(floor(min(y, na.rm = TRUE)), ceiling(max(y, na.rm = TRUE)), by = .01)
        } else if (metric == "Reputation") {
            function(y) seq(floor(min(y, na.rm = TRUE)), ceiling(max(y, na.rm = TRUE)), by = .1)
        } else {
            waiver()
        }
    }

    ggplot(data = hist_data, aes_string(x = metric, fill = "Username")) +
        geom_histogram(colour = "grey60") +
        geom_vline(data = hist_avg, aes(xintercept = Relevant), linetype = "dashed") +
        geom_label(data = hist_avg, vjust = 1.1, hjust = -0.1, aes(x = -Inf, y = Inf, label = Label), show.legend = FALSE) +
        scale_x_continuous(breaks = step_size(metric)) +
        xlab(tools::toTitleCase(gsub("_", " ", metric))) +
        ylab("Number of Models") +
        theme_bw() +
        theme(legend.position = "off") +
        facet_wrap(~Username, nrow=length(username))
}

#' Get the summary statistics for
#'
#' @name summary_statistics
#'
#' @param username A vector of one or more usernames
#' @param dates A vector of one or more dates to consider. If NULL, use all data
#' @param round_aggregate If TRUE, aggregate the submission data by round
#'
#' @export
#'
#' @import ggplot2
#' @import dplyr
#' @importFrom purrr map
#' @importFrom stats cor
#' @importFrom tidyr gather
#' @importFrom tidyr spread
#' @importFrom utils tail
#'
summary_statistics <- function(username, dates = NULL, round_aggregate = TRUE) {
    Date <- Variable <- Value <- Username <- NMR_Staked <- Payout_NMR <- NULL
    Leaderboard_Bonus <- `.` <- Round_Correlation <- Average_Daily_Correlation <- Correlation_With_MM <- MMC <- NULL

    all_data <- user_performance_data(username, dates = dates, round_aggregate = round_aggregate)

    summary_stat_user <- all_data$user_data %>%
        group_by(Username) %>%
        arrange(Date) %>%
        summarise(`Current Amount Staked` = tail(NMR_Staked, 1),
                  `Total Net Payout` = sum(Payout_NMR, na.rm = TRUE),
                  `Average Payout` = mean(Payout_NMR, na.rm = TRUE),
                  `Total Bonus` = sum(Leaderboard_Bonus, na.rm = TRUE),
                  `Average Daily Correlation` = mean(Average_Daily_Correlation, na.rm = TRUE))

    summary_stat_sub <- all_data$submission_data %>%
        group_by(Username) %>%
        summarise(`Average Round Correlation` = mean(Round_Correlation, na.rm = TRUE),
                  `Average MMC` = mean(MMC, na.rm = TRUE),
                  `Average Correlation With MM` = mean(Correlation_With_MM, na.rm = TRUE))

    summary_stat <- summary_stat_user %>%
        left_join(summary_stat_sub)

    if (length(username) == 1) return(summary_stat)

    cc_stat <- all_data$user_data %>%
        select(Date, Username, Average_Daily_Correlation) %>%
        gather(key = Variable, value = Value, 3:ncol(.)) %>%
        spread(key = Username, value = Value) %>%
        split(.$Variable) %>%
        map(.f = function(x) cor(x[,-(1:2)], use = "pairwise.complete.obs"))

    return(list(stats = summary_stat,
                corrs = cc_stat))

}
