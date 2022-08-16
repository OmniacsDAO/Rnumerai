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


#' Get all information about your account
#' @return User information
#'
#' @export
#'
#' @importFrom jsonlite fromJSON
#' @import lubridate
#'
#' @examples
#' \dontrun{
#' get_account()
#' }
get_account <- function() 
{
    qcon <- initialize_queries(auth=TRUE)
    con <- qcon[[1]]
    qry <- qcon[[2]]
    result <- fromJSON(con$exec(qry$queries$get_account))$data$account
    result$insertedAt <- as_datetime(result$insertedAt)
    result$availableNmr <- as.numeric(result$availableNmr)
    return(result)
}


#' Get mapping of account model names to model ids for convenience
#' @param tournament Tournament ID, 8 for Main, 11 for Signal
#' @return modelname -> model_id list
#'
#' @export
#'
#' @importFrom jsonlite fromJSON
#'
#' @examples
#' \dontrun{
#' get_models()
#' }
get_models <- function(tournament=8) 
{
    qcon <- initialize_queries(auth=TRUE)
    con <- qcon[[1]]
    qry <- qcon[[2]]
    result <- fromJSON(con$exec(qry$queries$get_models))$data$account$models
    result <- result[result$tournament==tournament,]
    resultl <- as.list(result$id)
    names(resultl) <- result$name
    return(resultl)
}


#' Get number of the current active round.
#' @return Number of the current active round
#'
#' @export
#'
#' @importFrom jsonlite fromJSON
#'
#' @examples
#' \dontrun{
#' get_current_round()
#' }
get_current_round <- function() 
{
    qcon <- initialize_queries(auth=FALSE)
    con <- qcon[[1]]
    qry <- qcon[[2]]
    result <- fromJSON(con$exec(qry$queries$get_current_round))$data$rounds$number
    return(result)
}


#' Retrieves information about all rounds
#' @param tournament Tournament ID, 8 for Main, 11 for Signal
#' @return Rounds Information
#'
#' @export
#'
#' @importFrom jsonlite fromJSON
#'
#' @examples
#' \dontrun{
#' get_competitions()
#' }
get_competitions <- function(tournament=8) 
{
    qcon <- initialize_queries(auth=FALSE)
    con <- qcon[[1]]
    qry <- qcon[[2]]
    result <- fromJSON(con$exec(qry$queries$get_competitions,list(tournament = tournament)))$data$rounds
    return(result)
}


#' Set bio field for a model id
#' @param model_id Target model UUID
#' @param bio Bio to change
#' @return If the bio was changed successfully
#'
#' @export
#'
#' @importFrom jsonlite fromJSON
#'
#' @examples
#' \dontrun{
#' set_bio(model_id = get_models()[["bayo"]], bio = "This Model Rocks")
#' }
set_bio <- function(model_id,bio) 
{
    qcon <- initialize_queries(auth=TRUE)
    con <- qcon[[1]]
    qry <- qcon[[2]]
    result <- fromJSON(con$exec(qry$queries$set_bio,list(modelId = model_id,value = bio)))$data$setUserBio
    return(result)
}


#' Set link field for a model id
#' @param model_id Target model UUID
#' @param link URL
#' @param link_text URL Text
#' @return If the link was changed successfully
#'
#' @export
#'
#' @importFrom jsonlite fromJSON
#'
#' @examples
#' \dontrun{
#' set_link(model_id = get_models()[["bayo"]], link = "https://www.google.com",link_text = "Google")
#' }
set_link <- function(model_id,link,link_text) 
{
    qcon <- initialize_queries(auth=TRUE)
    con <- qcon[[1]]
    qry <- qcon[[2]]
    result <- fromJSON(con$exec(qry$queries$set_link,list(modelId = model_id, linkUrl = link, linkText = link_text)))$data$setUserLink
    return(result)
}


#' Get all transactions in your wallet.
#' @return Wallet Txs Data Frame
#'
#' @export
#'
#' @importFrom jsonlite fromJSON
#' @import lubridate
#'
#' @examples
#' \dontrun{
#' wallet_transactions()
#' }
wallet_transactions <- function() 
{
    qcon <- initialize_queries(auth=TRUE)
    con <- qcon[[1]]
    qry <- qcon[[2]]
    result <- fromJSON(con$exec(qry$queries$wallet_transactions))$data$account$walletTxns
    result$time <- as_datetime(result$time)
    result$amount <- as.numeric(result$amount)
    return(result)
}


#' Set a model's submission webhook used in Numerai Compute
#' @param model_id Target model UUID
#' @param webhook The compute webhook to trigger this model
#' @return Confirmation that your webhook has been set
#'
#' @export
#'
#' @importFrom jsonlite fromJSON
#'
#' @examples
#' \dontrun{
#' set_submission_webhook(model_id = get_models()[["bayo"]], webhook = "..")
#' }
set_submission_webhook <- function(model_id,webhook) 
{
    qcon <- initialize_queries(auth=TRUE)
    con <- qcon[[1]]
    qry <- qcon[[2]]
    result <- fromJSON(con$exec(qry$queries$set_submission_webhook,list(modelId = model_id, newSubmissionWebhook = webhook)))$data$setSubmissionWebhook
    return(result)
}


#' Upload predictions to diagnostics from file.
#' @param file_path CSV file with predictions that will get uploaded
#' @param model_id Target model UUID (required for accounts with multiple models)
#' @param df DataFrame to upload, if given both df and file_path, df will be uploaded.
#' @param tournament Tournament ID, 8 for Main, 11 for Signal
#' @return diagnostics_id
#'
#' @export
#'
#' @importFrom jsonlite fromJSON
#' @import httr
#' @importFrom utils write.csv
#'
#' @examples
#' \dontrun{
#' upload_diagnostics(file_path = "prediction.csv",model_id = get_models()[["bayo"]])
#' }
upload_diagnostics <- function(file_path=NA,model_id,df=NA,tournament=8) 
{
    if("data.frame" %in% class(df))
    {
        file_path <- tempfile(fileext=".csv")
        write.csv(df, file_path, row.names = FALSE)
    }
    qcon <- initialize_queries(auth=TRUE)
    con <- qcon[[1]]
    qry <- qcon[[2]]
    aws_slot <- fromJSON(con$exec(qry$queries$diag_aws,list(filename = basename(file_path), tournament = tournament, modelId = model_id)))$data$diagnosticsUploadAuth
    mysubmission <- PUT(url = aws_slot$url,body = upload_file(path = file_path))
    result <- fromJSON(con$exec(qry$queries$diag_submission,list(filename = aws_slot$filename, tournament = tournament, modelId = model_id)))$data$createDiagnostics$id
    return(result)
}


#' Fetch results of diagnostics run
#' @param model_id Target model UUID
#' @param tournament Tournament ID, 8 for Main, 11 for Signal
#' @param diagnostics_id id returned by "upload_diagnostics"
#' @return Diagnostic results
#'
#' @export
#'
#' @importFrom jsonlite fromJSON
#' @import lubridate
#'
#' @examples
#' \dontrun{
#' diagnostics(model_id = get_models()[["bayo"]],tournament=8, diagnostics_id = "")
#' }
diagnostics <- function(model_id,tournament=8,diagnostics_id) 
{
    qcon <- initialize_queries(auth=TRUE)
    con <- qcon[[1]]
    qry <- qcon[[2]]
    result <- fromJSON(con$exec(qry$queries$stats_diagnostics,list(modelId = model_id, id = diagnostics_id)))$data$diagnostics
    result$updatedAt <- as_datetime(result$updatedAt)
    return(result)
}


#' Upload predictions from file.
#' @param file_path CSV file with predictions that will get uploaded
#' @param model_id Target model UUID (required for accounts with multiple models)
#' @param df DataFrame to upload, if given both df and file_path, df will be uploaded.
#' @param tournament Tournament ID, 8 for Main, 11 for Signal
#' @return submission_id
#'
#' @export
#'
#' @importFrom jsonlite fromJSON
#' @import httr
#' @importFrom utils write.csv
#'
#' @examples
#' \dontrun{
#' upload_predictions(file_path = "prediction.csv",model_id = get_models()[["bayo"]])
#' }
upload_predictions <- function(file_path=NA,model_id,df=NA,tournament=8) 
{
    if("data.frame" %in% class(df))
    {
        file_path <- tempfile(fileext=".csv")
        write.csv(df, file_path, row.names = FALSE)
    }
    qcon <- initialize_queries(auth=TRUE)
    con <- qcon[[1]]
    qry <- qcon[[2]]

    ## Compute Parameters
    NUMERAI_COMPUTE_ID <- ifelse(Sys.getenv("NUMERAI_COMPUTE_ID")=="",NA,Sys.getenv("NUMERAI_COMPUTE_ID"))
    TRIGGER_ID <- ifelse(Sys.getenv("TRIGGER_ID")=="",NA,Sys.getenv("TRIGGER_ID"))

    if(tournament==8)
    {
        aws_slot <- fromJSON(con$exec(qry$queries$pred_aws_main,list(filename = basename(file_path), tournament = tournament, modelId = model_id)))$data$submissionUploadAuth
        mysubmission <- PUT(url = aws_slot$url,body = upload_file(path = file_path),add_headers(x_compute_id = NUMERAI_COMPUTE_ID))
        result <- fromJSON(con$exec(qry$queries$pred_submission_main,list(filename = aws_slot$filename, tournament = tournament, modelId = model_id, triggerId = TRIGGER_ID)))$data$createSubmission
    }
    if(tournament==11)
    {
        aws_slot <- fromJSON(con$exec(qry$queries$pred_aws_signal,list(filename = basename(file_path), modelId = model_id)))$data$submissionUploadSignalsAuth
        mysubmission <- PUT(url = aws_slot$url,body = upload_file(path = file_path),add_headers(x_compute_id = NUMERAI_COMPUTE_ID))
        result <- fromJSON(con$exec(qry$queries$pred_submission_signal,list(filename = aws_slot$filename, modelId = model_id, triggerId = TRIGGER_ID)))$data$createSignalsSubmission
    }
    return(result)
}


#' Fetch round model performance of any user
#' @param username User Name
#' @param tournament Tournament ID, 8 for Main, 11 for Signal
#' @return list of round model performance entries
#'
#' @export
#'
#' @importFrom jsonlite fromJSON
#' @import lubridate
#'
#' @examples
#' \dontrun{
#' round_model_performances(username = "bayo")
#' }
round_model_performances <- function(username,tournament=8) 
{
    qcon <- initialize_queries(auth=FALSE)
    con <- qcon[[1]]
    qry <- qcon[[2]]
    if(tournament==8) result <- fromJSON(con$exec(qry$queries$round_model_performances_main,list(username = username)))$data$v3UserProfile$roundModelPerformances
    if(tournament==11) result <- fromJSON(con$exec(qry$queries$round_model_performances_signal,list(username = username)))$data$v2SignalsProfile$roundModelPerformances
    result$roundOpenTime <- as_datetime(result$roundOpenTime)
    result$roundResolveTime <- as_datetime(result$roundResolveTime)
    result$payout <- as.numeric(result$payout)
    result$roundPayoutFactor <- as.numeric(result$roundPayoutFactor)
    result$selectedStakeValue <- as.numeric(result$selectedStakeValue)
    return(result)
}


#' Change stake by `value` NMR
#' @param nmr amount of NMR you want to increase/decrease
#' @param action `increase` or `decrease`
#' @param model_id Target model UUID
#' @param tournament Tournament ID, 8 for Main, 11 for Signal
#' @return Stake change information
#'
#' @export
#'
#' @importFrom jsonlite fromJSON
#' @import lubridate
#'
#' @examples
#' \dontrun{
#' stake_change(nmr=10,action="decrease",model_id = get_models()[["bayo"]],tournament=8)
#' }
stake_change <- function(nmr,action="decrease",model_id,tournament=8)
{
    qcon <- initialize_queries(auth=TRUE)
    con <- qcon[[1]]
    qry <- qcon[[2]]
    result <- fromJSON(con$exec(qry$queries$stake_change,list(value = as.character(nmr), type = action, modelId = model_id,tournamentNumber=tournament)))$data$v2ChangeStake
    result$requestedAmount <- as.numeric(result$requestedAmount)
    result$dueDate <- as_datetime(result$dueDate)
    return(result)
}


#' Change stake type by model
#' @param model_id Target model UUID
#' @param corr_multiplier Multiplier of correlation for returns (Integer)
#' @param tc_multiplier Multiplier of TC for returns (Numeric)
#' @param take_profit TRUE/FALSE Determines whether payouts are returned to user wallet or automatically staked to next round.
#' @param tournament Tournament ID, 8 for Main, 11 for Signal
#' @return Confirmation that payout selection has been updated
#'
#' @export
#'
#' @importFrom jsonlite fromJSON
#' @import lubridate
#'
#' @examples
#' \dontrun{
#' set_stake_type(model_id = get_models()[["bayo"]],corr_multiplier=1,tc_multiplier=3,tournament=8)
#' }
set_stake_type <- function(model_id,corr_multiplier=0,tc_multiplier=0,take_profit=FALSE,tournament=8)
{
    qcon <- initialize_queries(auth=TRUE)
    con <- qcon[[1]]
    qry <- qcon[[2]]
    result <- fromJSON(con$exec(qry$queries$set_stake_type,list(modelId=model_id, corrMultiplier=corr_multiplier, tcMultiplier=tc_multiplier, takeProfit=take_profit, tournamentNumber=tournament)))
    return(result)
}


#' Get the current leaderboard
#' @param tournament Tournament ID, 8 for Main, 11 for Signal
#' @return Leaderboard entries
#'
#' @export
#'
#' @importFrom jsonlite fromJSON
#'
#' @examples
#' \dontrun{
#' get_leaderboard()
#' }
get_leaderboard <- function(tournament=8) 
{
    qcon <- initialize_queries(auth=FALSE)
    con <- qcon[[1]]
    qry <- qcon[[2]]
    offset=0
    leaderboard <- data.frame()
    if(tournament==8)
    {
        while(TRUE)
        {
            result <- fromJSON(con$exec(qry$queries$get_leaderboard_main,list(limit = 1000, offset=offset)))$data$v2Leaderboard
            leaderboard <- rbind(leaderboard,result)
            if(nrow(result)!=1000) break()    
            offset <- offset+1000
        }
    }
    if(tournament==11)
    {
        while(TRUE)
        {
            result <- fromJSON(con$exec(qry$queries$get_leaderboard_signal,list(limit = 1000, offset=offset)))$data$signalsLeaderboard
            leaderboard <- rbind(leaderboard,result)
            if(nrow(result)!=1000) break()    
            offset <- offset+1000
        }
    }
    return(leaderboard)
}


#' Submission status of the last submission associated with the account
#' @param model_id Target model UUID
#' @param tournament Tournament ID, 8 for Main, 11 for Signal
#' @return Submission status and stats
#'
#' @export
#'
#' @importFrom jsonlite fromJSON
#'
#' @examples
#' \dontrun{
#' model_id = get_models()[["bayo"]]
#' submission_status(model_id = model_id,tournament=8)
#' }
submission_status <- function(model_id,tournament=8) 
{
    qcon <- initialize_queries(auth=TRUE)
    con <- qcon[[1]]
    qry <- qcon[[2]]
    if(tournament==8) result <- fromJSON(con$exec(qry$queries$submission_status_main,list(modelId = model_id)))$data$model$latestSubmissionV2
    if(tournament==11) result <- fromJSON(con$exec(qry$queries$submission_status_signal,list(modelId = model_id)))$data$model$latestSignalsSubmission
    return(result)
}