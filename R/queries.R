#' @import ghql
initialize_queries <- function(auth=FALSE) 
{
    ## Initialize Client Connection
    con <- GraphqlClient$new(url = "https://api-tournament.numer.ai")
    if(auth) con <- GraphqlClient$new(url = "https://api-tournament.numer.ai",headers = list(Authorization = paste0("Token ",get_public_id(),"$",get_api_key())))

    ## Prepare New Query
    qry <- Query$new()
    
    ##################################################################
    ## Get all information about your account
    ##################################################################
    qry$query(
        'get_account',
        'query {
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
    )
    ##################################################################
    ##################################################################


    ##################################################################
    ## Get mapping of account model names to model ids for convenience
    ##################################################################
    qry$query(
        'get_models',
        'query {
            account {
              models {
                id
                name
                tournament
              }
            }
          }'
    )
    ##################################################################
    ##################################################################


    ##################################################################
    ## Get number of the current active round.
    ##################################################################
    qry$query(
        'get_current_round',
        'query {
              rounds(tournament: 8,number: 0) {
                number
              }
            }'
    )
    ##################################################################
    ##################################################################


    ##################################################################
    ## Retrieves information about all competitions
    ##################################################################
    qry$query(
        'get_competitions',
        'query($tournament: Int!) {
              rounds(tournament: $tournament) {
                number
                resolveTime
                openTime
                resolvedGeneral
                resolvedStaking
              }
            }'
    )
    ##################################################################
    ##################################################################


    ##################################################################
    ## Set bio field for a model id.
    ##################################################################
    qry$query(
        'set_bio',
        'mutation($modelId: String, $value: String!) {
            setUserBio(modelId: $modelId, value: $value)
        }'
    )
    ##################################################################
    ##################################################################


    ##################################################################
    ## Set link field for a model id.
    ##################################################################
    qry$query(
        'set_link',
        'mutation($modelId: String, $linkUrl: String!, $linkText: String!) {
            setUserLink(modelId: $modelId, linkUrl: $linkUrl, linkText: $linkText)
        }'
    )
    ##################################################################
    ##################################################################


    ##################################################################
    ## Get all transactions in your wallet
    ##################################################################
    qry$query(
        'wallet_transactions',
        'query {
            account {
              walletTxns {
                amount
                from
                status
                to
                time
                tournament
                txHash
                type
              }
            }
          }'
    )
    ##################################################################
    ##################################################################


    ##################################################################
    ## Set a model's submission webhook used in Numerai Compute.
    ##################################################################
    qry$query(
        'set_submission_webhook',
        'mutation($modelId: String, $newSubmissionWebhook: String!) {
            setSubmissionWebhook(modelId: $modelId, newSubmissionWebhook: $newSubmissionWebhook)
        }'
    )
    ##################################################################
    ##################################################################


    ##################################################################
    ## Diagnostics Upload Main/Signal
    ##################################################################
    qry$query(
        'diag_aws',
        'query($filename: String!, $tournament: Int!, $modelId: String) {
                diagnosticsUploadAuth(filename: $filename, tournament: $tournament, modelId: $modelId) {
                    filename
                    url
                }
            }'
    )
    qry$query(
        'diag_submission',
        'mutation($filename: String!, $tournament: Int!, $modelId: String) {
                createDiagnostics(filename: $filename, tournament: $tournament, modelId: $modelId) {
                    id
                }
            }'
    )
    ##################################################################
    ##################################################################


    ##################################################################
    ## Fetch results of diagnostics run
    ##################################################################
    qry$query(
        'stats_diagnostics',
        'query($id: String, $modelId: String!) {
            diagnostics(id: $id, modelId: $modelId) {
                erasAcceptedCount
                examplePredsCorrMean
                message
                perEraDiagnostics {
                    era
                    examplePredsCorr
                    validationCorr
                    validationFeatureCorrMax
                    validationFeatureNeutralCorr
                    validationMmc
                }
                status
                trainedOnVal
                updatedAt
                validationCorrMean
                validationCorrMeanRating
                validationCorrPlusMmcMean
                validationCorrPlusMmcMeanRating
                validationCorrPlusMmcSharpe
                validationCorrPlusMmcSharpeDiff
                validationCorrPlusMmcSharpeDiffRating
                validationCorrPlusMmcSharpeRating
                validationCorrPlusMmcStd
                validationCorrPlusMmcStdRating
                validationCorrSharpe
                validationCorrSharpeRating
                validationCorrStd
                validationCorrStdRating
                validationFeatureCorrMax
                validationFeatureCorrMaxRating
                validationFeatureNeutralCorrMean
                validationFeatureNeutralCorrMeanRating
                validationMaxDrawdown
                validationMaxDrawdownRating
                validationMmcMean
                validationMmcMeanRating
                validationMmcSharpe
                validationMmcSharpeRating
                validationMmcStd
                validationMmcStdRating
              }
            }'
    )
    ##################################################################
    ##################################################################


    ##################################################################
    ## Predictions Upload Main
    ##################################################################
    qry$query(
        'pred_aws_main',
        'query($filename: String!, $tournament: Int!, $modelId: String) {
                submissionUploadAuth(filename: $filename, tournament: $tournament, modelId: $modelId) {
                    filename
                    url
                }
            }'
    )
    qry$query(
        'pred_submission_main',
        'mutation($filename: String!, $tournament: Int!, $modelId: String, $triggerId: String) {
                createSubmission(filename: $filename, tournament: $tournament, modelId: $modelId, triggerId: $triggerId, source: "Rnumerai") {
                    id
                }
            }'
    )
    ##################################################################
    ##################################################################


    ##################################################################
    ## Predictions Upload Signal
    ##################################################################
    qry$query(
        'pred_aws_signal',
        'query($filename: String!, $modelId: String) {
                submissionUploadSignalsAuth(filename: $filename, modelId: $modelId) {
                    filename
                    url
                }
            }'
    )
    qry$query(
        'pred_submission_signal',
        'mutation($filename: String!, $modelId: String, $triggerId: String) {
                createSignalsSubmission(filename: $filename, modelId: $modelId, triggerId: $triggerId, source: "Rnumerai") {
                    id
                    firstEffectiveDate
                }
            }'
    )
    ##################################################################
    ##################################################################


    ##################################################################
    ## Fetch round model performance of a user
    ##################################################################
    qry$query(
        'round_model_performances_main',
        'query($username: String!) {
            v3UserProfile(modelName: $username) {
              roundModelPerformances {
                corr
                corr20d
                corr20dPercentile
                corrMultiplier
                corrPercentile
                corrWMetamodel
                tc
                tcPercentile
                ic
                icPercentile
                fnc
                fncPercentile
                fncV3
                fncV3Percentile
                mmc
                mmc20d
                mmc20dPercentile
                mmcMultiplier
                mmcPercentile
                payout
                roundNumber
                roundOpenTime
                roundPayoutFactor
                roundResolveTime
                roundResolved
                roundTarget
                selectedStakeValue
              }
            }
          }'
    )
    qry$query(
        'round_model_performances_signal',
        'query($username: String!) {
            v2SignalsProfile(modelName: $username) {
              roundModelPerformances {
                corr
                corr20d
                corr20dPercentile
                corrMultiplier
                corrPercentile
                corrWMetamodel
                tc
                tcPercentile
                ic
                icPercentile
                fnc
                fncPercentile
                fncV3
                fncV3Percentile
                mmc
                mmc20d
                mmc20dPercentile
                mmcMultiplier
                mmcPercentile
                payout
                roundNumber
                roundOpenTime
                roundPayoutFactor
                roundResolveTime
                roundResolved
                roundTarget
                selectedStakeValue
              }
            }
          }'
    )
    ##################################################################
    ##################################################################


    ##################################################################
    ## Change stake by `value` NMR.
    ##################################################################
    qry$query(
        'stake_change',
        'mutation($value: String!, $type: String!, $tournamentNumber: Int!, $modelId: String) {
              v2ChangeStake(value: $value, type: $type, modelId: $modelId, tournamentNumber: $tournamentNumber) {
                dueDate
                requestedAmount
                status
                type
              }
        }'
    )
    ##################################################################
    ##################################################################


    ##################################################################
    ## Change stake type by model.
    ##################################################################
    qry$query(
        'set_stake_type',
        'mutation ($corrMultiplier: Float!, $modelId: String!, $takeProfit: Boolean!, $tcMultiplier: Float!, $tournamentNumber: Int!) {
            v2ChangePayoutSelection(corrMultiplier: $corrMultiplier, modelId: $modelId, takeProfit: $takeProfit, tcMultiplier: $tcMultiplier, tournamentNumber: $tournamentNumber)
        }'
    )
    ##################################################################
    ##################################################################


    ##################################################################
    ## List of available data files
    ##################################################################
    qry$query(
        'list_datasets',
        'query ($round: Int) {
            listDatasets(round: $round)
        }'
    )
    ##################################################################
    ##################################################################


    ##################################################################
    ## Download specified file for the given round.
    ##################################################################
    qry$query(
        'download_dataset',
        'query ($filename: String!, $round: Int) {
            dataset(filename: $filename, round: $round)
        }'
    )
    ##################################################################
    ##################################################################


    ##################################################################
    ## Submission status of the last submission associated with the account
    ##################################################################
    qry$query(
        'submission_status_main',
        'query($modelId: String) {
                model(modelId: $modelId) {
                  latestSubmissionV2 {
                    filename
                    corrWithExamplePreds
                    validationCorrelation
                    validationSharpe
                    validationFeatureExposure
                    validationCorrelation
                    validationCorrelationRating
                    validationSharpe
                    validationSharpeRating
                    validationFeatureNeutralMean
                    validationFeatureNeutralMeanRating
                    validationStd
                    validationStdRating
                    validationMaxFeatureExposure
                    validationMaxFeatureExposureRating
                    validationMaxDrawdown
                    validationMaxDrawdownRating
                    validationCorrPlusMmcSharpe
                    validationCorrPlusMmcSharpeRating
                    validationMmcMean
                    validationMmcMeanRating
                    validationCorrPlusMmcSharpeDiff
                    validationCorrPlusMmcSharpeDiffRating
                  }
                }
              }'
    )
    qry$query(
        'submission_status_signal',
        'query($modelId: String) {
                model(modelId: $modelId) {
                  latestSignalsSubmission {
                    id
                    filename
                    firstEffectiveDate
                    userId
                    submissionIp
                    submittedCount
                    filteredCount
                    invalidTickers
                    hasHistoric
                    historicMean
                    historicStd
                    historicSharpe
                    historicMaxDrawdown
                  }
                }
              }'
    )
    ##################################################################
    ##################################################################


    ##################################################################
    ## Get the current leaderboard
    ##################################################################
    qry$query(
        'get_leaderboard_main',
        'query($limit: Int!, $offset: Int!) {
            v2Leaderboard(limit: $limit, offset: $offset) {
                bonusPerc
                nmrStaked
                oldStakeValue
                prevRank
                prevStakedRank
                rank
                stakedRank
                rolling_score_rep
                tier
                username
                leaderboardBonus
                averageCorrelationPayout
                payoutPending
                payoutSettled
                badges
                corrRep
                corrRank
                fncRep
                fncRank
                fncV3Rep
                fncV3Rank
                tcRep
                tcRank
              }
            }'
    )
    qry$query(
        'get_leaderboard_signal',
        'query($limit: Int!, $offset: Int!) {
              signalsLeaderboard(limit: $limit, offset: $offset) {
                prevRank
                rank
                sharpe
                today
                username
                mmc
                mmcRank
                nmrStaked
                icRank
                icRep
              }
            }'
    )
    ##################################################################
    ##################################################################
    return(list(con, qry))
}