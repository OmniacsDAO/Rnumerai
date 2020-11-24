[![Travis-CI Build Status](https://travis-ci.org/Omni-Analytics-Group/Rnumerai.svg?branch=master)](https://travis-ci.org/Omni-Analytics-Group/Rnumerai)

<a href="https://omnianalytics.io" target="_blank"><img src="man/figures/OAG_CLR_web_small.png" align="right"/></a>

# Rnumerai 
> R Interface to the Numerai Machine Learning Tournament API

This interface allows download of tournament data, submit predictions, get user information, stake NMR's and much more.
Using the functions from this package end user can write R code to automate the whole procedure related to numerai tournament.

If you encounter a problem or have suggestions, feel free to open an issue.

# Installation

- For the latest stable release: `install.packages("Rnumerai")`
- For the latest development release: `devtools::install_github("Omni-Analytics-Group/Rnumerai")`

# Automatic submission using this package

### 1. Load the package.

-   `library(Rnumerai)`

### 2. Set working directory where data will be downloaded and submission files would be kept.

- Use current working directory

    `data_dir <- getwd()`

- Or use temporary directory

    `data_dir <- tempdir()`

- Or use a user specific directory

    `data_dir <- "~/OAG/numerai"`

### 3. Set Public Key and Secret API key variables.

Get your public key and api key by going to numer.ai and then going to `Custom API Keys` section under your `Account` Tab. Select appropriate scopes to generate the key or select all scopes to use the full functionality of this package.

-   `set_public_id("public_id_here")`
-   `set_api_key("api_key_here")`

Optional: If we choose not to setup the credentials here the terminal will interactively prompt us to type the values when we make an API call.

### 4. Download data set for the current round and split it into training data and tournament data 

-    `data <- download_data(data_dir)`
-    `data_train <- data$data_train`
-    `data_tournament <- data$data_tournament`

### 5. Generate predictions

A user can put his/her own custom model code to generate the predictions here. For demonstration purposes, we will generate random predictions.

-   `submission <- data.frame(id=data_tournament$id,prediction = sample(seq(.35,.65,by=.1),nrow(data_tournament),replace=TRUE))`

### 6. Submit predictions for tournament and get submission id

The submission object should have two columns (id & prediction) only.

-    `submission_id <- submit_predictions(submission,data_dir,tournament="Nomi")`

### 7. Check the status of the submission (Wait for a few seconds to get the submission evaluated)

-   `Sys.sleep(10)      ## 10 Seconds wait period`
-   `status_submission_by_id(submission_id)`
    
### 8. Stake submission on submission and get transaction hash for it.

-   `stake_tx_hash <- stake_nmr(value = 1)`
-   `stake_tx_hash`

### 9. Release Stake and get transaction hash for it.

-   `release_tx_hash <- release_nmr(value = 1)`
-   `release_tx_hash`

# Performance functions

Users can now check performance across the following metrics: `Reputation`, `Rank`, `NMR_Staked`, `Leaderboard_Bonus`, `Payout_NMR`, `Average_Daily_Correlation`, `Round_Correlation`, `MMC`, `Correlation_With_MM`.

### 1. Display performance distributions

Create histograms of metric performance

- `performance_distribution(c("objectscience"), "Average_Daily_Correlation")`
- `performance_distribution(c("objectscience"), "MMC")`

### 2. Display performance over time

Create time series plots of metric performance

- `performance_over_time(c("objectscience"), "MMC")`
- `performance_over_time(c("objectscience", "uuazed", "arbitrage"), "Average_Daily_Correlation", outlier_cutoff = .01, merge = TRUE)`

### 3. Display performance summary statistics

Create a table of summary statistics

- `summary_statistics(c("objectscience", "uuazed", "arbitrage"))`

# Additional functions

### 1. Get account information

Get account information for the account whose API key and ID are entered, Check out the name of the return object to see what informations are included in the return and then subset the required information

-   `ainfo <- account_info()`
-   `ainfo`
-   `names(ainfo)`

### 2. Get Information for a round
Get information for a given round number.

-   `round_stats(tournament="Nomi",round_number=238)`

### 3. Get current open round
Get closing time and round number for current open round

-   `current_round()`

### 4. Get current leaderboard
Get V2 Leaderboard

-   `leaderboard()`

### 5. Get User Performance
Get V2 User Profile 

-   `user_performance(user_name="theomniacs")`

### 6. Run Custom GraphQL code from R:

-   `custom_query <- 'query queryname {
    					rounds (number:177) {
    						closeTime
    					}
    				}'`
-   `run_query(query=custom_query)$data`
