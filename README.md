[![Travis-CI Build Status](https://travis-ci.org/Omni-Analytics-Group/Rnumerai.svg?branch=master)](https://travis-ci.org/Omni-Analytics-Group/Rnumerai)

<a href="https://omnianalytics.org" target="_blank"><img src="man/figures/OAG_CLR_web_small.png" align="right"/></a>

# Rnumerai 
> R Interface to the Numerai Machine Learning Tournament API

This interface allows download of tournament data, submit predictions, get user information, stake NMR's and much more.
Using the functions from this package end user can write R code to automate the whole procedure related to numerai tournament.

If you encounter a problem or have suggestions, feel free to open an issue.

<hr>

## Installation
- For the latest development release: 
```
devtools::install_github("Omni-Analytics-Group/Rnumerai")
```
- For the latest stable release: 
```
install.packages("Rnumerai")
```


## Documentation Jump
- [Basic functions to perform tasks other than automating submissions](https://github.com/Omni-Analytics-Group/Rnumerai#basic-functions-to-perform-tasks-other-than-automating-submissions)
- [Automatic submission using this package (Main Competition)](https://github.com/Omni-Analytics-Group/Rnumerai#automatic-submission-using-this-package-main-competition)
- [Automatic submission using this package (Signals Competition)](https://github.com/Omni-Analytics-Group/Rnumerai#automatic-submission-using-this-package-signals-competition)

<hr>

## Basic functions to perform tasks other than automating submissions

#### 1. Load the package.
```
library(Rnumerai)
```

#### 2. Set Public Key and Secret API key variables.

Get your public key and api key by going to numer.ai and then going to `Custom API Keys` section under your `Account` Tab. Select appropriate scopes to generate the key or select all scopes to use the full functionality of this package.
```
set_public_id("public_id_here")
set_api_key("api_key_here")
```

#### 3. Get all information about your account
```
get_account()
```

#### 3. Get all of your models in Main and Signal Tournament
-   For Main Tournament
```
get_models(tournament=8)
```

-   For Signals Tournament
```
get_models(tournament=11)
```

#### 4. Get number of the current active round.
```
get_current_round()
```

#### 5. Get info on rounds in Main and Signal Tournament
-   For Main Tournament
```
get_competitions(tournament=8)
```
-   For Signals Tournament
```
get_competitions(tournament=11)
```

#### 6. Set Bio and Link Field for a Model Id
```
set_bio(model_id = get_models()[["bayo"]], bio = "This Model Rocks")
set_link(model_id = get_models()[["bayo"]], link = "https://www.google.com",link_text = "Google")
```

#### 7. Get all transactions in your wallet.
```
wallet_transactions()
```

#### 8. Set a model's submission webhook used in Numerai Compute.
```
set_submission_webhook(model_id = get_models()[["bayo"]], webhook = "..")
```

#### 9. Fetch round's model performance of any user
-   For Main Tournament
```
round_model_performances(username = "bayo",tournament=8)
```
-   For Signals Tournament
```
round_model_performances(username = "bayo",tournament=11)
```

#### 10. Fetch Daily model performance of any user
-   For Main Tournament
```
daily_model_performances(username = "bayo",tournament=8)
```
-   For Signals Tournament
```
daily_model_performances(username = "bayo",tournament=11)
```

#### 11. Fetch Daily submission performance of any user
-   For Main Tournament
```
daily_submission_performances(username = "bayo",tournament=8)
```
-   For Signals Tournament
```
daily_submission_performances(username = "bayo",tournament=11)
```

#### 12. Get Leaderboard
-   For Main Tournament
```
get_leaderboard(tournament=8)
```
-   For Signals Tournament
```
get_leaderboard(tournament=11)
```

#### 13. Submission status of the last submission associated with the account
-   For Main Tournament
```
model_id = get_models(tournament=8)[["bayo"]]
submission_status(model_id = model_id, tournament=8)
```

-   For Signals Tournament
```
model_id = get_models(tournament=11)[["test5678"]]
submission_status(model_id = model_id, tournament=11)
```

#### 14. Run a custom query
```
run_query(query = 'query{account{username}}', auth=TRUE)
run_query(query = 'query{rounds{number,closeTime}}', auth=FALSE)
```


<hr>


## Automatic submission using this package (Main Competition)

#### 1. Load the package.
```
library(Rnumerai)
```

#### 2. Set Public Key and Secret API key variables.

Get your public key and api key by going to numer.ai and then going to `Custom API Keys` section under your `Account` Tab. Select appropriate scopes to generate the key or select all scopes to use the full functionality of this package.
```
set_public_id("public_id_here")
set_api_key("api_key_here")
```

#### 3. List the datasets for current round
```
list_datasets()
```


#### 4A. For V2 Data (Released in late 2019), Download Train, Validation and Live data and submit predictions
-   Download
```
download_dataset("v2/numerai_datasets.zip", "numerai_datasets.zip")
download_dataset("v2/numerai_live_data.parquet", "numerai_live_data.parquet")
```
-   Load in R
```
unzip("numerai_datasets.zip",overwrite = TRUE, list = FALSE)
data_train <- read.csv("numerai_training_data.csv")
data_tournament <- read.csv("numerai_tournament_data.csv")
data_live <- data.table::setDT(arrow::read_parquet("numerai_live_data.parquet"))
```
-   Make Dummy Prediction and submit
```
predictions <- data.frame(id=data_live$id,prediction=sample(400:600,nrow(data_live),replace=TRUE)/1000)
upload_predictions(model_id = get_models()[["bayo"]],df=predictions)
```

#### 4B. For V3 Data (Released in September of 2021), Download Train, Validation and Live data and submit predictions
-   Download
```
download_dataset("v3/numerai_training_data.parquet", "numerai_training_data.parquet")
download_dataset("v3/numerai_validation_data.parquet", "numerai_validation_data.parquet")
download_dataset("v3/numerai_live_data.parquet", "numerai_live_data.parquet")
download_dataset("v3/numerai_datasets.zip", "numerai_datasets.zip")
```
-   Load in R
```
data_train <- data.table::setDT(arrow::read_parquet("numerai_training_data.parquet"))
data_validation <- data.table::setDT(arrow::read_parquet("numerai_validation_data.parquet"))
data_live <- data.table::setDT(arrow::read_parquet("numerai_live_data.parquet"))
```
-   Make Dummy Prediction and submit
```
predictions <- data.frame(id=data_live$id,prediction=sample(400:600,nrow(data_live),replace=TRUE)/1000)
upload_predictions(model_id = get_models()[["bayo"]],df=predictions)
```
-   Make Dummy Diagnostics and submit
```
diagnostics <- data.frame(id=data_validation$id,prediction=sample(400:600,nrow(data_validation),replace=TRUE)/1000)
diagnostics_id <- upload_diagnostics(model_id = get_models()[["bayo"]],df=diagnostics)
diagnostics(model_id = get_models()[["bayo"]],diagnostics_id=diagnostics_id)
```

#### 4C. For V4 Data (Released in April of 2022), Download Train, Validation and Live data and submit predictions
-   Download
```
download_dataset("v4/train.parquet", "train.parquet")
download_dataset("v4/validation.parquet", "validation.parquet")
download_dataset("v4/live.parquet", "live.parquet")
download_dataset("v4/live_example_preds.parquet", "live_example_preds.parquet")
download_dataset("v4/validation_example_preds.parquet", "validation_example_preds.parquet")
download_dataset("v4/features.json", "features.json")
```
-   Load in R
```
data_train <- data.table::setDT(arrow::read_parquet("train.parquet"))
data_validation <- data.table::setDT(arrow::read_parquet("validation.parquet"))
data_live <- data.table::setDT(arrow::read_parquet("live.parquet"))
```
-   Make Dummy Prediction and submit
```
predictions <- data.frame(id=data_live$id,prediction=sample(400:600,nrow(data_live),replace=TRUE)/1000)
upload_predictions(model_id = get_models()[["bayo"]],df=predictions)
```
-   Make Dummy Diagnostics and submit
```
diagnostic_preds <- data.frame(id=data_validation$id,prediction=sample(400:600,nrow(data_validation),replace=TRUE)/1000)
diagnostics_id <- upload_diagnostics(model_id = get_models()[["bayo"]],df=diagnostic_preds)
diagnostics(model_id = get_models()[["bayo"]],diagnostics_id=diagnostics_id)
```

#### 5. Change your stake
-   Increase
```
stake_change(nmr=.01,action="increase",model_id = get_models()[["bayo"]])
```
-   Decrease
```
stake_change(nmr=.01,action="decrease",model_id = get_models()[["bayo"]])
```
-   Change Stake Type
```
set_stake_type(model_id = get_models()[["bayo"]],corr_multiplier=1,tc_multiplier=2,tournament=8)
```

<hr>


## Automatic submission using this package (Signals Competition)

#### 1. Load the package.
```
library(Rnumerai)
```

#### 2. Set Public Key and Secret API key variables.
Get your public key and api key by going to numer.ai and then going to `Custom API Keys` section under your `Account` Tab. Select appropriate scopes to generate the key or select all scopes to use the full functionality of this package.
```
set_public_id("public_id_here")
set_api_key("api_key_here")
```

#### 3. Download the ticker universe.
```
tickers <- ticker_universe()
```

#### 4. Make Dummy Predictions and submit
```
predictions <- cbind(tickers,signal = sample(400:600,nrow(tickers),replace=TRUE)/1000)
upload_predictions(model_id = get_models(tournament=11)[["test5678"]],df=predictions,tournament=11)
```

#### 5. Make Dummy Diagnostics and submit
```
download_validation_data(file_path = "signals_historical_targets.csv")
data_validation <- read.csv("signals_historical_targets.csv")
data_validation <- data_validation[sample(1:nrow(data_validation),1000),1:3]
data_validation$data_type <- "validation"
diagnostic_preds <- cbind(data_validation,signal = sample(400:600,nrow(data_validation),replace=TRUE)/1000)
diagnostics_id <- upload_diagnostics(model_id = get_models(tournament=11)[["test5678"]],df=diagnostic_preds,tournament=11)
diagnostics(model_id = get_models(tournament=11)[["test5678"]],tournament=11,diagnostics_id=diagnostics_id)
```
#### 6. Change your stake
-   Increase
```
stake_change(nmr=.01,action="increase",tournament=11,model_id = get_models(tournament=11)[["test5678"]])
```
-   Decrease
```
stake_change(nmr=.01,action="decrease",tournament=11,model_id = get_models(tournament=11)[["test5678"]])
```
-   Change Stake Type
```
set_stake_type(model_id = get_models(tournament=11)[["test5678"]],corr_multiplier=1,tc_multiplier=2,tournament=11)
```