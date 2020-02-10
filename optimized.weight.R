source("configuration.R")

# Initialization ------------------------------------------------------------------
inputData = fromJSON(input)

id = as.integer(inputData$id)
try(if(is.na(id)) stop("No id!"))

riskTolerance = as.character(inputData$riskLevel)
investmentHorizon = as.character(inputData$investmentHorizon)

# should be today's date, but for now just use "2016-12-30"
DATE = as.character(inputData$date)
SR = as.character(inputData$SR)  # scaling ratio 
# !!! remember to add
try(if(is.na(SR)) stop("No scaling ratio!"))

# Selection ----------------------------------------------------------
asset_pool = as.data.frame(connect.and.query("SELECT asset,category,credit,maturity FROM asset_pool"))
slec.tickers = apply.Preference(asset_pool, excludeSheet, riskTolerance, investmentHorizon)

proj_return = as.data.frame(connect.and.query(paste0("SELECT ",slec.tickers," FROM proj_return WHERE Date < ",DATE)))
daily_return = as.data.frame(connect.and.query(paste0("SELECT ",slec.tickers," FROM daily_return WHERE Date < ",DATE)))

# clean.charInNum
# clean.spike


# Get optimized weights ------------------------------------------------------
weights = optimized.weights(daily_return,proj_return,SD)

weights
