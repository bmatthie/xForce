# R script to read security intelligence data from IBM xForce Exchange

# Load my favorite libraries

library(RCurl)
library(jsonlite)
library(dplyr)

# Get the authorization token from xForce Exchange. Save the token in the curl
# options for use on all xForce API calls.

xfe_auth = fromJSON(getURL("https://xforce-api.mybluemix.net/auth/anonymousToken"))
curl.opts <- list(httpheader = c("Accept-Language" = "en-US",
                                 "Accept-Encoding" = "gzip",
                                 "Authorization" = paste("Bearer", xfe_auth$token),
                                 "Accept" = "application/json"))

# Call xForce to return the list of all known applications in JSON format.
# JASON key "canonicalNames" holds the list of application names.

appList <- fromJSON(getURL("https://xforce-api.mybluemix.net/app", 
                                  .opts = curl.opts))

# Create the final receiving data frame to be used in plotting and loop through
# the list of applications known to xForce, retrieving details for each
# application. Set application risk to "5" if it is missing from xForce.

appData <- data.frame()

# appList <- list("dropbox", "GOAL")
# for (appName in appList) { 
#     print(appName)

for (appName in appList$canonicalNames) {
   
    # Replace spaces in URLs  
    appName <- gsub(" ", "%20", appName)        
    
    # Call xForce to pull the app details
    appDetail <- fromJSON(getURL(paste0("https://xforce-api.mybluemix.net/app/", 
                                        appName), .opts = curl.opts))
    
    # Replace missing risk scores with "5.0"
    if (is.null(appDetail$application$score)) {
        appDetail$application$score <- "5"
    }
    
    # Accumulate the data table for plotting.
    # Note that JSON keys map to R "name" when using fromJSON function.
    appData <- rbind(appData, matrix(list(
                                "Application",
                                appDetail$application$name,
                                appDetail$application$description,
                                names(appDetail$application$categories),
                                names(appDetail$application$actions),
                                names(appDetail$application$rlfs),
                                as.numeric(appDetail$application$score)),
                                nrow = 1, ncol = 7))
}

# Add column names to make it easier to work with.
colnames(appData) <- list("Type", "Name", "Description", "Categories", "Actions", "Rilfs", "Score")

# Plot the histogram of xForce risk scores. 
x <- as.vector(appData$Score, mode="numeric")
h <- hist(x, breaks=10, col="red", xlab="xForce Risk Scores", 
          main="Distribution of Risk Scores for xForce Exchange Applications")

# Add a distribution curve.
xfit <-seq(min(x),max(x),length=40) 
yfit <-dnorm(xfit,mean=mean(x),sd=sd(x)) 
yfit <- yfit*diff(h$mids[1:2])*length(x) 
lines(xfit, yfit, col="blue", lwd=2)

