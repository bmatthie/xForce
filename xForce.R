# R script to read security intelligence data from IBM xForce Exchange

# Load my favorite libraries

library(RCurl)
library(jsonlite)
library(dplyr)

# Get the authorization token from xForce Exchange. Save the token in the curl
# options for use on all xForce API calls.

xfe_auth = getURL("https://xforce-api.mybluemix.net/auth/anonymousToken")
token = fromJSON(xfe_auth)
curl.opts <- list(httpheader = c("Accept-Language" = "en-US",
                                 "Accept-Encoding" = "gzip",
                                 "Authorization" = paste("Bearer", token),
                                 "Accept" = "application/json"))

# Call xForce to return the list of all known applications in JSON format.
# "Unlist" them to create a character vector of the application names.

appList <- unlist(fromJSON(getURL("https://xforce-api.mybluemix.net/app", 
                                  .opts = curl.opts)), use.names=FALSE)

# Create the final receiving data frame to be used in plotting and loop through
# the list of applications known to xForce, retrieving details for each
# application. Set application risk to "5" is it is missing from xForce.

appData <- data.frame()

# appList <- list("dropbox", "GOAL")
for (appName in appList) {
    
    # Replace spaces in URLs  
    appName <- gsub(" ", "%20", appName)        
    
    # Call xForce to pull the app details
    appDetail <- fromJSON(getURL(paste0("https://xforce-api.mybluemix.net/app/", 
                                        appName), .opts = curl.opts))
    
    # Replace missing risk scores with "5.0"
    if (is.null(appDetail$application$score)) {
        appDetail$application$score <- "5"
    }
    
    # Accumulate the data table for plotting
    appData <- rbind(appData, matrix(list(
                                as.character(appDetail$application$name), 
                                as.numeric(appDetail$application$score)),
                                nrow = 1, ncol = 2))
}

# Add column names to make it easier to work with.
colnames(appData) <- list("Name", "Score")

# Plot the histogram of xForce risk scores. 
x <- as.vector(appData$Score, mode="numeric")
h <- hist(x, breaks=10, col="red", xlab="xForce Risk Scores", 
          main="Distribution of Risk Scores for xForce Exchange Applications")

# Add a distribution curve.
xfit<-seq(min(x),max(x),length=40) 
yfit<-dnorm(xfit,mean=mean(x),sd=sd(x)) 
yfit <- yfit*diff(h$mids[1:2])*length(x) 
lines(xfit, yfit, col="blue", lwd=2)

