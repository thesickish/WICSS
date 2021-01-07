#############################################################
#
#				WICSS-TUCSON
#	Winter Institute in Computational Social Science
#
#				Social Networks
#			Thursday January 7th, 2020
#
#############################################################

# This script goes through using the R package twitteR to construct a social
# network based on retweets, starting with a user's Twitter handle.

# We will be using Twitter's API, which requires API keys. If you already
# have these, feel free to skip this step. Obtaining API keys typically 
# requires going through an application process, and it can take several days
# to get approval. Another route towards getting API keys, and the one we will 
# be taking here, is to have them distributed to you by what Twitter calls an
# organizational account. In order for me to provide you with keys through my
# organizational account, you must:
# 1) Create a Twitter account, if you do not have one already (or would prefer
# not to share your existing Twitter handle with the WICSS organizers)
# 2) Make sure to verify your email address on your Twitter account 
# 3) Make sure there is a phone number associated with the account
# 4) Email me your Twitter handle at wicss.tucson@gmail.com

# Note: if I am able to add you to the organizational account, you should receive
# an email from Twitter. Make sure to click the link in that email. If I cannot 
# add your Twitter handle, that means either your email was not verified or there
# was no phone number associated with the account. I will make sure to email you 
# back if I was unable to add you to the organizational account.

# The final step, after you've clicked the link in the email that Twitter sent,
# is to create an app through Twitter's Developer Portal
# 1) Go to https://developer.twitter.com/en/portal/dashboard
# 2) Click 'Create Project'
# 3) Fill in the necessary information when prompted. For the project description, 
# feel free to use "As part of a workshop at the Winter Institute in Computational
# Social Science, we will be constructing a retweet network from a user's handle"
# 4) Create a new app. It must have a unique name (e.g. WICSS-TucsonYOUR_NAME).
# 5) Once your app is created, you should be able to locate it on the left
# 6) Click on the app, then select 'Keys and tokens' near the top
# 7) From here, you should be able to view your API key & secret
# 8) Scroll down to generate your Access token & secret

# Install and load the package twitteR
install.packages("twitteR") # skip if already installed
library(twitteR)

# In the quotes below, enter your authentication codes in the following order: 
# API key (AK), API key secret (AKS), Access token (AT), Access token secret (ATS)
setup_twitter_oauth("AK","AKS","AT","ATS")


# The command below may open a browser tab where you will need to authorize the app
pol_raw = searchTwitter("politics")
pol_raw

# Convert list of tweets to dataframe
pol <- twListToDF(pol_raw)
names(pol)


#############################################################
#
# SECTION 1: Collecting user timeline tweets
#
#############################################################


# Define objects for storing the retweeter network data
posts <- NULL
text <- NULL
people <- NULL
links <- NULL

# Define object for comparing retweets to retweeters obtained
check <- NULL

# Set number of user and retweeter tweets to be pulled
U = 10
R = 10

# Obtain updated rate limit information
# Find the rate limits here: https://developer.twitter.com/en/docs/twitter-api/v1/rate-limits
getCurRateLimitInfo()

# Obtain updated rate limit information for Timelines and Retweets
getCurRateLimitInfo(resources='statuses')

# Counter to keep track of User Timeline and Retweeter calls of Twitter's API
callsUT = 0 # Rate limit = 900 / 15 min
callsRT = 0 # Rate limit = 75 / 15 min

# Set minimum and maximum Tweet IDs
SID <- 760836150760050689 # https://twitter.com/HillaryClinton/status/760836150760050689
MID <- 795954831718498305 # https://twitter.com/realDonaldTrump/status/795954831718498305

# Test non-existent account
ego_raw <- userTimeline("fakeaccount1998448", n=U, includeRts=FALSE, excludeReplies=TRUE)
ego_raw

# Obtain updated rate limit information for Timelines and Retweets
getCurRateLimitInfo(resources='statuses')

# Update number of Timeline calls
callsUT = callsUT + 1

# Use the 'try' function to the prevent code from breaking when encountering a non-existent account
ego_raw <- try(userTimeline("fakeaccount1998448", n=U, includeRts=FALSE, excludeReplies=TRUE))
ego_raw

# Update number of Timeline calls
callsUT = callsUT + 1

# Set variable for error message
error <- "Error in twInterface"

# Pull U pre-election tweets from Senator Shelli Yoder
ego_raw <- try(userTimeline("Shelli4Indiana", n=U, maxID = MID, sinceID = SID, includeRts=FALSE, excludeReplies=TRUE))
ego_raw

# Update number of Timeline calls
callsUT = callsUT + 1

# Convert list of tweets to dataframe
ego <- twListToDF(ego_raw)
ego

# Store number of tweets obtained
egoD <- dim(ego)[1]
egoD


#############################################################
#
# SECTION 2: Collecting retweeter information
#
#############################################################


# Check that tweets were pulled
if(egoD !=0 & substr(ego_raw[1],1,20) != error)
{

	# Collect retweeters for all ego tweets
	for(i in 1:egoD)
	{
		# Store tweet info
		egoID <- ego[i,"id"]
		egoSN <- ego[i,"screenName"]
		egoTX <- ego[i,"text"]
		egoRT <- ego[i,"retweetCount"]
		egoFC <- ego[i,"favoriteCount"]
		egoCR <- ego[i,"created"]

		newpost <- cbind(0,egoID,egoSN,egoRT,egoFC,egoCR,"egoPost")
		posts <- rbind(posts,newpost)

		newtext <- cbind(0,egoID,egoSN,egoTX)
		text <- rbind(text,newtext)
		
		# Pull retweeters of ego tweets
		if(egoRT > 0)
		{
			
			# Check if Retweeter rate limit is reached
			if(callsRT == 75)
			{
				Sys.sleep(900)
				callsRT = 0
			}
			
			egoRTers_raw <- retweets(egoID,n=100)
			callsRT = callsRT + 1

			if(length(egoRTers_raw) > 0)
			{
				# Store ego retweeter screen names
				egoRTers <- twListToDF(egoRTers_raw)
				egoRTersSN <- t(t(egoRTers$screenName))
			
				if(length(egoRTers) > 0)
				{
				
					# Store other ego retweeter info
					newcheck <- cbind(0,i,egoRT,length(egoRTersSN))
					check <- rbind(check,newcheck)
					
					newpeople <- cbind(egoRTersSN)
					people <- rbind(people,newpeople)
					
					newlinks <- cbind(egoRTersSN,egoSN,"egoRetweeter/egoPost")
					links <- rbind(links,newlinks)
				}
			}
		}
	}
}

# Print dimensions of retweeter object
dim(people)

# Remove duplicates and store number of unique retweeters
people <- unique(people)
peopleD <- dim(people)[1]
peopleD


#############################################################
#
# SECTION 3: Collecting retweeter's retweeter information
#
#############################################################


# Collect tweets from all retweeters
for(j in 1:peopleD)
{
	# Check if User Timeline rate limit is reached
	if(callsUT == 900)
	{
		Sys.sleep(900)
		callsUT = 0
	}

	# Pull retweeter tweets
	alter_raw <- try(userTimeline(people[j,1], n=R, maxID = MID, sinceID = SID, includeRts=FALSE, excludeReplies=TRUE))
	callsUT = callsUT + 1

	# Check that tweets were pulled
	if(length(alter_raw)!=0 & substr(alter_raw[1],1,20) != error)
	{
		# Store number of tweets obtained
		alter <- twListToDF(alter_raw)
		alterD <- dim(alter)[1]

		# Collect retweeters for all alter tweets
		for(k in 1:alterD)
		{
			# Store tweet info
			alterID <- alter[k,"id"]
			alterSN <- alter[k,"screenName"]
			alterTX <- alter[k,"text"]
			alterRT <- alter[k,"retweetCount"]
			alterFC <- alter[k,"favoriteCount"]
			alterCR <- alter[k,"created"]
			
			newpost <- cbind(people[j,1],alterID,alterSN,alterRT,alterFC,alterCR,"alterPost")
			posts <- rbind(posts,newpost)

			newtext <- cbind(people[j,1],alterID,alterSN,alterTX)
			text <- rbind(text,newtext)
			
			# Pull retweeters of alter tweets
			if(alterRT > 0)
			{
				# Check if Retweeter rate limit is reached
				if(callsRT == 75)
				{
					Sys.sleep(900)
					callsRT = 0
				}
	
				altRTers_raw <- retweets(alterID,n=100)
				callsRT = callsRT + 1
				
				if(length(altRTers_raw) > 0)
				{
					# Store alter retweeter screen names
					altRTers <- twListToDF(altRTers_raw)
					altRTersSN <- t(t(altRTers$screenName))

					if(length(altRTers) > 0)
					{
						# Store other alter retweeter info
						newcheck <- cbind(j,k,alterRT,length(altRTersSN))
						check <- rbind(check,newcheck)
				
						newlinks <- cbind(altRTersSN,alterSN,"alterRetweeter/alterPost")
						links <- rbind(links,newlinks)
					}
				}
			}
		}
	}
}


#############################################################
#
# SECTION 4: Visualizing the retweet network
#
#############################################################


# Install and load the packages plyr and igraph
install.packages("plyr") # skip if already installed
install.packages("igraph")  # skip if already installed
library(plyr)
library(igraph)

# Print total number of network edges (including duplicates)
dim(links)

# Print edge information
links

# Collapse across duplicate edges
network <- count(links, c(1,2))

# Print number of network edges (with duplicates collapsed into edge weight)
dim(network)

# Print weighted edge information
network

# Convert data to matrix (for input into igraph)
network=as.matrix(network)

# Define full graph
g=graph.edgelist(network[,1:2])
E(g)$weight=as.numeric(network[,3])

# Plot full graph
plot(g,vertex.size=3,vertex.label=NA,edge.width=E(g)$weight,edge.arrow.size=.25)

# Define ego graph
g2 <- make_ego_graph(g, 1, nodes = "Shelli4Indiana", mode = c("all", "out", "in"), mindist = 0)

# Plot ego graph
plot(g2[[1]],vertex.size=3,edge.width=E(g2[[1]])$weight,edge.arrow.size=.1)


