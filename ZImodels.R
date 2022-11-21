options(warn=-1) # ignore warning messages.

source("ziSetup-1 copy.R")
library(pracma)
library(xts)

#The "logging" tag can be F or T 
#If it is T then eventLog keeps track of (event,price)
#colnames(eventLog)<<-c("Type","Price")
#eventType <<- c("LB","LS","CB","CS","MB","MS")

logging <- T

# Parameter settings
alpha <- 1   # rate of arrival limit orders
mu <- 10     # rate of arrival market orders
delta <- 2/5 # rate of cancellations

# Initialize book with asymptotic depth of 5 shares
numEvents <- 10
initializeBook5()

plot(-20:20, bookShape(20), main=NA, xlab="Relative price", ylab="# Shares", col="red", type="b")


# Add two orders to the (best) bid side
limitBuyOrder(bestBid())
limitBuyOrder(bestBid())
limitBuyOrder(-10)

# Plot the resulting book shape
plot(-20:20, bookShape(20), main=NA, xlab="Relative price", ylab="# Shares", col="red", type="b")

# Construct time series of the mid-price and bid-ask spread

# construct the time series of the spread/bid/ask

spdata = numeric(0)
middata = numeric(0)
bids <- NULL
offers <- NULL

# Burn in for 1000 events
for(count in 1:1000){
  generateEvent()
  midPrice <- mid()
  sp <- spread()
  spdata <- c(spdata,sp)
  middata <- c(middata, midPrice)
  bids <- c(bids, bestBid())
  offers <- c(offers, bestOffer())
}

# generate fake TAQ data:
# if eventType=MB/MS, Price=TradePrice, Ask=bestOffer, Bid=bestBid
# bidPosn, askPosn

tqdata.full <- data.frame(SYMBOL="YYY", EX="None",
                          BID = bids, BIDSIZ = 5,
                          OFR = offers, OFRSIZ=5,
                          MODE=20,PRICE=as.numeric(eventLog$Price),
                          DIR=eventLog$Type)



tqdata <- subset(tqdata.full, DIR=="MB"| DIR=="MS")
rownames(tqdata) <- 1:length(tqdata$BID)

tqdata_ms <- subset(tqdata.full, DIR=="MS")
tqdata_mb <- subset(tqdata.full, DIR=="MB")


#generate the timeline
first <- as.POSIXct("2022-04-01 10:20:20")
timeline <- seq(from=first, length.out=length(tqdata$BID),by="mins")

#Construct xts object
#generate the timeline
first <- as.POSIXct("2022-04-01 10:20:20")
timeline <- seq(from=first, length.out=length(tqdata$BID),by="mins")

#Construct xts object
tqdata <- xts(x=tqdata, order.by=timeline)

head(tqdata,75)
length(tqdata$BID)

# Plot the TAQ data

ylim100 = c(min(as.numeric(tqdata$BID[1:50]))-2,
            max(as.numeric(tqdata$OFR[1:50]))+2)


plot(as.numeric(tqdata$PRICE[1:50]), col="black",
     main="tqdata", xlab="trading time", ylab="",
     ylim=ylim100)
#points(as.numeric(tqdata$PRICE[tqdata$DIR=="MS"]), col="green")
lines(as.numeric(tqdata$BID[1:50]),col="blue",type="s")
lines(as.numeric(tqdata$OFR[1:50]),col="red",type="s")


# Part (ii) ACF of trade prices

price <- as.numeric(tqdata$PRICE)
price.diff <- diff(price)
price.diff.acf <- acf(price.diff, lag.max=20, type="covariance")
price.diff.acfc <- acf(price.diff, lag.max=20, type="correlation",
                       main="ACF(correl)")


observed_sp <- mean(spdata)
# observed_sp = 2.324
obs_sp <- mean(as.numeric(tqdata$OFR)-as.numeric(tqdata$BID))
# obs_sp = 3.54237

# record the trade direction (MB=+1, MS=-1)

trade.dir <- coredata(tqdata$DIR[,1])[,1]
trade.sign.recorded <- ifelse(trade.dir=="MB",1,-1)

plot(trade.sign.recorded, type="l")

acf(trade.sign.recorded, main="acf(trade signs)")

head(spdata)

gamma1 <- price.diff.acf$acf[2]
gamma1

gamma0 <- price.diff.acf$acf[1]
gamma0





roll_estimate <- sqrt(-gamma1)
roll_estimate

sigu2 <- gamma0 + 2 * gamma1
vol_roll <- sqrt(sigu2)
vol_roll


roll_spread <- 2 * roll_estimate
roll_spread

observed_sp <- mean(spdata)

obs_sp <- mean(as.numeric(tqdata$OFR)-as.numeric(tqdata$BID))


# record the trade direction (MB=+1, MS=-1)

trade.dir <- coredata(tqdata$DIR[,1])[,1]
trade.sign.recorded <- ifelse(trade.dir=="MB",1,-1)

plot(trade.sign.recorded, type="l")

acf(trade.sign.recorded, main="acf(trade signs)")

head(spdata)

# plot the mid-price
#    print(spdata)
#    class(spdata)
ymin <- min(middata)
ymax <- max(middata)
yrange <- c(ymin-0.1,ymax+0.1)

plot(middata, type="l", col="red",ylim =yrange,main="Mid-price")
#    lines(biddata,col="blue", type="l")
#    lines(biddata,col="blue", type="l")

avmid <- mean(middata)
sdmid <- sd(middata)
print(avmid)
print(sdmid)

summary(middata)  

#plot the bid-ask spread

yspmin <- min(spdata)
yspmax <- max(spdata)
ysprange <- c(yspmin-0.1,yspmax+0.1)

plot(spdata, type="l", col="red",ylim =ysprange,main="Spread")

avsp <- mean(spdata)
sdsp <- sd(spdata)
print(avsp)
print(sdsp)

summary(spdata)




# Function to plot average book shape for given parameters
plotAveBookShape <- function(a, m, d, n){

    # Logging data flag
    logging <- F

    # Initialize order book
    alpha <<- a
    mu <<- m
    delta <<- d 
    initializeBook5()

    # Burn in for 1000 events
    for(count in 1:1000){
        generateEvent()
    }
    
# Construct the stationary distribution of the book    
    
    # Set event times
    numEvents <- 100000

    # Calculate average book shape
    avgBookShape <- bookShape(20) / numEvents
    for(count in 2:numEvents){
        generateEvent()
        avgBookShape <- avgBookShape + bookShape(20) / numEvents
    }
    
    # Fit order slope and depth
    fit1 <- lm(avgBookShape[12:21] ~ c(-9:0))
    fit2 <- lm(avgBookShape[21:30] ~ c(0:9))
    slope1 <- fit1$coefficients[2]
    slope2 <- fit2$coefficients[2]
    slope <- (slope2-slope1) / 2
    depth <- (mean(avgBookShape[1:10]) + mean(avgBookShape[32:41])) / 2
    results[n,1] <<- slope
    results[n,2] <<- depth
    # Plot the result
    plot(-20:20, avgBookShape, main=n, xlab="Relative price", ylab="# Shares", col="red", type="b")
    return(results[n,])
}

# Setup book parameters
alphas <- c(1, 1, 5, 1)
mus <- c(10, 20, 10, 10)
deltas <- c(1/5, 1/5, 1/5, 2/5)

#old parameters 
#alphas <- c(1, 1, 1, 1)
#mus <- c(10, 8, 10, 10)
#deltas <- c(1/5, 1/5, 1/6, 1/8)

# Setup result matrix
results <- matrix(0,4,2)
rownames(results) <- c("I", "II", "III", "IV")
colnames(results) <- c("Slope", "Depth")

# Set plots position
#par(mfrow=c(2,2))

# Plot results

results[1,] <- t(as.matrix(plotAveBookShape(alphas[1], mus[1], deltas[1], rownames(results)[1])))
results[2,] <- t(as.matrix(plotAveBookShape(alphas[2], mus[2], deltas[2], rownames(results)[2])))
results[3,] <- t(as.matrix(plotAveBookShape(alphas[3], mus[3], deltas[3], rownames(results)[3])))
results[4,] <- t(as.matrix(plotAveBookShape(alphas[4], mus[4], deltas[4], rownames(results)[4])))

print(results)

