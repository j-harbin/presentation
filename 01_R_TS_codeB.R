# Report on bad Pacific salinities (non-interactive)

library(argoFloats)
# Step one: getIndex()
ai <- getIndex()

# Step two: subset() - circle and polygon
index1 <- subset(ai, circle=list(longitude=-83, latitude=9, radius=150))
par(mfrow=c(1,2))
plot(index1)
atl <- locator(type="l")
# Interactively click around Atlantic points
pac <- locator(type="l")
# Interactively click around Pacific points
atlantic <- subset(index1, polygon=list(longitude=atl$x, latitude=atl$y))
pacific <- subset(index1, polygon=list(longitude=pac$x, latitude=pac$y))
points(atlantic[["longitude"]], atlantic[["latitude"]], pch=20, col="blue")
points(pacific[["longitude"]], pacific[["latitude"]], pch=20, col="green")
legend("topleft", c("Atlantic", "Pacific"), col=c("blue", "green"), pch=c(20,20), cex=0.7)

# Step three: getProfiles()
profilesA <- getProfiles(atlantic)
profilesP <- getProfiles(pacific)

# Step four: readProfiles()
argosA <- readProfiles(profilesA)
argosP <- readProfiles(profilesP)

# Step five: Process the data
SA <- unlist(argosA[["SA"]])
SP <- unlist(argosP[["SA"]])
TA <- unlist(argosA[["CT"]])
TP <- unlist(argosP[["CT"]])

plot(argosA, which="TS", col="blue", xlim=range(SA,SP), ylim=range(TA,TP))
points(SP,TP, pch=20, col="green")

# Analyzing "bad" data

badFlag <- c(0,3,4,9)

cycles <- argosP[["argos"]]


for (icycle in seq_along(cycles)) {
    thisCycle <- cycles[[icycle]]
    badLevel <- as.vector(thisCycle[['salinityFlag']]) %in% badFlag
    badCycle <- any(badLevel)
    if (badCycle) {
        message("Float ID ", thisCycle[["ID"]], " at cycle position ", icycle," ie. cycle number ", thisCycle[["cycleNumber"]]," is flagged bad at levels: ", paste(which(badLevel), collapse=" "))
        showQCTests(thisCycle)
    }
}
