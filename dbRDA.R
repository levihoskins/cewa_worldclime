

####### db-RDA pages(188-190 in Brocard et al.)
library(vegan)
cewa_bio <- readRDS("Cerulean_Warbler/cewa_bio.RDS")



#########Modify Data
#remove site 8
spe <- spe[-8, ]
env <- env[-8, ]

env$pen

# Remove the 'das' variable from the env dataset
env <- env[, -1]

# Recode the slope variable (pen) into a factor (qualitative) 
# variable (to show how these are handled in the ordinations)
pen2 <- rep("very_steep", nrow(env))
pen2[env$pen <= quantile(env$pen)[4]] <- "steep"
pen2[env$pen <= quantile(env$pen)[3]] <- "moderate"
pen2[env$pen <= quantile(env$pen)[2]] <- "low"
pen2 <- factor(pen2, levels=c("low", "moderate", "steep", "very_steep"))
table(pen2)


# Create an env2 data frame with slope as a qualitative variable
env2 <- env
env2$pen <- pen2


#name each environmental varaible

alt<-env2$alt
oxy<-env2$oxy
dbo<-env2$dbo
dur<-env2$dur
pen<-env2$pen


#Use the "capscale" function in Vegan to run db-rda.Note that the "distance" 
#argument turns the site by species matrix into a distance matrix. You can use any distance measure in vegan (i.e., vegdist function)

db.rda <- capscale(spe ~ alt + oxy + dbo + dur, distance = "bray", add=TRUE)
summary(db.rda)


#R2 and adjusted R2
R2 <- RsquareAdj(db.rda)$r.squared
R2adj <- RsquareAdj(db.rda)$adj.r.squared


#Plot using the F-scores:
par(mfrow=c(1,2))
plot(db.rda, scaling=1, 
     main="Triplot db-rda F scores")
spe.sc <- scores(db.rda, choices=1:2, scaling=2, display="sp")
arrows(0, 0, spe.sc[, 1], spe.sc[, 2], length=0, lty=1, col="red")

#Plot using the Z-scores:
plot(db.rda, scaling=1, display=c("sp", "lc", "cn"), 
     main="Triplot db-rda  Z scores")
arrows(0, 0, spe.sc[, 1], spe.sc[, 2], length=0, lty=1, col="red")


#Conduct a permutation test using anova function in vegan to test the significance of the model, individual axes, and varaibles:

#Global test of the RDA result
anova(db.rda, step=1000)

#Tests of all canonical axes:
anova(db.rda, by="axis", step=1000)

#Tests of all variables:
anova(db.rda, by="margin", step=1000)

#partial RDA
#Use the "capscale" function in Vegan to run partial db-rda

db.rda <- capscale(spe ~ alt + Condition(oxy + dbo + dur), distance = "bray", add=TRUE)
summary(db.rda)

R2 <- RsquareAdj(db.rda)$r.squared
R2adj <- RsquareAdj(db.rda)$adj.r.squared


#Here we partition the variance for the model we constructed trough forward selection above: 
#first we have to create our distance matrix as the response matrix
resp<-vegdist(spe, method="bray")

#then we run the varpart function from vegan
spe.part <- varpart(resp,~ alt,~ oxy,~ dur ,~dbo ,data=env2)
plot(spe.part, digits=2)