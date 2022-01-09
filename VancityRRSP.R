## Vancity RRSP Campaign 
# Mikka Marin
# June 27, 2021

## Load required R packages
library("dplyr")
library("car")
library("forcats")
library("gplots")
library("effects")
library("corrplot")

source("BCA_functions_source_file.R")

# Read file to R
VC<-read.csv(file="VC_data.csv", stringsAsFactors = TRUE)

View(VC)

## -------- DATA CLEANING ---------------------------

# VARIABLES TO EXCLUDE
# "unique" --> identifier variable
# "pcode" --> postal codes are regional identifiers
# "gendf"

# CORRELATIONS 
# Examine correlations among predictor variables 
corVC<-cor(select_if(VC, is.numeric))
View(corVC)
corrplot(corVC, method="number", type="lower",diag=FALSE,number.cex=0.7)

# Exclude DUMNOLOAN, DUMNOMRGG, and NINDINC1

# --------- EST/VAL SETS -----------------------------
# Split data into "estimation" and "validation" samples
# Create Estimation (Training) and Validation (Test) sample
VC$Sample<-create.samples(VC,
                            est = 0.60, # 60% estimation sample
                            val = 0.40, # 40% validation sample
                            rand.seed = 15)

# LINEAR MODEL using logit regression ---------------------------

# MODEL 1
LinearVC1<-glm(formula = APURCH ~ age + gendm + atmcrd + paydep + DUMNOCHQ + 
                 BALCHQ + DUMNOSAV + BALSAV + TOTDEP + BALLOAN + DUMNOLOC + 
                 BALLOC + BALMRGG + NEWLOC + NEWMRGG + TXBRAN + TXATM + TXPOS + 
                 TXCHQ + TXWEB + TXTEL + TOTSERV + CHNMSERV + CHNMPRD + 
                 valsegm + numrr1 + avginc1 + avginv1,
                  data = filter(VC, Sample == "Estimation"),
                  family = binomial(logit))

summary(LinearVC1)
# Remove NEWMRGG

# MODEL 2
LinearVC2<-glm(formula = APURCH ~ age + gendm + atmcrd + paydep + 
                DUMNOCHQ + BALCHQ + DUMNOSAV + BALSAV + TOTDEP + BALLOAN +
                DUMNOLOC + BALLOC + BALMRGG + NEWLOC + TXBRAN + 
                TXATM + TXPOS + TXCHQ + TXWEB + TXTEL + TOTSERV + CHNMSERV + 
                CHNMPRD + valsegm + numrr1 + avginc1 + avginv1,
              data = filter(VC, Sample == "Estimation"),
              family = binomial(logit))

summary(LinearVC2)
# Remove CHNMPRD

# MODEL 3
LinearVC3<-glm(formula = APURCH ~ age + gendm + atmcrd + paydep + 
                 DUMNOCHQ + BALCHQ + DUMNOSAV + BALSAV + TOTDEP + BALLOAN +
                 DUMNOLOC + BALLOC + BALMRGG + NEWLOC + TXBRAN + 
                 TXATM + TXPOS + TXCHQ + TXWEB + TXTEL + TOTSERV + CHNMSERV + 
                 valsegm + numrr1 + avginc1 + avginv1,
               data = filter(VC, Sample == "Estimation"),
               family = binomial(logit))

summary(LinearVC3)
# Remove avginv1

# MODEL 4
LinearVC4<-glm(formula = APURCH ~ age + gendm + atmcrd + paydep + 
                 DUMNOCHQ + BALCHQ + DUMNOSAV + BALSAV + TOTDEP + BALLOAN +
                 DUMNOLOC + BALLOC + BALMRGG + NEWLOC + TXBRAN + 
                 TXATM + TXPOS + TXCHQ + TXWEB + TXTEL + TOTSERV + CHNMSERV + 
                 valsegm + numrr1 + avginc1,
               data = filter(VC, Sample == "Estimation"),
               family = binomial(logit))

summary(LinearVC4)
# Remove TOTDEP

# MODEL 5
LinearVC5<-glm(formula = APURCH ~ age + gendm + atmcrd + paydep + 
                 DUMNOCHQ + BALCHQ + DUMNOSAV + BALSAV + BALLOAN +
                 DUMNOLOC + BALLOC + BALMRGG + NEWLOC + TXBRAN + 
                 TXATM + TXPOS + TXCHQ + TXWEB + TXTEL + TOTSERV + CHNMSERV + 
                 valsegm + numrr1 + avginc1,
               data = filter(VC, Sample == "Estimation"),
               family = binomial(logit))

summary(LinearVC5)
# Remove numrr1

# MODEL 6
LinearVC6<-glm(formula = APURCH ~ age + gendm + atmcrd + paydep + 
                 DUMNOCHQ + BALCHQ + DUMNOSAV + BALSAV + BALLOAN +
                 DUMNOLOC + BALLOC + BALMRGG + NEWLOC + TXBRAN + 
                 TXATM + TXPOS + TXCHQ + TXWEB + TXTEL + TOTSERV + CHNMSERV + 
                 valsegm + avginc1,
               data = filter(VC, Sample == "Estimation"),
               family = binomial(logit))

summary(LinearVC6)
# Remove gendm

# MODEL 7  AIC = 3554.4
LinearVC7<-glm(formula = APURCH ~ age + atmcrd + paydep + 
                 DUMNOCHQ + BALCHQ + DUMNOSAV + BALSAV + BALLOAN +
                 DUMNOLOC + BALLOC + BALMRGG + NEWLOC + TXBRAN + 
                 TXATM + TXPOS + TXCHQ + TXWEB + TXTEL + TOTSERV + CHNMSERV + 
                 valsegm + avginc1,
               data = filter(VC, Sample == "Estimation"),
               family = binomial(logit))

summary(LinearVC7)
# Remove TXCHQ

# MODEL 8  AIC = 3552.8
LinearVC8<-glm(formula = APURCH ~ age + atmcrd + paydep + 
                 DUMNOCHQ + BALCHQ + DUMNOSAV + BALSAV + BALLOAN +
                 DUMNOLOC + BALLOC + BALMRGG + NEWLOC + TXBRAN + 
                 TXATM + TXPOS + TXWEB + TXTEL + TOTSERV + CHNMSERV + 
                 valsegm + avginc1,
               data = filter(VC, Sample == "Estimation"),
               family = binomial(logit))

summary(LinearVC8)

Anova(LinearVC8)

# Recode the value segments A, B and C into top 20% 
VC$New.valsegm <-
  recode_factor(VC$valsegm,
                'A' = "ABC",
                'B' = "ABC",
                'C' = "ABC")
View(VC)

# Run with recoded variable 

# MODEL 9
LinearVC9<-glm(formula = APURCH ~ age + atmcrd + paydep + 
                 DUMNOCHQ + BALCHQ + DUMNOSAV + BALSAV + BALLOAN +
                 DUMNOLOC + BALLOC + BALMRGG + NEWLOC + TXBRAN + 
                 TXATM + TXPOS + TXWEB + TXTEL + TOTSERV + CHNMSERV + 
                 New.valsegm + avginc1,
               data = filter(VC, Sample == "Estimation"),
               family = binomial(logit))

summary(LinearVC9)

Anova(LinearVC9)
# Remove TXATM

# MODEL 10  AIC = 3552.8
LinearVC10<-glm(formula = APURCH ~ age + atmcrd + paydep + 
                 DUMNOCHQ + BALCHQ + DUMNOSAV + BALSAV + BALLOAN +
                 DUMNOLOC + BALLOC + BALMRGG + NEWLOC + TXBRAN + 
                 TXPOS + TXWEB + TXTEL + TOTSERV + CHNMSERV + 
                 New.valsegm + avginc1,
               data = filter(VC, Sample == "Estimation"),
               family = binomial(logit))

summary(LinearVC10)
# Remove avginc1

# MODEL 11  AIC = 3551.3
LinearVC11<-glm(formula = APURCH ~ age + atmcrd + paydep + 
                  DUMNOCHQ + BALCHQ + DUMNOSAV + BALSAV + BALLOAN +
                  DUMNOLOC + BALLOC + BALMRGG + NEWLOC + TXBRAN + 
                  TXPOS + TXWEB + TXTEL + TOTSERV + CHNMSERV + New.valsegm,
                data = filter(VC, Sample == "Estimation"),
                family = binomial(logit))

summary(LinearVC11)
# Remove BALLOC

# MODEL 12  AIC = 3551
LinearVC12<-glm(formula = APURCH ~ age + atmcrd + paydep + 
                  DUMNOCHQ + BALCHQ + DUMNOSAV + BALSAV + BALLOAN +
                  DUMNOLOC + BALMRGG + NEWLOC + TXBRAN + TXPOS + TXWEB + 
                  TXTEL + TOTSERV + CHNMSERV + New.valsegm,
                data = filter(VC, Sample == "Estimation"),
                family = binomial(logit))

summary(LinearVC12)
# Remove TXWEB

# MODEL 13  AIC = 3550.7
LinearVC13<-glm(formula = APURCH ~ age + atmcrd + paydep + 
                  DUMNOCHQ + BALCHQ + DUMNOSAV + BALSAV + BALLOAN +
                  DUMNOLOC + BALMRGG + NEWLOC + TXBRAN + TXPOS + 
                  TXTEL + TOTSERV + CHNMSERV + New.valsegm,
                data = filter(VC, Sample == "Estimation"),
                family = binomial(logit))

summary(LinearVC13)
# Remove TXPOS

# MODEL 14  AIC = 3550.2
LinearVC14<-glm(formula = APURCH ~ age + atmcrd + paydep + 
                  DUMNOCHQ + BALCHQ + DUMNOSAV + BALSAV + BALLOAN +
                  DUMNOLOC + BALMRGG + NEWLOC + TXBRAN + 
                  TXTEL + TOTSERV + CHNMSERV + New.valsegm,
                data = filter(VC, Sample == "Estimation"),
                family = binomial(logit))

summary(LinearVC14)
# Remove DUMNOLOC

# MODEL 15  AIC = 3549.8
LinearVC15<-glm(formula = APURCH ~ age + atmcrd + paydep + DUMNOCHQ + BALCHQ + 
                  DUMNOSAV + BALSAV + BALLOAN + BALMRGG + NEWLOC + TXBRAN + 
                  TXTEL + TOTSERV + CHNMSERV + New.valsegm,
                data = filter(VC, Sample == "Estimation"),
                family = binomial(logit))

summary(LinearVC15)

Anova(LinearVC15)

# valsegmE will combined into ABC
VC$New.valsegm <-
  recode_factor(VC$valsegm,
                'A' = "ABCE",
                'B' = "ABCE",
                'C' = "ABCE",
                'E' = "ABCE")
View(VC)

# Run with recoded variable

# MODEL 16  AIC = 3548.6 (MINIMIZED AIC)
LinearVC16<-glm(formula = APURCH ~ age + atmcrd + paydep + DUMNOCHQ + BALCHQ + 
                  DUMNOSAV + BALSAV + BALLOAN + BALMRGG + NEWLOC + TXBRAN + 
                  TXTEL + TOTSERV + CHNMSERV + New.valsegm,
                data = filter(VC, Sample == "Estimation"),
                family = binomial(logit))

summary(LinearVC16)
# Remove NEWLOC

# MODEL 17
LinearVC17<-glm(formula = APURCH ~ age + atmcrd + paydep + DUMNOCHQ + BALCHQ + 
                  DUMNOSAV + BALSAV + BALLOAN + BALMRGG + TXBRAN + 
                  TXTEL + TOTSERV + CHNMSERV + New.valsegm,
                data = filter(VC, Sample == "Estimation"),
                family = binomial(logit))

summary(LinearVC17)
# Remove TXTEL

# MODEL 18
LinearVC18<-glm(formula = APURCH ~ age + atmcrd + paydep + DUMNOCHQ + BALCHQ + 
                  DUMNOSAV + BALSAV + BALLOAN + BALMRGG + TXBRAN + TOTSERV + 
                  CHNMSERV + New.valsegm,
                data = filter(VC, Sample == "Estimation"),
                family = binomial(logit))

summary(LinearVC18)
# Remove BALLOAN

# MODEL 19 AIC = 3555.4
LinearVC19<-glm(formula = APURCH ~ age + atmcrd + paydep + DUMNOCHQ + BALCHQ + 
                  DUMNOSAV + BALSAV + BALMRGG + TXBRAN + TOTSERV + CHNMSERV + 
                  New.valsegm,
                data = filter(VC, Sample == "Estimation"),
                family = binomial(logit))

summary(LinearVC19)
# All variables now statistically significant
# Remove DUMNOCHQ

# MODEL 20 AIC = 3559.3
LinearVC20<-glm(formula = APURCH ~ age + atmcrd + paydep + BALCHQ + DUMNOSAV + 
                  BALSAV + BALMRGG + TXBRAN + TOTSERV + CHNMSERV + New.valsegm,
                data = filter(VC, Sample == "Estimation"),
                family = binomial(logit))

summary(LinearVC20)
# Remove DUMNOSAV

# MODEL 21 AIC = 3560.2
LinearVC21<-glm(formula = APURCH ~ age + atmcrd + paydep + BALCHQ + 
                  BALSAV + BALMRGG + TXBRAN + TOTSERV + CHNMSERV + New.valsegm,
                data = filter(VC, Sample == "Estimation"),
                family = binomial(logit))

summary(LinearVC21)
# AIC is increasing


# --- LIFT CHARTS ----
# Run with estimation sample
lift.chart(modelList = c("LinearVC1", "LinearVC16", "LinearVC19", "LinearVC21"),
           data = filter(VC, Sample == "Estimation"),
           targLevel = "Y", 
           trueResp = 0.022,
           type = "cumulative",
           sub = "Estimation Set")

# Run with validation sample
lift.chart(modelList = c("LinearVC1", "LinearVC16", "LinearVC19", "LinearVC21"),
           data = filter(VC, Sample == "Validation"),
           targLevel = "Y", 
           trueResp = 0.022, 
           type = "cumulative", 
           sub = "Validation Set")


## ---------- EXPLORING NON LINEAR RELATIONSHIPS -------------------------

# Recode "APURCH" factor variable to be numerical for easier visualizations
VC$APURCH.Num <- if_else(VC$APURCH == "Y",1,0)

# After binning, create plot of means for APURCH and numeric variables


# ---- age
VC$age.Cat <- binVariable(VC$age, bins = 4,
                                 method = "proportions",
                                 labels = NULL) 

plotmeans(APURCH.Num ~ age.Cat, data = VC)


# ---- BALCHQ
VC$BALCHQ.Cat <- binVariable(VC$BALCHQ, bins = 4,
                          method = "proportions",
                          labels = NULL) 

plotmeans(APURCH.Num ~ BALCHQ.Cat, data = VC)


# ---- BALSAV <-- CANNOT BE BINNED
VC$BALSAV.Cat <- binVariable(VC$BALSAV, bins = 4,
                             method = "proportions",
                             labels = NULL) 

plotmeans(APURCH.Num ~ BALSAV.Cat, data = VC)


# ---- BALLOAN<-- CANNOT BE BINNED
VC$BALLOAN.Cat <- binVariable(VC$BALLOAN, bins = 4,
                             method = "proportions",
                             labels = NULL) 

plotmeans(APURCH.Num ~ BALLOAN.Cat, data = VC)


# ---- BALMRGG <-- CANNOT BE BINNED
VC$BALMRGG.Cat <- binVariable(VC$BALMRGG, bins = 4,
                             method = "proportions",
                             labels = NULL) 

plotmeans(APURCH.Num ~ BALMRGG.Cat, data = VC)


# ---- TXBRAN
VC$TXBRAN.Cat <- binVariable(VC$TXBRAN, bins = 4,
                             method = "proportions",
                             labels = NULL) 

plotmeans(APURCH.Num ~ TXBRAN.Cat, data = VC)



# ---- TXTEL <-- CANNOT BE BINNED
VC$TXTEL.Cat <- binVariable(VC$TXTEL, bins = 4,
                             method = "proportions",
                             labels = NULL) 

plotmeans(APURCH.Num ~ TXTEL.Cat, data = VC)


# ---- TOTSERV <-- CANNOT BE BINNED
VC$TOTSERV.Cat <- binVariable(VC$TOTSERV, bins = 3,
                             method = "proportions",
                             labels = NULL) 

plotmeans(APURCH.Num ~ TOTSERV.Cat, data = VC)


## --------- MODEL WITH TRANSFORMED VARIABLES -----------------------------

# Transform numeric variables
VC$Log.age <- log(VC$age + 1)
VC$Exp.BALCHQ <- exp(VC$BALCHQ)
VC$Log.TXBRAN <- log(VC$TXBRAN + 1) 

# Check McFadden R-square for LinearVC16 = 0.085
MRtxt <- "McFadden Rsquared :"
MR2.3 <- round(1 - (LinearVC16$deviance / LinearVC16$null.deviance),digits=3)
print(paste(MRtxt,MR2.3))

# Change Linear Model 16 variables into transformed variables 

# -- Log MODEL 1 - Log.Age
TrnVC1<-glm(formula = APURCH ~ Log.age + atmcrd + paydep + DUMNOCHQ + BALCHQ + 
              DUMNOSAV + BALSAV + BALLOAN + BALMRGG + NEWLOC + 
              TXBRAN + TXTEL + TOTSERV + CHNMSERV + New.valsegm,
            data = filter(VC, Sample == "Estimation"),
            family = binomial(logit))

summary(TrnVC1)

MR2.3 <- round(1 - (TrnVC1$deviance / TrnVC1$null.deviance),digits=3)
print(paste(MRtxt,MR2.3))

# Decreased in stat. sig., AIC = 3550.4, McF = 0.084


# -- Log MODEL 2 - Exp.BALCHQ
TrnVC2<-glm(formula = APURCH ~ age + atmcrd + paydep + DUMNOCHQ + Exp.BALCHQ + 
                  DUMNOSAV + BALSAV + BALLOAN + BALMRGG + NEWLOC + 
                  TXBRAN + TXTEL + TOTSERV + CHNMSERV + New.valsegm,
                data = filter(VC, Sample == "Estimation"),
                family = binomial(logit))

summary(TrnVC2)

MR2.3 <- round(1 - (TrnVC2$deviance / TrnVC2$null.deviance),digits=3)
print(paste(MRtxt,MR2.3))

# Error - Exp.BALCHQ is now infinite


# -- Log MODEL 3 - Log.TXBRAN
TrnVC3<-glm(formula = APURCH ~ age + atmcrd + paydep + DUMNOCHQ + BALCHQ + 
              DUMNOSAV + BALSAV + BALLOAN + BALMRGG + NEWLOC + 
              Log.TXBRAN + TXTEL + TOTSERV + CHNMSERV + New.valsegm,
            data = filter(VC, Sample == "Estimation"),
            family = binomial(logit))

summary(TrnVC3)

MR2.3 <- round(1 - (TrnVC3$deviance / TrnVC3$null.deviance),digits=3)
print(paste(MRtxt,MR2.3))

# Increased in stat. sig., AIC = 3542.4, McF = 0.086


# -- Log MODEL 4 - Log.TXBRAN
TrnVC4<-glm(formula = APURCH ~ age + atmcrd + paydep + DUMNOCHQ + BALCHQ + 
              DUMNOSAV + BALSAV + BALLOAN + BALMRGG + NEWLOC + 
              Log.TXBRAN + TXTEL + TOTSERV + CHNMSERV + New.valsegm,
            data = filter(VC, Sample == "Estimation"),
            family = binomial(logit))

summary(TrnVC4)

MR2.3 <- round(1 - (TrnVC4$deviance / TrnVC4$null.deviance),digits=3)
print(paste(MRtxt,MR2.3))

# AIC = 3542.4, McF = 0.086


## Continue removing insignificant variables to min AIC and max McF

# -- Log MODEL 5 - Remove NEWLOC   
TrnVC5<-glm(formula = APURCH ~ age + atmcrd + paydep + DUMNOCHQ + BALCHQ + 
              DUMNOSAV + BALSAV + BALLOAN + BALMRGG + 
              Log.TXBRAN + TXTEL + TOTSERV + CHNMSERV + New.valsegm,
            data = filter(VC, Sample == "Estimation"),
            family = binomial(logit))

summary(TrnVC5)

MR2.3 <- round(1 - (TrnVC5$deviance / TrnVC5$null.deviance),digits=3)
print(paste(MRtxt,MR2.3))

# AIC = 3543.1 (increased), McF = 0.085 (decreased)


# -- Log MODEL 6 - Remove TXTEL   
TrnVC6<-glm(formula = APURCH ~ age + atmcrd + paydep + DUMNOCHQ + BALCHQ + 
              DUMNOSAV + BALSAV + BALLOAN + BALMRGG + 
              Log.TXBRAN + TOTSERV + CHNMSERV + New.valsegm,
            data = filter(VC, Sample == "Estimation"),
            family = binomial(logit))

summary(TrnVC6)

MR2.3 <- round(1 - (TrnVC6$deviance / TrnVC6$null.deviance),digits=3)
print(paste(MRtxt,MR2.3))

# AIC = 3547.4 (increased), McF = 0.084 (decreased)


# -- Log MODEL 7 - Remove BALLOAN   
TrnVC7<-glm(formula = APURCH ~ age + atmcrd + paydep + DUMNOCHQ + BALCHQ 
            + DUMNOSAV + BALSAV + BALMRGG + Log.TXBRAN + TOTSERV + CHNMSERV + New.valsegm,
            data = filter(VC, Sample == "Estimation"),
            family = binomial(logit))

summary(TrnVC7)

MR2.3 <- round(1 - (TrnVC7$deviance / TrnVC7$null.deviance),digits=3)
print(paste(MRtxt,MR2.3))

# AIC = 3549.1 (increased), McF = 0.083 (decreased)

## ------- CHECKING MODEL PERFORMANCE -------------------------------------

# Create lift chart of best linear model, and several transformed models
# Run with validation sample
lift.chart(modelList = c("LinearVC16", "TrnVC4", "TrnVC7"),
           data = filter(VC, Sample == "Validation"),
           targLevel = "Y", 
           trueResp = 0.022, 
           type = "cumulative", 
           sub = "Validation Set")


## ---------- FINAL CUMULATIVE AND INCREMENTAL LIFT CHART ------------------

# Final weighted cumulative response lift chart of final model
# Run with validation sample
lift.chart(modelList = c("TrnVC4"),
           data = filter(VC, Sample == "Validation"),
           targLevel = "Y", 
           trueResp = 0.022, 
           type = "cumulative", 
           sub = "Validation Set")

# Final Incremental Lift chart of final model
# Run with Validation sample
lift.chart(modelList = c("TrnVC4"),
           data = filter(VC, Sample == "Validation"),
           targLevel = "Y", 
           trueResp = 0.022, 
           type = "incremental", 
           sub = "Validation Set")




