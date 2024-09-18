### Sorting data for analysis

#Saving 'Home'
hm <- getwd()

## Setting working directory for data
setwd("Data")

### laoding data
load("ukc_ri.r")

ri_late

## Defining outcome
which(!is.na(ri$late_opDt))



ri_late$opTm <- as.numeric(as.Date(ri_late$late_opDt,"%Y-%m-%d")-as.Date(ri_late$T0,"%d%b%Y"))/30.44
ri_late$out <- ((!is.na(ri_late$opTm))|ri_late$cen==1)
#ri_late$out <- as.numeric(ri_late$opTm<ri_late$stime)
ri_late$out[which(is.na(ri_late$out))] <- 0
ri_late$t2 <- factor(ri_late$tx,labels=c("OR","EV","EV"))




# PSC
data_or  <- ri_late[which(ri_late$t2=="OR"),]
model <- glm(out~factor(I_sex)+diameter+factor(asa2)+(age)+htn+neck_lgth+bmi+i_crea,data=data_or,family="binomial")

summary(model)
anova(model)

data_or[1:3,]

modelri_late$asa2 <- factor(ri_late$asa,labels=c(1,1,3,3,3))
table(data_or$out,data_or$asa2)


