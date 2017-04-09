setwd("C:/Users/Shreyas/Desktop/R Files")
insur = read.csv("SwedishMotorInsurance.csv")
head(insur)
summary(insur)
dim(insur)
#the dimensions of the dataset are 2182 x 7 
#through summary we can see that the mean of kilometre is 2.986 ie, 10,000-15,000
#similarly the maximum number of claims are 3338 and mean is 51.97 
##we perform corelation to find out the relation between Claims and Payment
b=cor(insur$Claims,insur$Payment)
b*100 #percentage of corelation between claims and payments
#hence we can see that the corelation between claims and payment are 0.9954 ie, 99% corelated , 
plot(insur$Claims,insur$Payment)
c=cor(insur$Claims,insur$Insured)
c*100
#claims and insured have corelation of 91% 
d=cor(insur$Insured,insur$Payment)
d*100
##Payment and Insured has Corelation of 93% 
plot(insur$Claims,insur$Insured)
plot(insur$Payment,insur$Insured)
##we can see that the plot between claims and payment was linear
##we now use linear regression for Payments vs the rest of the data
reg=lm(insur$Payment~insur$Claims+insur$Insured+insur$Make+insur$Bonus+insur$Zone+insur$Kilometres)
summary(reg)
plot(reg)

aggkm=apply(insur[,c(5,6,7)],2,function(x) tapply(x,insur$Kilometres,mean))
aggkm
##in these 5 zones we could see 1st km dist has been more insured than others with 2nd being close to it
##the amount of claims in the 2nd km distribution is higher hence more payments
aggzone=apply(insur[,c(5,6,7)],2,function(x) tapply(x,insur$Zone,mean))
aggzone
##zone number 4 has higher number of insured vehicle and more amount of claims and payment is more due to claims
aggbonus = apply(insur[,c(5,6,7)],2,function(y) tapply(y,insur$Bonus,mean))
aggbonus
##here we can see that variations are low with 7th group having more insured count
#to find if claims is affected by km,zone,bonusand make
regcl = lm(insur$Claims~insur$Zone+insur$Bonus+insur$Kilometres+insur$Make)
summary(regcl)
plot(regcl)
##here we can see that all the factors affect the Claims they are significant (pvalues)

