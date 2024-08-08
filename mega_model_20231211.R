
setwd(".")

# libraries
library(plyr) #manipulating dfs
library(dplyr)
library(data.table)
library(ggplot2) #viz
library(plotly)

library(lme4) #glmer

df<- read.csv("megadf_onezero_formodel_20231221.csv", stringsAsFactors = F)

t<- df[is.na(df$prop_typclwint),]
#separate out northern and southern roosts, lets focus on southern roosts now 


winter_df<- subset(df, season == "winter" )

model.scld<- df
model.scld[,c(3,112)]<- NULL
################################# Make sure to rescale variables #######################
### rescale important variables to be between 0 and 1, scales packaged produced negative values when there wasn't any
normalized <- function(x, ...) {(x - min(x, ...)) / (max(x, ...) - min(x, ...))}

model.scld[, c(23:65)] <- lapply(model.scld[, c(23:65)], normalized) 

model.scld$camp<- as.factor(model.scld$camp)
model.scld$ghff.presence <- as.factor(model.scld$ghff.presence)
model.scld$lrff.presence <- as.factor(model.scld$lrff.presence)

t<-model.scld[is.na(model.scld$prop_typclwint),]


complete <-model.scld[complete.cases(model.scld),]
t<-model.scld[!complete.cases(model.scld),] 



winter<- subset(complete, season == "winter")


##########correlation of vars 
library(MASS)
library(car)
library(corrplot)

wint<-winter[c(22:108, 114,  116, 118, 120, 122:123)]
c<- cor(wint)
c<-as.matrix(c)


df<- as.matrix(df)
var_inv <- ginv(c)

colnames(var_inv) <- colnames(wint) 
rownames(var_inv) <- colnames(wint)
corrplot(c, method="circle", type = "lower", order="hclust", tl.col = "black", tl.cex = 0.45, cl.cex = 0.45  )
corrplot(c)
dev.off()

cor.mtest <- function(mat, ...) {
  mat <- as.matrix(mat)
  n <- ncol(mat)
  p.mat<- matrix(NA, n, n)
  diag(p.mat) <- 0
  for (i in 1:(n - 1)) {
    for (j in (i + 1):n) {
      tmp <- cor.test(mat[, i], mat[, j], ...)
      p.mat[i, j] <- p.mat[j, i] <- tmp$p.value
    }
  }
  colnames(p.mat) <- rownames(p.mat) <- colnames(mat)
  p.mat
}
# matrix of the p-value of the correlation
p.mat <- cor.mtest(wint)
corrplot(c, method="circle", type = "lower", order="hclust", tl.col = "black", tl.cex = 0.45, cl.cex = 0.45 )
corrplot(c, method="circle", type = "lower", order="hclust", tl.col = "black", tl.cex = 0.5, cl.cex = 0.5, p.mat = p.mat, sig.level = 0.05, insig = "blank"  )
df<- as.matrix(df)



################### optimize lambda 
#set lambdas... go from 0 to 10^5, in 10 log steps
lambda <- 10^seq(-3,5, length=10)


#dummy vectors of model fit values for each lambda: BIC, AIC, prediction error
BIC_vec <- rep(Inf, length(lambda))
AIC_vec <- rep(Inf, length(lambda))
Devianz_ma<-NULL
Coeff_ma<-NULL


library(glmmLasso)
family = gaussian(link = "logit")


#dummy vectors of model fit values for each lambda: BIC, AIC, prediction error
BIC_vec <- rep(Inf, length(lambda))
AIC_vec <- rep(Inf, length(lambda))
Devianz_ma<-NULL
Coeff_ma<-NULL

####tersring 
j<-1
for (j in 1:length(BIC_vec)){
  print(paste("Iteration ", j, sep=""))
  
  glm1w <- try( #i write out all  possible interaction effects... glmmLasso syntax doesn't allow for "*" notation
    glmmLasso(bff.presence ~ morI_atypclwint:mean_prcp_3mo_w +  morI_atypclwint:mean_tmax_3mo_w   +  morI_atypclwint:mean_tmin_3mo_w
              
              + morI_typclwint:mean_prcp_3mo_w +  morI_typclwint:mean_tmax_3mo_w   +  morI_typclwint:mean_tmin_3mo_w
              
              ##########  LAGGED  PRECIP ANOMALIES
              
              + morI_typclwint:prcp3mow_lag3mo
              
              + morI_typclwint:prcp3mow_lag6mo
              + morI_typclwint:prcp3mow_lag9mo
              + morI_typclwint:prcp3mow_lag12mo
              
              + morI_atypclwint:prcp3mow_lag3mo
              + morI_atypclwint:prcp3mow_lag6mo
              + morI_atypclwint:prcp3mow_lag9mo
              + morI_atypclwint:prcp3mow_lag12mo
              
               + morI_typclwint:tmin3mow_lag3mo
              + morI_typclwint:tmin3mow_lag6mo
              + morI_typclwint:tmin3mow_lag9mo
              + morI_typclwint:tmin3mow_lag12mo
              
              + morI_atypclwint:tmin3mow_lag3mo
              + morI_atypclwint:tmin3mow_lag6mo
              + morI_atypclwint:tmin3mow_lag9mo
              + morI_atypclwint:tmin3mow_lag12mo
              
              + morI_typclwint:tmax3mow_lag3mo
              + morI_typclwint:tmax3mow_lag6mo
              + morI_typclwint:tmax3mow_lag9mo
              + morI_typclwint:tmax3mow_lag12mo
              
              + morI_atypclwint:tmax3mow_lag3mo
              + morI_atypclwint:tmax3mow_lag6mo
              + morI_atypclwint:tmax3mow_lag9mo
              + morI_atypclwint:tmax3mow_lag12mo,
              
              
              rnd = list(camp=~1), data = winter, 
              family = gaussian(link = "logit"),
              lambda = lambda[j],
              switch.NR = TRUE,
              final.re = TRUE),
    silent = TRUE)
  
  
  # code to make it continue anyway if an error occurs
  if(class(glm1w)!="try-error")
  {
    
    #save BIC, AIC
    BIC_vec[j]<-glm1w$bic
    AIC_vec[j]<-glm1w$aic
    
    #save coefficient outputs
    Coeff_ma<-cbind(Coeff_ma,glm1w$coefficients)
    
    #save error (deviance) values
    y.hat<-predict(glm1w,winter)
    Devianz_ma[j]<-sum(family$dev.resids(winter$bff.presence, y.hat,wt=rep(1,length(y.hat))))
    
  }
}

lambda[which.min(BIC_vec)]



lambda[which.min(AIC_vec)]

## [1] 3

lambda[which.min(Devianz_ma)]
final_lambda<-lambda[which.min(BIC_vec)] 
summary(glm1w)






##### real glmmLasso no lambaa optimization
las4<- glmmLasso(bff.presence ~ morI_atypclwint:mean_prcp_3mo_w +  morI_atypclwint:mean_tmax_3mo_w   #+  morI_atypclwint:mean_tmin_3mo_w
                 
                 + morI_typclwint:mean_prcp_3mo_w +  morI_typclwint:mean_tmax_3mo_w  # +  morI_typclwint:mean_tmin_3mo_w
                 
                 ##########  LAGGED  PRECIP ANOMALIES
                 
                #   + morI_typclwint:prcp3mow_lag3mo
                 
                # + morI_typclwint:prcp3mow_lag6mo
                 + morI_typclwint:prcp3mow_lag9mo
                 + morI_typclwint:prcp3mow_lag12mo
                 
                #    + morI_atypclwint:prcp3mow_lag3mo
                # + morI_atypclwint:prcp3mow_lag6mo
                 + morI_atypclwint:prcp3mow_lag9mo
                 + morI_atypclwint:prcp3mow_lag12mo
                 
               #     + morI_typclwint:tmin3mow_lag3mo
               # + morI_typclwint:tmin3mow_lag6mo
                 + morI_typclwint:tmin3mow_lag9mo
                 + morI_typclwint:tmin3mow_lag12mo
                 
                #    + morI_atypclwint:tmin3mow_lag3mo
               # + morI_atypclwint:tmin3mow_lag6mo
                 + morI_atypclwint:tmin3mow_lag9mo
                 + morI_atypclwint:tmin3mow_lag12mo
                 
                #     + morI_typclwint:tmax3mow_lag3mo
               # + morI_typclwint:tmax3mow_lag6mo
                 + morI_typclwint:tmax3mow_lag9mo
                 + morI_typclwint:tmax3mow_lag12mo
                 
                #      + morI_atypclwint:tmax3mow_lag3mo
               #  + morI_atypclwint:tmax3mow_lag6mo
                 + morI_atypclwint:tmax3mow_lag9mo
                 + morI_atypclwint:tmax3mow_lag12mo,
                 
                 
                 rnd = list(camp=~1), data =winter, 
                 lambda =final_lambda, family = binomial(link="logit"), final.re=TRUE)
summary(las4)

print(las4, correlation = TRUE)

### removing highly correlated variables 
##############

logme0 <- glmer(bff.presence ~ morI_atypclwint:mean_prcp_3mo_w  #+  morI_atypclwint:mean_tmax_3mo_w  
                +  morI_atypclwint:mean_tmin_3mo_w
                
                + morI_typclwint:mean_prcp_3mo_w #+  morI_typclwint:mean_tmax_3mo_w   
                +  morI_typclwint:mean_tmin_3mo_w
                
                ##########  LAGGED  PRECIP ANOMALIES

                #+ morI_typclwint:prcp3mow_lag3mo
              #+ morI_typclwint:prcp3mow_lag6mo
                + morI_typclwint:prcp3mow_lag9mo
                + morI_typclwint:prcp3mow_lag12mo
                
                # + morI_atypclwint:prcp3mow_lag3mo
               # + morI_atypclwint:prcp3mow_lag6mo
                + morI_atypclwint:prcp3mow_lag9mo
                + morI_atypclwint:prcp3mow_lag12mo
                
                # + morI_typclwint:tmin3mow_lag3mo
               # + morI_typclwint:tmin3mow_lag6mo
                 + morI_typclwint:tmin3mow_lag9mo
                + morI_typclwint:tmin3mow_lag12mo
                
                # + morI_atypclwint:tmin3mow_lag3mo
               # + morI_atypclwint:tmin3mow_lag6mo
                + morI_atypclwint:tmin3mow_lag9mo
                + morI_atypclwint:tmin3mow_lag12mo
                
               #  + morI_typclwint:tmax3mow_lag3mo
               # + morI_typclwint:tmax3mow_lag6mo
                + morI_typclwint:tmax3mow_lag9mo
                + morI_typclwint:tmax3mow_lag12mo
                
                # + morI_atypclwint:tmax3mow_lag3mo
                # + morI_atypclwint:tmax3mow_lag6mo
                + morI_atypclwint:tmax3mow_lag9mo
                + morI_atypclwint:tmax3mow_lag12mo
                
                + (1 |camp),
                data = winter, family = binomial,
                control = glmerControl(optimizer = "bobyqa", optCtrl=list(maxfun=1e5)),
                nAGQ = 10)
summary(logme0)

print(logme0, correlation = TRUE)
