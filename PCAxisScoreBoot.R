PCAxisScoreBoot<- function(PCA, AxisNumber,reps) {
# IN RESPINSE TO REVIEWS OF V1: BOOTSTRAP TO SEE IF RATIO OF SAMPLES TO VARIABLES A PROBLEM for our PCA
bootvector<-NULL 
noisevector<-NULL #this is to record how many times you had to add "noise" to the Mg column
for (i in 1:reps) {
  # bootstrap sample the data
  boot_data<-sample_frac(PCA,AxisNumber,replace=TRUE)
  # the values for Mg are almost all 0.1. If your bootstrap sample has all 0.1 in the Mg column, then it can't do the PCA. Get around this by adding a little noise.
  if (mean(boot_data$Mg)==0.1){
    boot_data$Mg<-boot_data$Mg+runif(20, min=0.00001, max=0.00009)
    noisevector[i]<-1
  }
  # load your function to calclulate the PCA for your bootstrapped dataset 
  # and return the variance expakined by the 1st principal component
  getPrcVar <- function (df){
    prcs <- prcomp(df,center = TRUE, scale. = TRUE) # returns matrix
    #return(prcs$importance[2,1]) # pick out the thing we need
    return(summary(prcs)) # pick out the thing we need
  }
  # record the var explained by the 1st PCA
  bootvector[i]<-getPrcVar(boot_data)$importance[2,1]
  next
}

# bootvector
# hist(bootvector)
#proportion of the bootsrapped samples with #var of 1st PCA LOWER than the actual value. 
# sum(bootvector<summary(nest.env.pca.nosoil)$importance[2,1])/i

act.var<-cat("actual prop of variance explained by the PC Axis: ", (summary(nest.env.pca.nosoil)$importance[2,1]),"\n")
mean.var.boot<-cat("mean of the variance explained by PC Axis in bootstrapped runs: ",mean(bootvector),"\n")
sd.var.boot<-cat("sd of variance explained by 1st PC in bootstrapped runs: ",sd(bootvector),"\n")
prop.var.boot<-cat("proportion of the bootstrapped 'variance of 1st PCA' < 'actual var explained by 1st PCA': ", sum(bootvector<summary(nest.env.pca.nosoil)$importance[2,1])/i,"\n")


}
