white.test <- function(lmobj, squares.only=FALSE)
{
  stopifnot(class(lmobj)=='lm')
  mydata <- lmobj$model
  mydata[,1] <- lmobj$residual^2
  fml <- lmobj$call$formula
  formula1 <- paste(fml[2],fml[1],fml[3])
  pvs <- attr(lmobj$terms,"term.labels")
  k <- length(pvs);
  n <- length(lmobj$fit)
  
  for(i in 1:k){
    tmp <- NULL;
    if(substr(pvs[i],1,2)=="I("){
      tmp2 <- substr(pvs[i],3, nchar(pvs[i])-1);
    }else{
      tmp2 <- pvs[i];
    }
    for(j in 1:nchar(tmp2)){
      tmp1 <- substr(tmp2,j,j)
      if(tmp1 == ":")
        tmp <- paste(tmp, "*", sep='')
      else
        tmp <- paste(tmp, tmp1, sep='')
    }
    pvs[i] <- tmp
  }
  formula2 <- paste(fml[2],fml[1])
  for(i in 1:k){
    if(i>1)
      formula2 <- paste(formula2, "+", sep='')
    formula2 <- paste(formula2, "I(", pvs[i],")",sep='')
    if(squares.only){
      formula2 <- paste(formula2, "+I(", pvs[i], 
                        "*", pvs[i], ")", sep = "")
    }else{
      for(j in i:k)
        formula2 <- paste(formula2,"+I(",pvs[i],
                          "*",pvs[j],")", sep='')
    }
  }
  
  method <- ifelse(squares.only,
                   "White test for constant variance, squares only",
                   "White test for constant variance")
  
  out <- lm(as.formula(formula2),data=mydata)
  if(summary(out)$r.squared == 1.0){
    RVAL <- NULL;
    warning("Test failed.  Possible reasons:\n\t (1) collinearity, or (2) sample size is not big enough for the White's test.");
  }else{
    LM = summary(out)$r.squared * n  
    names(LM) <- "White"
    df <- out$rank - 1
    names(df) <- "df";
    RVAL <- list(statistic = LM,
                 parameter = df,
                 method = method,
                 p.value= pchisq(LM,df,lower.tail=FALSE),
                 data.name=NULL)
    class(RVAL) <- "htest"
  }
  return(RVAL)
}
