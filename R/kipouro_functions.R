

#' Regression standardized probabilities from mexhaz models with competing risks
#'
#' This function is the work horse function in csprobPar
#'
#' @param model1 Mexhaz model for event of interest
#' @param model2 Mexhaz model for competing risk, usually death
#' @param time.max Max time to predict on, should be within observed data range, number
#' @param subdiv How many time points to predict on, number
#' @param data.val Dataframe to predict on, with counterfactial exposure level, dataframe
#' @param alpha Alpha level, usually 0.05 by convention, number
#'
#' @noRd
#' @return A list
#'
#' @examples
#' colon_death <- colon[colon$etype == 2, ] # Select rows with event indicator and follow-up time for death
#' colon_recur <- colon[colon$etype == 1, ] # Select rows with event indicator and follow-up time for recurrence
#' predict_df <- colon_death
#' predict_df$rx <- "Lev"
#'
#' myk_death <- quantile(colon_death$time/365.25, probs=c(1/3,2/3))
#' myk_recur <- quantile(colon_recur$time/365.25, probs=c(1/3,2/3))
#'
#' maxt = 9
#' fragm = maxt / 0.25 # 36, for predictions every quarter
#'
#' recur_mod <- mexhaz(Surv(time/365.25, status) ~ rx + sex + age + nodes, base = "exp.bs", degree = 3, knots = myk_recur, verbose = 0, print.level = 0, data = colon_recur)
#' death_mod <- mexhaz(Surv(time/365.25, status) ~ rx + sex + age + nodes, base = "exp.bs", degree = 3, knots = myk_death, verbose = 0, print.level = 0, data = colon_death)
#'
#' cumIncidence.CS(recur_mod, death_mod, maxt, fragm, data.val = predict_df)
#'
cumIncidence.CS <- function(model1,model2,time.max,subdiv, data.val=data.frame(.NotUsed=NA),alpha=0.05){
  time.pts <- seq(0,time.max,le=(subdiv+1))
  CstMult <- time.max/(2*subdiv)
  CstCI <- qnorm(1-alpha/2)

  # Or if you want to take into account the size of the population
  # CstCI <- qt(1-alpha/2,df=model£n.obs)

  Pred1 <- mexhaz::predict.mexhaz(model1,time.pts,data.val, include.gradient=T)
  Pred2 <- mexhaz::predict.mexhaz(model2,time.pts,data.val, include.gradient=T)

  ISurv1 <- Pred1$results$hazard*Pred1$results$surv*Pred2$results$surv
  CPr.1 <- cumsum(ISurv1+c(0,ISurv1[-subdiv]))*(time.max/(2*subdiv))

  ISurv2 <- Pred2$results$hazard*Pred1$results$surv*Pred2$results$surv
  CPr.2 <- cumsum(ISurv2+c(0,ISurv2[-subdiv]))*(time.max/(2*subdiv))

  ISurvT <- (Pred1$results$hazard+Pred2$results$hazard)*Pred1$results$surv*Pred2$results$surv
  CPrT<- cumsum(ISurvT+c(0,ISurvT[-subdiv]))*(time.max/(2*subdiv))
  SurvT <- 1-CPrT

  # Confidence intervals
  which.td1<- rownames(Pred1$vcov)[-c(1,which(rownames(Pred1$vcov) %in% model1$names.ph))]
  which.ntd1 <- c(rownames(Pred1$vcov)[-c(which(rownames(Pred1$vcov) %in% model1$names.ph))][1],
                  rownames(Pred1$vcov)[which(rownames(Pred1$vcov) %in% model1$names.ph)])

  which.td2<- rownames(Pred2$vcov)[-c(1,which(rownames(Pred2$vcov) %in% model2$names.ph))]

  which.ntd2 <- c(rownames(Pred2$vcov)[-c(which(rownames(Pred2$vcov) %in% model2$names.ph))][1],
                  rownames(Pred2$vcov)[which(rownames(Pred2$vcov) %in% model2$names.ph)])

  Vcov1 <- model1$vcov[c(which.ntd1,which.td1),c(which.ntd1,which.td1)]
  Vcov2 <- model2$vcov[c(which.ntd2,which.td2),c(which.ntd2,which.td2)]
  CovMat <- as.matrix(bdiag(Vcov1,Vcov2))
  AGrad11 <- (Pred1$grad.loghaz + Pred1$grad.logcum*(log(Pred1$results$surv)))[,c(which.ntd1,which.td1)]

  AGrad12 <- (Pred2$grad.logcum*(log(Pred2$results$surv)))[,c(which.ntd2,which.td2)]

  AGrad21 <- (Pred1$grad.logcum*(log(Pred1$results$surv)))[,c(which.ntd1,which.td1)]

  AGrad22 <- (Pred2$grad.loghaz + Pred2$grad.logcum*(log(Pred2$results$surv)))[,c(which.ntd2,which.td2)]


  AGrad1 <- cbind(AGrad11,AGrad12)
  AGrad2 <- cbind(AGrad21,AGrad22)
  AGradT <- cbind(AGrad21,AGrad12)

  Temp1 <- ISurv1*AGrad1
  Denom1 <- (1-CPr.1)*log(1-CPr.1)
  BGrad1 <- apply(Temp1,2,function(x) cumsum(x+c(0,x[-subdiv]))*(time.max/(2*subdiv)))

  TMatVar1 <- CovMat%*%t(BGrad1)
  Var1 <- sapply(1:subdiv,function(i) BGrad1[i,]%*%TMatVar1[,i])
  Var1b <- Var1/((1-CPr.1)*log(1-CPr.1))^2

  Temp2 <- ISurv2*AGrad2
  Denom2 <- (1-CPr.2)*log(1-CPr.2)^2
  BGrad2 <- apply(Temp2,2,function(x) cumsum(x+c(0,x[-subdiv]))*(time.max/(2*subdiv)))

  TMatVar2 <- CovMat%*%t(BGrad2)
  Var2 <- sapply(1:subdiv,function(i) BGrad2[i,]%*%TMatVar2[,i])
  Var2b <- Var2/((1-CPr.2)*log(1-CPr.2))^2

  BGradT <- AGradT/log(SurvT)
  TMatVarT <- CovMat%*%t(BGradT)
  VarT <- sapply(1:subdiv,function(i) BGradT[i,]%*%TMatVarT[,i])

  Transf <- function(x,vx,m){
    log(-log(x))+m*CstCI*sqrt(vx)
  }

  InvTransf <- function(x){
    exp(-exp(x))
  }

  Cr1Lo <- 1-InvTransf(Transf(1-CPr.1,Var1b,-1))
  Cr1Up <- 1-InvTransf(Transf(1-CPr.1,Var1b,1))
  Cr2Lo <- 1-InvTransf(Transf(1-CPr.2,Var2b,-1))
  Cr2Up <- 1-InvTransf(Transf(1-CPr.2,Var2b,1))
  CrTLo <- 1-InvTransf(Transf(SurvT,VarT,-1))
  CrTUp <- 1-InvTransf(Transf(SurvT,VarT,1))


  return(list("time"= time.pts, "frag"=subdiv,
              "CPr.1"=CPr.1,"CPr.2"=CPr.2,"CovMat"=CovMat,
              "BGrad1"=BGrad1,"BGrad2"=BGrad2,"CPr1Lo"= Cr1Lo,
              "CPr1Up"=Cr1Up,"CPr2Lo"= Cr2Lo,"CPr2Up"=Cr2Up,
              "Var1"=Var1, "Var2"=Var2,"Var1b"=Var1b,"Var2b"=Var2b))

}


#' Regression standardized probabilities from mexhaz models with competing risks
#'
#' This is a function that works without parallell computing
#' @param data_df Dataframe to predict on, with counterfactial exposure level, dataframe
#' @param modA Mexhaz model for event of interest
#' @param modB Mexhaz model for competing risk, usually death
#' @param time.max Max time to predict on, should be within observed data range, number
#' @param frag How many time points to predict on, number
#'
#' @return A list
#'
#' @noRd
#'
#' @examples
#'colon_death <- colon[colon$etype == 2, ] # Select rows with event indicator and follow-up time for death
#' colon_recur <- colon[colon$etype == 1, ] # Select rows with event indicator and follow-up time for recurrence
#' predict_df <- colon_death
#' predict_df$rx <- "Lev"
#'
#' myk_death <- quantile(colon_death$time/365.25, probs=c(1/3,2/3))
#' myk_recur <- quantile(colon_recur$time/365.25, probs=c(1/3,2/3))
#'
#' maxt = 9
#' fragm = maxt / 0.25 # 36, for predictions every quarter
#'
#' recur_mod <- mexhaz(Surv(time/365.25, status) ~ rx + sex + age + nodes, base = "exp.bs", degree = 3, knots = myk_recur, verbose = 0, print.level = 0, data = colon_recur)
#' death_mod <- mexhaz(Surv(time/365.25, status) ~ rx + sex + age + nodes, base = "exp.bs", degree = 3, knots = myk_death, verbose = 0, print.level = 0, data = colon_death)
#'
#' csprob(predict_df, recur_mod, death_mod, maxt, fragm)
#'
csprob<-function(data_df, modA, modB, time.max, frag){
  results_list<- list()
  for (y in 1:nrow(data_df)){
    results_list[[y]] <- cumIncidence.CS(modA, modB,time.max,
                                         subdiv=frag,
                                         data.val=data_df[y,])
  }

  return(results_list)
}


#' Internal function
#'
#' @param BGrad.ls XX
#' @param frag XX
#' @param N XX
#' @param p_dim XX
#'
#' @noRd
#'
#' @return list
#'
#' @examples XX
BGrad_func <- function(BGrad.ls,frag,N, p_dim){
  BGrad.3da <- array(unlist(BGrad.ls),dim = c(frag,p_dim,N))
  BGrad.3d <- aperm(BGrad.3da,dim=c(3,1,2))
  NBGrad.ls <- plyr::alply(BGrad.3d,3)
  return(NBGrad.ls)

}

Var_func <- function (x,w,NBGrad, CovMat){
  BGrad<- w%*%NBGrad[[x]]
  TMatVar <- CovMat%*%t(BGrad)
  res<- BGrad%*%TMatVar
  return(res)
}

Transf <- function(x,vx,m){log(-log(x))+m*qnorm(0.975)*sqrt(vx)}

InvTransf <- function(x){exp(-exp(x))}

predict.prob <- function(csprobObj, pop){
  CovMat <- csprobObj[[1]]$CovMat
  p_dim <- dim(csprobObj[[1]]$CovMat)[1]
  N <- length(csprobObj)
  frag <- csprobObj[[1]]$frag

  if (pop==FALSE){
    BGrad1.ls <- lapply(1:N, function(x) csprobObj[[x]]$BGrad1)
    BGrad2.ls <- lapply(1:N, function(x) csprobObj[[x]]$BGrad2)
    CPr1.df <- sapply(1:N, function(x) csprobObj[[x]]$CPr.1)
    CPr2.df <- sapply(1:N, function(x) csprobObj[[x]]$CPr.2)
    CPr1Lo.df <- sapply(1:N, function(x) csprobObj[[x]]$CPr1Lo)
    CPr1Up.df <- sapply(1:N, function(x) csprobObj[[x]]$CPr1Up)
    CPr2Lo.df <- sapply(1:N, function(x) csprobObj[[x]]$CPr2Lo)
    CPr2Up.df <- sapply(1:N, function(x) csprobObj[[x]]$CPr2Up)


    return(list("time"=csprobObj[[1]]$time,"frag"=frag,
                "BGrad1.ls"=BGrad1.ls, "BGrad2.ls"=BGrad2.ls,
                "CPr1Lo.df"=CPr1Lo.df,"CPr1Up.df"=CPr1Up.df,
                "CPr2Lo.df"=CPr2Lo.df,"CPr2Up.df"=CPr2Up.df,
                "CPr1.df"=CPr1.df,"CPr2.df"=CPr2.df))
  }
  if (pop==TRUE){
    w <- matrix(rep(1/N,N), nrow =1)
    BGrad1.ls<- lapply(1:N, function(x) csprobObj[[x]]$BGrad1)
    BGrad2.ls<- lapply(1:N, function(x) csprobObj[[x]]$BGrad2)
    CPr1.df <- sapply(1:N, function(x) csprobObj[[x]]$CPr.1)
    CPr2.df <- sapply(1:N, function(x) csprobObj[[x]]$CPr.2)


    CPr.1 <- w%*%t(CPr1.df)
    CPr.2 <- w%*%t(CPr2.df)

    NBGrad1.ls <- BGrad_func(BGrad1.ls,frag,N,p_dim)
    NBGrad2.ls <- BGrad_func(BGrad2.ls,frag,N,p_dim)


    Var1 <- data.frame(sapply(1:frag, function(x) Var_func(x,w,NBGrad1.ls, CovMat) ))
    Var1b <- Var1/((1-CPr.1)*log(1-CPr.1))^2
    CPr1Lo <- 1-InvTransf(Transf(1-CPr.1,Var1b,-1))
    CPr1Up <- 1-InvTransf(Transf(1-CPr.1,Var1b,1))

    Var2 <- data.frame(sapply(1:frag, function(x) Var_func(x,w,NBGrad2.ls, CovMat) ))
    Var2b <- Var2/((1-CPr.2)*log(1-CPr.2))^2
    CPr2Lo <- 1-InvTransf(Transf(1-CPr.2,Var2b,-1))
    CPr2Up <- 1-InvTransf(Transf(1-CPr.2,Var2b,1))
    return(list("time"=csprobObj[[1]]$time[-1],"frag"=frag,
                "BGrad1.ls"=BGrad1.ls, "BGrad2.ls"=BGrad2.ls,
                "CPr.1"=CPr.1[1,],"CPr.2"=CPr.2[1,],"NBGrad1.ls"=NBGrad1.ls,
                "NBGrad2.ls"=NBGrad2.ls,"Var1"=Var1,"Var2"=Var2, "Var1b"=Var1b,
                "Var2b"=Var2b, "CPr1Lo"=CPr1Lo[,1],"CPr1Up"=CPr1Up[,1],
                "CPr2Lo"=CPr2Lo[,1],"CPr2Up"=CPr2Up[,1], "CPr1.df"=CPr1.df,
                "CPr2.df"=CPr2.df,"CovMat"=CovMat))

  }
}


csprobdif<- function(predprob1, predprob2){

  N <- ncol(predprob1$CPr1.df)
  w<- matrix(rep(1/N,N), nrow =1)
  frag <- predprob1$frag
  CovMat <- predprob1$CovMat

  NBGrad1.ls <- lapply(1:frag, function (x) predprob1$NBGrad1.ls[[x]]- predprob2$NBGrad1.ls[[x]])
  NBGrad2.ls <- lapply(1:frag, function (x) predprob1$NBGrad2.ls[[x]]- predprob2$NBGrad2.ls[[x]])

  CP1.dif_df <- predprob1$CPr1.df-predprob2$CPr1.df
  CP2.dif_df <- predprob1$CPr2.df-predprob2$CPr2.df

  CP1.dif <-apply(CP1.dif_df,1,mean)
  CP2.dif<-apply(CP2.dif_df,1,mean)

  ci_form <- function(x,var,m){
    x+m*qnorm(0.975)*sqrt(var)
  }

  Var1 <-data.frame(sapply(1:frag, function(x) Var_func(x,w,NBGrad1.ls,CovMat)))
  Var2 <-data.frame(sapply(1:frag, function(x) Var_func(x,w,NBGrad2.ls,CovMat)))


  CPr1.difLo <- ci_form(CP1.dif,Var1,-1)[,1]
  CPr1.difUp <- ci_form(CP1.dif,Var1,+1)[,1]
  CPr2.difLo <- ci_form(CP2.dif,Var2,-1)[,1]
  CPr2.difUp <- ci_form(CP2.dif,Var2,+1)[,1]
  return(list("time"= predprob1$time,
              "ProbDif1"=CP1.dif, "ProbDif2"=CP2.dif,
              "ProbDif1Lo"=CPr1.difLo,"ProbDif1Up"=CPr1.difUp,
              "ProbDif2Lo"=CPr2.difLo,"ProbDif2Up"=CPr2.difUp))

}


csprobPar <- function(data_df, modA, modB, time.max, frag){
  foreach(y = 1:nrow(data_df),.combine = "rbind") %dopar% {
    library("mexhaz")
    library("Matrix")
    source("CumIncid.R")

    list(cumIncidence.CS(modA, modB,time.max,subdiv=frag, data.val=data_df[y,]))

  }
}


