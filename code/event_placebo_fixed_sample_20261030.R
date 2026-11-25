suppressPackageStartupMessages({library(data.table);library(DRDID)})
set.seed(20261030)
root <- 'results/paired'
args <- commandArgs(trailingOnly=TRUE)
outcome <- if(length(args))args[1] else 'cesd_z'
extended <- length(args)>1 && args[2]=='extended'
stopifnot(outcome%in%c('cesd_z','esteem_z'))
outroot <- if(extended)file.path(root,'extended_history')else root
out <- if(outcome=='cesd_z')outroot else file.path(outroot,'self_esteem')
dir.create(out,recursive=TRUE,showWarnings=FALSE)
d <- as.data.table(readRDS('inputs/audit_input.rds'))
d[,cesd_z:=get(outcome)]
people <- unique(d[,.(h_pid,g,family,female_base,lowinc_base,age_base)])
stopifnot(!anyDuplicated(people$h_pid),!anyNA(people$family))
ks <- if(extended)-5:3 else -4:3
shift <- 1-min(ks);basecol<-paste0('y',shift-2)
placebos <- c(if(extended)'fake_minus4','fake_minus3','fake_minus2')
fits <- list(); counts <- list(); verification <- list()
fitpair <- function(z,a,b){
 x <- model.matrix(~female_base+lowinc_base+age_base,z)
 y1 <- z[[a]]; y0 <- z[[b]]
 f <- drdid_panel(y1,y0,z$D,x,inffunc=TRUE)
 # Independent propensity fit and QR regression; no DRDID point-estimator code.
 ps <- pmin(glm.fit(x,z$D,family=binomial())$fitted.values,1-1e-6)
 beta <- qr.coef(qr(x[z$D==0,,drop=FALSE]),(y1-y0)[z$D==0])
 res <- y1-y0-drop(x%*%beta)
 wt <- z$D; wc <- (1-z$D)*ps/(1-ps)*(ps<.995)
 independent <- sum(wt*res)/sum(wt)-sum(wc*res)/sum(wc)
 stopifnot(abs(f$ATT-independent)<1e-7)
 list(att=f$ATT,ifunc=as.numeric(f$att.inf.func),id=z$h_pid,D=z$D,n=nrow(z),nt=sum(z$D),
      verification=abs(f$ATT-independent),maxps=max(ps),trimmed=sum(ps>=.995 & z$D==0))
}
# Outcome rows through g+3 are interviewed at g+4. The control anticipation
# buffer must also be within observed economic years, ending in 2023.
for(g0 in (2011-min(ks)):2019){
 eligible <- d[year==g0+min(ks) & (g==g0 | g==0 | g>g0+4),h_pid]
 z <- merge(people[h_pid%in%eligible],dcast(d[h_pid%in%eligible & year%in%(g0+ks),.(h_pid,k=paste0('y',year-g0+shift),cesd_z)],h_pid~k,value.var='cesd_z'),by='h_pid')
 z[,D:=as.integer(g==g0)]
 balanced <- z[complete.cases(z[,paste0('y',seq_along(ks)),with=FALSE])]
 for(mode in c('balanced','pair_complete')){
  for(k in ks){
   col <- paste0('y',k+shift)
   zz <- if(mode=='balanced')balanced else z[is.finite(get(col)) & is.finite(get(basecol))]
   counts[[length(counts)+1]] <- data.table(mode,g=g0,k,eligible_treated=sum(z$D),eligible_control=sum(1-z$D),treated=sum(zz$D),control=sum(1-zz$D))
   if(k== -2){
    f <- list(att=0,ifunc=rep(0,nrow(zz)),id=zz$h_pid,D=zz$D,n=nrow(zz),nt=sum(zz$D),verification=0,maxps=NA_real_,trimmed=0)
   }else f <- fitpair(zz,col,basecol)
   f$mode <- mode; f$g <- g0; f$label <- as.character(k)
   fits[[paste(mode,g0,k,sep='_')]] <- f
  }
 }
 for(p in placebos){
  pk <- -as.integer(sub('fake_minus','',p))
  f <- fitpair(balanced,paste0('y',pk+shift),paste0('y',pk-1+shift))
  f$mode <- 'balanced';f$g <- g0;f$label <- p
  fits[[paste('balanced',g0,p,sep='_')]] <- f
  f1 <- fits[[paste('balanced',g0,pk,sep='_')]]
  f0 <- fits[[paste('balanced',g0,pk-1,sep='_')]]
  verification[[length(verification)+1]] <- data.table(g=g0,placebo=p,point_error=abs(f$att-(f1$att-f0$att)),influence_error=max(abs(f$ifunc-(f1$ifunc-f0$ifunc))))
 }
}
ids <- people$h_pid
wald <- function(b,V){stat<-drop(t(b)%*%solve(V,b));data.table(stat,p=pchisq(stat,length(b),lower.tail=FALSE),q=length(b))}
results <- list(); tests <- list(); objects <- list()
for(mode in c('balanced','pair_complete')){
 labels <- c(as.character(ks),if(mode=='balanced')placebos)
 score <- matrix(0,nrow(people),length(labels),dimnames=list(ids,labels));est<-numeric(length(labels)); nts<-numeric(length(labels))
 for(j in seq_along(labels)){
  fs <- Filter(function(f)f$mode==mode && f$label==labels[j],fits)
  nt <- sum(vapply(fs,function(f)f$nt,0));nts[j]<-nt
  theta <- sum(vapply(fs,function(f)f$att*f$nt/nt,0));est[j]<-theta
  for(f in fs){
   ix<-match(f$id,ids)
   score[ix,j]<-score[ix,j]+f$nt/nt*f$ifunc/f$n+f$D*(f$att-theta)/nt
  }
 }
 fam <- rowsum(score,people$family,reorder=TRUE)
 V<-crossprod(fam);se<-sqrt(diag(V));person_se<-sqrt(colSums(score^2))
 # One multiplier per connected household; reused controls share the same multiplier.
 nz<-which(se>1e-12);display<-which(labels%in%as.character(ks) & se>1e-12)
 sims<-matrix(sample(c(-1,1),1999*nrow(fam),replace=TRUE),1999,nrow(fam))%*%fam
 crit<-as.numeric(quantile(apply(abs(sweep(sims[,display,drop=FALSE],2,se[display],'/')),1,max),.95))
 tab<-data.table(mode,label=labels,estimate=est,se,person_se,treated=nts,p=2*pnorm(-abs(est/se)),lower=est-1.96*se,upper=est+1.96*se,simultaneous_critical=crit)
 results[[mode]]<-tab
 pre<-which(labels%in%as.character(ks[ks< -2]))
 tests[[length(tests)+1]]<-cbind(data.table(mode,test='pre_relative_to_minus2'),wald(est[pre],V[pre,pre]))
 if(mode=='balanced'){
  ix<-which(labels%in%placebos)
  tests[[length(tests)+1]]<-cbind(data.table(mode,test='joint_pre_placebos'),wald(est[ix],V[ix,ix]))
 }
 objects[[mode]]<-list(estimate=est,labels=labels,score=score,family_score=fam,V=V)
}
fwrite(rbindlist(results),file.path(out,'fixed_sample_results.csv'))
fwrite(rbindlist(tests),file.path(out,'fixed_sample_tests.csv'))
fwrite(rbindlist(counts),file.path(out,'fixed_sample_counts.csv'))
fwrite(rbindlist(verification),file.path(out,'placebo_identity_verification.csv'))
fwrite(rbindlist(lapply(fits,function(f)data.table(mode=f$mode,g=f$g,label=f$label,estimate=f$att,n=f$n,treated=f$nt,maxps=f$maxps,trimmed=f$trimmed,independent_error=f$verification))),file.path(out,'fixed_sample_cohorts.csv'))
saveRDS(list(fits=fits,aggregates=objects),file.path(out,'fixed_sample_fits.rds'))
print(rbindlist(results));print(rbindlist(tests));print(rbindlist(verification))
