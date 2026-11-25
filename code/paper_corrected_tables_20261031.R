suppressPackageStartupMessages({library(data.table);library(fixest);library(did);library(ggplot2)})
setFixest_notes(FALSE);setDTthreads(4);set.seed(20261031)
O<-"results";T<-file.path(O,"tables");stopifnot(file.exists("docs/PLAN.md"))
OA<-"inputs"
P<-readRDS(file.path(OA,"income_definition_panels.rds"))
r<-as.data.table(readRDS("inputs/reconstructed_panel.rds"));setorder(r,h_pid,year)
c0<-fread(file.path(OA,"corrected_core_tables.csv"))
add<-c("h_pid","year","cesd","cesd_z","esteem","esteem_z","ide","log_real","log_real_l1","log_real_l2","log_real_l3","log_eq","log_eq_l1","log_eq_l2","log_eq_l3","real_income","income","income_l1","income_l2","income_l3","consumption","consumption_l1","cpi","liquid_l1","h01_1","female","h_hc","p03_12","h_cin","cesd_z_l1","esteem_z_l1","ide_f1","cesd_z_f1",paste0("p05_",9:19),"h11_3aq2","h11_3aq3","h11_3aq4","h11_3aq6","h11_3aq7")
x<-merge(as.data.table(P$real_log_available),r[,..add],by=c("h_pid","year"))
stopifnot(nrow(x)==nrow(P$real_log_available),min(x$year)>=2011)
setorder(x,h_pid,year);x[,family:=as.numeric(family)]
num<-function(v,d=3)ifelse(is.na(v),"",formatC(v,digits=d,format="f"))
star<-function(p)fcase(is.na(p),"",p<.01,"***",p<.05,"**",p<.10,"*",default="")
pv<-function(p)ifelse(is.na(p),"",ifelse(p<.001,"<0.001",formatC(p,digits=3,format="f")))
cf<-function(m,t,d=3){if(is.null(m)||!t%in%names(coef(m)))return("");ct<-coeftable(m);paste0(num(ct[t,1],d),star(ct[t,4])," (",num(ct[t,2],d),")")}
est<-function(m,t)if(t%in%names(coef(m)))unname(coef(m)[t])else NA_real_
dft<-function(m)as.numeric(degrees_freedom(m,"t"))
lc<-function(m,ts,w=rep(1,length(ts))){b<-coef(m)[ts];V<-vcov(m)[ts,ts,drop=FALSE];e<-sum(w*b);se<-sqrt(drop(t(w)%*%V%*%w));list(e=e,se=se,p=2*pt(-abs(e/se),dft(m)))}
jt<-function(m,ts){b<-coef(m)[ts];V<-vcov(m)[ts,ts];st<-drop(t(b)%*%solve(V,b))/length(ts);list(F=st,p=pf(st,length(ts),dft(m),lower.tail=FALSE))}
wr<-function(m)as.numeric(fitstat(m,"wr2")[[1]])
fe<-"h_pid+year";CL<-~family+year
fit<-function(y,rhs,d,fex=fe)feols(as.formula(paste(y,"~",rhs,"|",fex)),d,cluster=CL)
w01<-function(z,ref=z){q<-quantile(ref,c(.01,.99),na.rm=TRUE);pmin(pmax(z,q[1]),q[2])}
wmean<-function(d,wg){mt<-as.matrix(d[,.(log_real_l1,log_real_l2,log_real_l3)]);W<-matrix(wg,nrow(mt),3,byrow=TRUE);rowSums(mt*W,na.rm=TRUE)/rowSums(W*(!is.na(mt)))}
wt<-function(tab,f)fwrite(tab,file.path(T,f))
two<-function(d){d[d[,.I[.N>=2],by=h_pid]$V1]}
M<-two(x[!is.na(cesd_z)]);ME<-two(x[!is.na(esteem_z)]);I<-two(x[!is.na(harm_ide)&!is.na(ide_l1)&ide_l1==0])
M0<-fit("cesd_z","L+G",M);stopifnot(nobs(M0)==148887,abs(coef(M0)["L"]-c0[definition=="real_log_available"&outcome=="cesd"&model=="baseline",estimate_loss])<1e-8)
Mu<-M[obs(M0)];cat("main N",nrow(Mu),"people",uniqueN(Mu$h_pid),"years",uniqueN(Mu$year),"\n")

# Variables needed later
yn<-function(v)fcase(v==1,1,v==2,0,default=NA_real_)
for(D in list(x)){}
x[,`:=`(food1=fcase(h11_3aq2%in%1:2,1,h11_3aq2==3,0,default=NA_real_),food2=fcase(h11_3aq3%in%1:2,1,h11_3aq3==3,0,default=NA_real_),food3=yn(h11_3aq4),food4=yn(h11_3aq6),food5=yn(h11_3aq7))]
fm<-as.matrix(x[,.(food1,food2,food3,food4,food5)]);x[,food:=fifelse(rowSums(!is.na(fm))>0,rowMeans(fm,na.rm=TRUE),NA_real_)]
key<-x[,.(h_pid,year=year+1,food_l=food)];x<-merge(x,key,by=c("h_pid","year"),all.x=TRUE)
cp<-unique(r[,.(year=year+1,cpi_l=cpi)]);x<-merge(x,cp,by="year",all.x=TRUE);setorder(x,h_pid,year)
x[,`:=`(worse_food=fifelse(!is.na(food)&!is.na(food_l),as.numeric(food-food_l>=.2),NA_real_),
 dcons=log(consumption/cpi)-log(consumption_l1/cpi_l),cut16=as.numeric(cesd>=16),L2=L^2,G2=G^2,bigL=as.numeric(raw_gap<=log(.75)),bigG=as.numeric(raw_gap>=log(1.25)),
 ls_z=fifelse(p03_12%in%1:5,as.numeric(p03_12),NA_real_),gap_w=G-L,low_inc=as.numeric(h_hc==2))]
x[,ls_z:=(ls_z-mean(ls_z,na.rm=TRUE))/sd(ls_z,na.rm=TRUE)]
x[,cons_drop:=as.numeric(dcons<=log(.8))]
x[,year_c:=year-mean(year),by=h_pid]
M<-two(x[!is.na(cesd_z)]);ME<-two(x[!is.na(esteem_z)]);I<-two(x[!is.na(harm_ide)&!is.na(ide_l1)&ide_l1==0])
Mu<-M[obs(fit("cesd_z","L+G",M))]
# Table 1
cont<-function(v,l){z<-Mu[[v]];data.table(Variable=l,Mean=num(mean(z,na.rm=TRUE)),SD=num(sd(z,na.rm=TRUE)),P25=num(quantile(z,.25,na.rm=TRUE)),Median=num(median(z,na.rm=TRUE)),P75=num(quantile(z,.75,na.rm=TRUE)))}
Mu[,`:=`(log_cin=log(h_cin/cpi),log_cons=log(100*consumption/cpi),raw_annual_w=Aplus-Aminus)]
t1a<-rbind(cont("cesd","CESD-11 score (0-60)"),cont("cesd_z","CESD-11 z-score"),cont("esteem","Self-esteem score"),cont("esteem_z","Self-esteem z-score"),cont("log_real","Log real disposable income"),cont("benchmark","Recent-income benchmark"),cont("gap_w","Signed benchmark gap, winsorised"),cont("L","Benchmark shortfall"),cont("G","Benchmark gain"),cont("raw_annual_w","Annual change in log real income, winsorised"),cont("Aminus","Annual decline"),cont("Aplus","Annual rise"),cont("log_cons","Log real consumption"),cont("food","Food-hardship index"),cont("age","Age"),cont("h01_1","Household size"))
wt(t1a,"T1a.csv")
bin<-function(z,l){data.table(Variable=l,Proportion=num(mean(z==1,na.rm=TRUE)),`Count = 1`=format(sum(z==1,na.rm=TRUE),big.mark=","),N=format(sum(!is.na(z)),big.mark=","))}
t1b<-rbind(bin(as.numeric(Mu$cesd==0),"CESD-11 score of zero"),bin(Mu$cut16,"CESD-11 score of 16 or more"),bin(Mu$ide,"Suicidal ideation, current"),bin(I$ide,"New ideation report (prior-year report of none)"),
 bin(as.numeric(Mu$raw_annual<=log(.8)),"Annual income decline (20% or more)"),bin(as.numeric(Mu$raw_annual<=log(.75)),"Annual income decline (25% or more)"),bin(as.numeric(Mu$raw_annual>=log(1.2)),"Annual income rise (20% or more)"),bin(as.numeric(Mu$raw_annual>=log(1.25)),"Annual income rise (25% or more)"),
 bin(Mu$cons_drop,"Consumption decline (20% or more)"),bin(as.numeric(Mu$food>0),"Any food hardship"),bin(Mu$worse_food,"Worsening food hardship"),bin(Mu$female,"Female"),bin(Mu$low_inc,"Low-income household"),bin(as.numeric(Mu$n_hist==3),"All three prior income years observed"))
wt(t1b,"T1b.csv")
ids<-data.table(n=nrow(Mu),people=uniqueN(Mu$h_pid),neg_all=r[adult==TRUE&wv%between%c(7,19),sum(h_din<0,na.rm=TRUE)],all=r[adult==TRUE&wv%between%c(7,19),.N],zero=mean(Mu$cesd==0),lag_only=sum(Mu$n_hist<3),med_gap=median(Mu$gap_w),share_short=mean(Mu$L>0),sdL=sd(Mu$L),nI=nrow(I),posI=sum(I$ide==1,na.rm=TRUE))
wt(ids,"facts.csv");print(ids)

# Table 2
m1<-fit("cesd_z","L+G",M);m2<-fit("cut16","L+G",M);m3<-fit("esteem_z","L+G",ME);m4<-fit("harm_ide","L+G",I);m5<-fit("cesd_z","L+L2+G+G2",M);m6<-fit("harm_ide","bigL+bigG",I)
x2<-copy(M);lm_<-as.matrix(x2[,.(log_eq_l1,log_eq_l2,log_eq_l3)]);rq<-x$log_eq-rowMeans(as.matrix(x[,.(log_eq_l1,log_eq_l2,log_eq_l3)]),na.rm=TRUE);x2[,eg:=w01(log_eq-rowMeans(lm_,na.rm=TRUE),rq)];x2[,`:=`(Le=pmax(-eg,0),Ge=pmax(eg,0))];m7<-fit("cesd_z","Le+Ge",x2)
ms<-list(`CESD-11 z-score`=m1,`CESD >= 16`=m2,`Self-esteem z-score`=m3,`New ideation report (pp)`=m4,`CESD quadratic`=m5,`New ideation: large gap (pp)`=m6,`Equivalised income`=m7)
row<-function(l,ts){c(list(Variable=l),setNames(mapply(function(m,t)cf(m,t),ms,ts,SIMPLIFY=FALSE),names(ms)))}
ap<-list(pv(lc(m1,c("L","G"))$p),pv(lc(m2,c("L","G"))$p),pv(lc(m3,c("L","G"))$p),pv(lc(m4,c("L","G"))$p),pv({b<-coef(m5);V<-vcov(m5);R<-rbind(c(1,0,1,0),c(0,1,0,1));q<-R%*%b;f<-drop(t(q)%*%solve(R%*%V%*%t(R),q))/2;pf(f,2,dft(m5),lower.tail=FALSE)}),pv(lc(m6,c("bigL","bigG"))$p),pv(lc(m7,c("Le","Ge"))$p));names(ap)<-names(ms)
t2a<-rbindlist(list(row("Benchmark shortfall",c("L","L","L","L","L","","")),row("Benchmark gain",c("G","G","G","G","G","","")),row("Shortfall squared",c("","","","","L2","","")),row("Gain squared",c("","","","","G2","","")),row("Large shortfall (25% or more)",c("","","","","","bigL","")),row("Large gain (25% or more)",c("","","","","","bigG","")),row("Equivalised shortfall",c("","","","","","","Le")),row("Equivalised gain",c("","","","","","","Ge")),
 c(list(Variable="N"),lapply(ms,function(m)format(nobs(m),big.mark=","))),c(list(Variable="Within R2"),lapply(ms,function(m)num(wr(m),4))),c(list(Variable="Symmetry p-value"),ap)))
wt(t2a,"T2a.csv")
rec<-function(y,d,eta){wg<-exp(-eta*0:2);ref<-x$log_real-wmean(x,wg);z<-copy(d);z[,s:=w01(log_real-wmean(d,wg),ref)];z[,`:=`(Lr=pmax(-s,0),Gr=pmax(s,0))];fit(y,"Lr+Gr",z)}
t2b<-rbindlist(lapply(list(c("cesd_z","CESD-11 z-score"),c("esteem_z","Self-esteem z-score"),c("harm_ide","New ideation report (pp)")),function(o){d<-if(o[1]=="cesd_z")M else if(o[1]=="esteem_z")ME else I
 rbindlist(lapply(c(0,.5,1,2),function(e){m<-rec(o[1],d,e);data.table(Outcome=o[2],`Recency parameter`=num(e,1),Shortfall=num(est(m,"Lr")),Gain=num(est(m,"Gr")),`Symmetry p-value`=pv(lc(m,c("Lr","Gr"))$p))}))}))
wt(t2b,"T2b.csv")
cs<-function(q,l){d<-if(q==1)M else M[abs(gap_w)<=quantile(M$gap_w,1)*0+quantile(abs(M$gap_w),q)];m<-fit("cesd_z","L+G",d);data.table(`Sample restriction`=l,Shortfall=cf(m,"L"),Gain=cf(m,"G"),`Symmetry p-value`=pv(lc(m,c("L","G"))$p),`Shortfall/gain ratio`=num(abs(est(m,"L")/est(m,"G")),2),N=format(nobs(m),big.mark=","))}
t2c<-rbind(cs(1,"Full winsorised sample"),cs(.95,"|gap| at or below 95th percentile"),cs(.90,"|gap| at or below 90th percentile"),cs(.80,"|gap| at or below 80th percentile"));wt(t2c,"T2c.csv")

# Table 3: ladder
rh<-list(`Benchmark terms only`="L+G",`+ annual changes`="Aminus+Aplus+L+G",`+ current income`="current+L+G",`+ annual changes and current income`="Aminus+Aplus+current+L+G")
lad<-function(y,lab,d){rbindlist(lapply(names(rh),function(n){m<-fit(y,rh[[n]],d);data.table(Outcome=lab,Specification=n,Shortfall=cf(m,"L"),Gain=cf(m,"G"),`Symmetry p-value`=pv(lc(m,c("L","G"))$p),`Joint benchmark p-value`=pv(jt(m,c("L","G"))$p),`Within R2`=num(wr(m),5),N=format(nobs(m),big.mark=","))}))}
t3<-rbind(lad("cesd_z","CESD-11 z-score",M),lad("esteem_z","Self-esteem z-score",ME),lad("harm_ide","New ideation report (pp)",I));wt(t3,"T3.csv");print(t3)
lv<-c0[definition%in%c("real_log_complete3","real_level_complete3")&outcome=="cesd"&model%in%c("baseline","combined"),.(definition,model,n,estimate_loss,estimate_gain,p_asymmetry,joint_p)];wt(lv,"T3_sensitivity.csv")
tf<-fread(file.path(OA,"total_fitted_shortfall_contrasts.csv"));wt(tf,"T3_contrasts.csv")

# Table 5: liquidity
liq<-function(lowv,lab,full){d<-M[!is.na(get(lowv))];d[,low:=get(lowv)];base<-"L+G+L:low+G:low"
 if(lowv=="low_lag")base<-paste(base,"+low")
 if(full)base<-paste(base,"+Aminus+Aplus+current+Aminus:low+Aplus:low+current:low")
 m<-fit("cesd_z",base,d);list(m=m,lab=lab)}
L4<-list(liq("low_base","Baseline assets",FALSE),liq("low_lag","Lagged assets",FALSE),liq("low_base","Baseline assets, income controls",TRUE),liq("low_lag","Lagged assets, income controls",TRUE))
pd<-sapply(L4,function(z)lc(z$m,"L:low")$p);q_<-p.adjust(pd,"BH");pa<-sapply(L4,function(z)lc(z$m,c("L:low","G:low"))$p);qa_<-p.adjust(pa,"BH")
t5<-rbindlist(lapply(seq_along(L4),function(i){m<-L4[[i]]$m;d1<-lc(m,c("L","G"));d2<-lc(m,c("L","L:low","G","G:low"));d3<-lc(m,c("L:low","G:low"));lo<-lc(m,c("L","L:low"))
 data.table(Specification=L4[[i]]$lab,Shortfall=cf(m,"L"),`Shortfall x low assets`=cf(m,"L:low"),Gain=cf(m,"G"),`Gain x low assets`=cf(m,"G:low"),`Implied low-asset shortfall`=num(lo$e),`BH q (shortfall interaction)`=pv(q_[i]),`Symmetry p, higher assets`=pv(d1$p),`Symmetry p, low assets`=pv(d2$p),`Asymmetry difference p`=pv(d3$p),`BH q (asymmetry difference)`=pv(qa_[i]),N=format(nobs(m),big.mark=","))}))
wt(t5,"T5.csv");print(t5)
Mq<-M[!is.na(liquid_l1)];Mq[,ter:=cut(liquid_l1,quantile(liquid_l1,0:3/3),include.lowest=TRUE,labels=FALSE)];Mq[,cover:=liquid_l1/(exp(log_real_l1)/12)]
subg<-function(e,l){d<-Mq[eval(e)];m<-fit("cesd_z","L+G",d);data.table(Group=l,Shortfall=cf(m,"L"),Gain=cf(m,"G"),`Symmetry p-value`=pv(lc(m,c("L","G"))$p),N=format(nobs(m),big.mark=","))}
t5b<-rbind(subg(quote(ter==1),"Lowest tercile of lagged liquid assets"),subg(quote(ter==2),"Middle tercile"),subg(quote(ter==3),"Highest tercile"),subg(quote(cover<1),"Lagged assets cover under 1 month of income"),subg(quote(cover>=1&cover<6),"Cover 1-6 months"),subg(quote(cover>=6),"Cover 6 months or more"));wt(t5b,"T5b.csv");print(t5b)

# Appendix
wg<-exp(-.3*0:2)
a1<-function(y,lab,d){dd<-copy(d);ref<-x$log_real-wmean(x,wg);dd[,s:=w01(log_real-wmean(d,wg),ref)];dd[,`:=`(Ld=pmax(-s,0),Gd=pmax(s,0))]
 m0<-fit(y,"L+G",dd);m1<-fit(y,"Ld+Gd",dd);dw<-dd[!is.na(weight)&weight>0];m2<-feols(as.formula(paste(y,"~L+G|",fe)),dw,weights=~weight,cluster=CL);m3<-fit(y,"L+G",dw)
 rbind(data.table(Outcome=lab,Specification="Equal-weight benchmark",Shortfall=cf(m0,"L"),Gain=cf(m0,"G"),`Symmetry p-value`=pv(lc(m0,c("L","G"))$p),N=format(nobs(m0),big.mark=",")),data.table(Outcome=lab,Specification="Discounted benchmark",Shortfall=cf(m1,"Ld"),Gain=cf(m1,"Gd"),`Symmetry p-value`=pv(lc(m1,c("Ld","Gd"))$p),N=format(nobs(m1),big.mark=",")),data.table(Outcome=lab,Specification="Positive-weight sample, unweighted",Shortfall=cf(m3,"L"),Gain=cf(m3,"G"),`Symmetry p-value`=pv(lc(m3,c("L","G"))$p),N=format(nobs(m3),big.mark=",")),data.table(Outcome=lab,Specification="Longitudinal weights",Shortfall=cf(m2,"L"),Gain=cf(m2,"G"),`Symmetry p-value`=pv(lc(m2,c("L","G"))$p),N=format(nobs(m2),big.mark=",")))}
wt(rbind(a1("cesd_z","CESD-11",M),a1("esteem_z","Self-esteem",ME),a1("harm_ide","New ideation report (pp)",I)),"A1.csv")
ar<-M[n_hist==3];lm0<-lm(log_real~log_real_l1+log_real_l2+log_real_l3+factor(year),ar);ar[,res:=w01(residuals(lm0))];ar[,`:=`(Lar=pmax(-res,0),Gar=pmax(res,0))]
a2row<-function(l,m,lt="L",gt="G")data.table(Specification=l,`Shortfall / negative shock`=cf(m,lt),`Gain / positive shock`=cf(m,gt),`Symmetry p-value`=pv(lc(m,c(lt,gt))$p),N=format(nobs(m),big.mark=","))
mh<-fit("cesd_z","L+G+Lar+Gar",ar)
a2<-rbind(a2row("Benchmark terms only",fit("cesd_z","L+G",M)),a2row("Annual-change controls",fit("cesd_z","Aminus+Aplus+L+G",M)),a2row("Annual changes and current income",fit("cesd_z","Aminus+Aplus+current+L+G",M)),a2row("Annual changes and individual trends",fit("cesd_z","Aminus+Aplus+L+G",M,"h_pid+year+h_pid[year_c]")),a2row("AR(3) residual shocks only",fit("cesd_z","Lar+Gar",ar),"Lar","Gar"),a2row("Horse race: benchmark terms",mh),a2row("Horse race: AR(3) residual shocks",mh,"Lar","Gar"))
a2<-rbind(a2,data.table(Specification="Correlation of shortfall with negative AR(3) shock",`Shortfall / negative shock`=num(cor(ar$L,ar$Lar)),`Gain / positive shock`="",`Symmetry p-value`="",N=""),data.table(Specification="Correlation of gain with positive AR(3) shock",`Shortfall / negative shock`="",`Gain / positive shock`=num(cor(ar$G,ar$Gar)),`Symmetry p-value`="",N=""));wt(a2,"A2.csv");print(a2)
ff<-copy(M);ff[,`:=`(sL1=pmin(L,.1),sL2=pmax(L-.1,0),sG1=pmin(G,.1),sG2=pmax(G-.1,0))];ms<-fit("cesd_z","sL1+sL2+sG1+sG2",ff)
un<-copy(M);un[,`:=`(Lu=pmax(-raw_gap,0),Gu=pmax(raw_gap,0))];lvb<-rowMeans(as.matrix(M[,.(l1=exp(log_real_l1),l2=exp(log_real_l2),l3=exp(log_real_l3))]),na.rm=TRUE);ac<-copy(M);ac[,arc:=2*(exp(log_real)-lvb)/(exp(log_real)+lvb)];ac[,`:=`(La=pmax(-arc,0),Ga=pmax(arc,0))]
Ln<-two(as.data.table(P$nominal_calendar_available)[!is.na(harm_cesd)]);Ll<-two(as.data.table(P$real_level_complete3)[!is.na(harm_cesd)]);Lc<-two(as.data.table(P$real_log_complete3)[!is.na(harm_cesd)])
a3r<-function(l,m,lt="L",gt="G")data.table(Specification=l,Shortfall=cf(m,lt),Gain=cf(m,gt),`Symmetry p-value`=pv(lc(m,c(lt,gt))$p),N=format(nobs(m),big.mark=","))
a3<-rbind(a3r("Positive real logs, 1-3 lags (main)",fit("cesd_z","L+G",M)),a3r("Positive real logs, unwinsorised",fit("cesd_z","Lu+Gu",un),"Lu","Gu"),a3r("Arc-percent gap around arithmetic history",fit("cesd_z","La+Ga",ac),"La","Ga"),a3r("Positive real logs, all three lags",fit("harm_cesd","L+G",Lc)),a3r("Nominal log(1+income), clipped at zero",fit("harm_cesd","L+G",Ln)),a3r("Real income levels, all three lags",fit("harm_cesd","L+G",Ll)),a3r("Real income levels with both income controls",fit("harm_cesd","Aminus+Aplus+current+L+G",Ll)),a3r("Spline: gaps up to 0.10",ms,"sL1","sG1"),a3r("Spline: gaps above 0.10",ms,"sL2","sG2"));wt(a3,"A3.csv");print(a3)
sf<-function(v)fcase(v==1,0,v==2,1,v==3,2,v==4,3,default=NA_real_);sr<-function(v)fcase(v==1,3,v==2,2,v==3,1,v==4,0,default=NA_real_)
sb<-copy(M);sb[,`:=`(c1=sf(p05_9),c2=sr(p05_10),c3=sf(p05_11),c4=sf(p05_12),c5=sf(p05_13),c6=sf(p05_14),c7=sr(p05_15),c8=sf(p05_16),c9=sf(p05_17),c10=sf(p05_18),c11=sf(p05_19))]
sb[,`:=`(neg=(c3+c6+c9)/3,posd=(c2+c7)/2,som=(c1+c4+c5+c11)/4,inter=(c8+c10)/2)]
a4<-rbindlist(lapply(list(c("neg","Negative affect"),c("posd","Positive-affect deficit"),c("som","Somatic / effort"),c("inter","Interpersonal")),function(v){sb[,z:=(get(v[1])-mean(get(v[1]),na.rm=TRUE))/sd(get(v[1]),na.rm=TRUE)];m<-fit("z","L+G",sb);data.table(Subscale=v[2],Shortfall=cf(m,"L"),Gain=cf(m,"G"),pp=lc(m,c("L","G"))$p)}));a4[,`:=`(`Symmetry p-value`=pv(pp),`BH q`=pv(p.adjust(pp,"BH")))];a4[,pp:=NULL];wt(a4,"A4.csv")
# floor
gr<-c(1,3,6,9,16,24,33)*20/11;gr<-c(1*20/11,3*20/11,6*20/11,9*20/11,16,13*20/11,18*20/11)
fl<-rbind(data.table(Model="P(CESD = 0)",m=list(fit("zz","L+G",copy(M)[,zz:=as.numeric(cesd==0)])),base=mean(M$cesd==0)),rbindlist(lapply(gr,function(k){d<-copy(M)[,zz:=as.numeric(cesd>=k-1e-9)];data.table(Model=paste0("P(CESD >= ",round(k,1),")"),m=list(fit("zz","L+G",d)),base=mean(d$zz))})))
a6a<-fl[,.(Model,`Base rate`=num(base),Shortfall=sapply(m,cf,"L"),Gain=sapply(m,cf,"G"),`Symmetry p-value`=sapply(m,function(z)pv(lc(z,c("L","G"))$p)))]
tr<-copy(M);tr[,`:=`(ih=asinh(cesd),rk=frank(cesd)/.N)]
ppm<-function(rhs){fepois(as.formula(paste("cesd~",rhs,"|",fe)),M,cluster=CL)};fr<-function(rhs){d<-copy(M)[,fy:=cesd/60];feglm(as.formula(paste("fy~",rhs,"|",fe)),d,family=quasibinomial(),cluster=CL)}
glmr<-function(l,m)data.table(Model=l,`Base rate`="",Shortfall=cf(m,"L"),Gain=cf(m,"G"),`Symmetry p-value`=pv(lc(m,c("L","G"))$p))
fp<-M[order(year),.(c1=cesd[1],y1=year[1]),by=h_pid];rm_<-merge(M,fp,by="h_pid")[year>y1];rm_[,room:=as.numeric(c1>0)]
Mr0<-fit("cesd_z","L+G",rm_[room==0]);Mr1<-fit("cesd_z","L+G",rm_[room==1]);Mri<-fit("cesd_z","L+G+L:room+G:room",rm_);LS<-two(x[!is.na(ls_z)])
a6<-rbind(a6a,glmr("asinh(CESD)",fit("ih","L+G",tr)),glmr("Percentile rank of CESD",fit("rk","L+G",tr)),glmr("Poisson (PPML), CESD score",ppm("L+G")),glmr("PPML with both income controls",ppm("Aminus+Aplus+current+L+G")),glmr("Fractional logit, CESD/60",fr("L+G")),glmr("Fractional logit with both income controls",fr("Aminus+Aplus+current+L+G")),glmr("First observed CESD = 0, later years",Mr0),glmr("First observed CESD > 0, later years",Mr1),glmr("Life satisfaction z-score",fit("ls_z","L+G",LS)),glmr("Life satisfaction with both income controls",fit("ls_z","Aminus+Aplus+current+L+G",LS)))
a6<-rbind(a6,data.table(Model="Asymmetry difference, first CESD > 0 vs = 0",`Base rate`="",Shortfall=num(lc(Mri,c("L:room","G:room"))$e),Gain="",`Symmetry p-value`=pv(lc(Mri,c("L:room","G:room"))$p)));wt(a6,"A6.csv");print(a6)
# kink
kk<-function(y,d,b){z<-d[abs(gap_w)<=b];z[,`:=`(xx=gap_w,sc=pmax(gap_w,0))];m<-fit(y,"xx+sc",z);list(c=cf(m,"sc"),p=pv(coeftable(m)["sc",4]),n=format(nobs(m),big.mark=","))}
a7<-rbindlist(lapply(c(.05,.1,.15,.2,.25,.3,.4,.5),function(b){a<-kk("cesd_z",M,b);e<-kk("esteem_z",ME,b);data.table(Bandwidth=num(b,2),`Slope change: CESD-11`=a$c,`CESD p-value`=a$p,`Slope change: self-esteem`=e$c,`Self-esteem p-value`=e$p,N=a$n)}));wt(a7,"A7.csv");print(a7)
# robustness
comm<-M[n_hist==3];win<-function(d,lags,base=x){ref<-base$log_real-rowMeans(as.matrix(base[,..lags]),na.rm=TRUE);mt<-as.matrix(d[,..lags]);z<-copy(d);z[,s:=w01(log_real-rowMeans(mt,na.rm=TRUE),ref)];z[,`:=`(Lw=pmax(-s,0),Gw=pmax(s,0))];z}
a8r<-function(l,m,lt="L",gt="G")data.table(Specification=l,Shortfall=cf(m,lt),Gain=cf(m,gt),`Symmetry p-value`=pv(lc(m,c(lt,gt))$p),N=format(nobs(m),big.mark=","))
lags<-list(`One-year`="log_real_l1",`Two-year`=c("log_real_l1","log_real_l2"),`Three-year`=c("log_real_l1","log_real_l2","log_real_l3"))
a8<-rbind(rbindlist(lapply(names(lags),function(n)a8r(paste(n,"benchmark, all available"),fit("cesd_z","Lw+Gw",win(M,lags[[n]])),"Lw","Gw"))),rbindlist(lapply(names(lags),function(n)a8r(paste(n,"benchmark, common three-lag sample"),fit("cesd_z","Lw+Gw",win(comm,lags[[n]],x[n_hist==3])),"Lw","Gw"))),a8r("Current log income control",fit("cesd_z","current+L+G",M)),a8r("Lagged outcome control",fit("cesd_z","cesd_z_l1+L+G",M[!is.na(cesd_z_l1)])),a8r("Excluding income years 2020-2021",fit("cesd_z","L+G",M[!year%in%2020:2021])),a8r("Individual linear trends",fit("cesd_z","L+G",M,"h_pid+year+h_pid[year_c]")),a8r("Same household across income dates",fit("cesd_z","L+G",M[stable_household==TRUE])));wt(a8,"A8.csv");print(a8)
# FE/clustering
a9r<-function(l,m)data.table(Specification=l,Shortfall=cf(m,"L"),Gain=cf(m,"G"),`Symmetry p-value`=pv(lc(m,c("L","G"))$p),N=format(nobs(m),big.mark=","))
mb<-function(cl)feols(cesd_z~L+G|h_pid+year,M,cluster=cl)
a9<-rbind(a9r("Individual effects only, household-group and year clusters",feols(cesd_z~L+G|h_pid,M,cluster=CL)),a9r("Individual and year effects, household-group and year clusters (main)",mb(CL)),a9r("Lagged CESD added",fit("cesd_z","cesd_z_l1+L+G",M[!is.na(cesd_z_l1)])),a9r("Individual and year clusters",mb(~h_pid+year)),a9r("Household-group clusters only",feols(cesd_z~L+G|h_pid+year,M,cluster=~family)),a9r("Individual clusters only",mb(~h_pid)));wt(a9,"A9.csv")
# later ideation
fw<-two(x[!is.na(ide_f1)&!is.na(ide)&ide==0&!is.na(cesd_z)]);fw[,idef:=100*ide_f1]
a10<-rbind(data.table(Specification="CESD(t) and new report at t+1",`CESD coefficient (pp)`=cf(fit("idef","cesd_z",fw),"cesd_z"),N=format(nobs(fit("idef","cesd_z",fw)),big.mark=",")),data.table(Specification="Adds benchmark terms and current income",`CESD coefficient (pp)`=cf(fit("idef","cesd_z+L+G+current",fw),"cesd_z"),N=format(nobs(fit("idef","cesd_z+L+G+current",fw)),big.mark=",")),data.table(Specification="Adds lagged CESD",`CESD coefficient (pp)`=cf(fit("idef","cesd_z+cesd_z_l1+L+G+current",fw[!is.na(cesd_z_l1)]),"cesd_z"),N=format(nobs(fit("idef","cesd_z+cesd_z_l1+L+G+current",fw[!is.na(cesd_z_l1)])),big.mark=",")),data.table(Specification="No ideation report in either prior year",`CESD coefficient (pp)`=cf(fit("idef","cesd_z",fw[!is.na(ide_l1)&ide_l1==0]),"cesd_z"),N=format(nobs(fit("idef","cesd_z",fw[!is.na(ide_l1)&ide_l1==0])),big.mark=",")));wt(a10,"A10.csv")
# persistence
nx<-x[,.(h_pid,year=year-1,gap_next=raw_gap)];pr<-merge(M,nx,by=c("h_pid","year"),all.x=TRUE);pr<-pr[!(L>0&is.na(gap_next))];pr[,`:=`(Lrev=fifelse(L>0&!is.na(gap_next)&gap_next>=0,L,0),Lper=fifelse(L>0&(is.na(gap_next)|gap_next<0),L,0))]
peI<-copy(pr);a11o<-function(y,lab,d){a<-fit(y,"Lrev+Lper+G",d);b<-fit(y,"Lrev+Lper+G+Aminus+Aplus+current",d);data.table(Outcome=lab,`Reverting shortfall`=cf(a,"Lrev"),`Persisting shortfall`=cf(a,"Lper"),Gain=cf(a,"G"),`Reverting, both income controls`=cf(b,"Lrev"),`Persisting, both income controls`=cf(b,"Lper"),N=format(nobs(a),big.mark=","))}
a11<-rbind(a11o("cesd_z","CESD-11",pr),a11o("esteem_z","Self-esteem",pr[!is.na(esteem_z)]));wt(a11,"A11.csv")
# hardship
Ch<-M[!is.na(cons_drop)&!is.na(worse_food)];ChE<-ME[!is.na(cons_drop)&!is.na(worse_food)];ChI<-I[!is.na(cons_drop)&!is.na(worse_food)]
a12o<-function(y,lab,d){a<-fit(y,"L+G",d);b<-fit(y,"L+G+cons_drop+worse_food",d);data.table(Outcome=lab,`Shortfall, no hardship terms`=cf(a,"L"),`Shortfall, with hardship terms`=cf(b,"L"),`Gain, with hardship terms`=cf(b,"G"),`Consumption decline (20%)`=cf(b,"cons_drop"),`Worsening food hardship`=cf(b,"worse_food"),N=format(nobs(b),big.mark=","))}
a12<-rbind(a12o("cesd_z","CESD-11",Ch),a12o("esteem_z","Self-esteem",ChE),a12o("harm_ide","New ideation report (pp)",ChI));wt(a12,"A12.csv");print(a12)

# Figure 1 data
fg<-copy(M);br<-seq(-.6,.6,by=.05);fg[,cl:=pmin(pmax(raw_gap,-.6),.6)];fg[,bn:=findInterval(cl,br,rightmost.closed=TRUE,all.inside=TRUE)]
mid<-head(br,-1)+.025;ref<-which.min(abs(mid));fg[,bf:=factor(bn)];fg[,bf:=relevel(bf,as.character(ref))]
mb_<-feols(cesd_z~bf|h_pid+year,fg,cluster=CL);ct<-as.data.table(coeftable(mb_),keep.rownames=TRUE);ct[,bin:=as.integer(sub("bf","",rn))];ct<-ct[,.(bin,est=Estimate,se=`Std. Error`)];ct<-rbind(ct,data.table(bin=ref,est=0,se=0));ct[,mid:=mid[bin]];ct[,pct:=100*(exp(mid)-1)];ct[,n:=fg[,.N,by=bn][match(ct$bin,bn)]$N];fwrite(ct[order(bin)],file.path(O,"figures/fig1_bins.csv"))
fwrite(data.table(L=est(m1,"L"),G=est(m1,"G"),sL=vcov(m1)["L","L"]^.5,sG=vcov(m1)["G","G"]^.5),file.path(O,"figures/fig1_lines.csv"))
cat("tables done\n")

# Event study (Table 4, A5, Figure 2)
d<-as.data.table(readRDS("inputs/audit_input.rds"));stopifnot(min(d$year)>=2011)
ev<-function(y){dd<-d[is.finite(get(y)),.(h_pid=as.numeric(h_pid),year=as.numeric(year),G=as.numeric(g),family,Y=get(y),female_base,lowinc_base,age_base)];setorder(dd,h_pid,year)
 ga<-function(bp,cl,bs)suppressWarnings(att_gt(yname="Y",tname="year",idname="h_pid",gname="G",xformla=~female_base+lowinc_base+age_base,data=as.data.frame(dd),control_group="notyettreated",est_method="dr",allow_unbalanced_panel=TRUE,anticipation=1,base_period=bp,faster_mode=TRUE,bstrap=bs,cband=bs,biters=999,clustervars=cl))
 a<-ga("universal","family",TRUE);es<-suppressWarnings(aggte(a,type="dynamic",min_e=-6,max_e=6,na.rm=TRUE,bstrap=TRUE,cband=TRUE,biters=999,clustervars="family"))
 l<-copy(dd);l[,G2:=fifelse(G==0,Inf,G)];setorder(l,year,G2,h_pid);ord<-unique(l[,.(h_pid,family)]);IF<-as.matrix(es$inf.function$dynamic.inf.func.e);stopifnot(nrow(ord)==nrow(IF))
 V<-crossprod(rowsum(IF,ord$family))/nrow(IF)^2;ee<-es$egt
 tab<-data.table(outcome=y,k=ee,est=es$att.egt,se=sqrt(diag(V)),boot_se=es$se.egt,crit=as.numeric(es$crit.val.egt))
 wal<-function(b,W){b<-as.numeric(b);st<-drop(t(b)%*%solve(W,b));pchisq(st,length(b),lower.tail=FALSE)}
 z<-data.table(g=a$group,t=a$t,att=a$att);z[,k:=t-g];IFc<-as.matrix(a$inffunc);Vc<-crossprod(rowsum(IFc,ord$family))/nrow(IFc)^2;Vp<-crossprod(IFc)/nrow(IFc)^2;ix<-which(z$k< -1&abs(z$att)>0)
 ia<-which(ee%in%c(-4,-3));ib<-which(ee%in%-6:-3)
 v<-ga("varying","h_pid",FALSE);zv<-data.table(g=v$group,t=v$t,att=v$att);zv[,k:=t-g];iv<-which(zv$k< -1);Vv<-as.matrix(crossprod(v$inffunc))/v$n^2
 pt_<-data.table(outcome=y,test=c("Displayed leads k=-4,-3","Displayed leads k=-6 to -3","All clean cells, household covariance","All clean cells, person covariance","All clean cells, varying base, person covariance","Package test including transition year"),p=c(wal(es$att.egt[ia],V[ia,ia]),wal(es$att.egt[ib],V[ib,ib]),wal(z$att[ix],Vc[ix,ix]),wal(z$att[ix],Vp[ix,ix]),wal(zv$att[iv],Vv[iv,iv]),as.numeric(a$Wpval)))
 w<-rep(0,length(ee));w[ee%in%0:3]<-.25;av<-sum(w*es$att.egt);avs<-sqrt(drop(t(w)%*%V%*%w))
 sm<-data.table(outcome=y,avg0_3=av,avg_se=avs,treated=uniqueN(dd[G>0,h_pid]),never=uniqueN(dd[G==0,h_pid]),n=nrow(dd))
 list(tab=tab,pt=pt_,sm=sm)}
E<-list(ev("cesd_z"),ev("esteem_z"))
fwrite(rbindlist(lapply(E,`[[`,"tab")),file.path(T,"event_dynamics.csv"));fwrite(rbindlist(lapply(E,`[[`,"pt")),file.path(T,"event_pretests.csv"));fwrite(rbindlist(lapply(E,`[[`,"sm")),file.path(T,"event_summary.csv"))
print(rbindlist(lapply(E,`[[`,"tab")));print(rbindlist(lapply(E,`[[`,"pt")));print(rbindlist(lapply(E,`[[`,"sm")))
# Figures
ev2<-fread(file.path(T,"event_dynamics.csv"));bins<-fread(file.path(O,"figures/fig1_bins.csv"));ln<-fread(file.path(O,"figures/fig1_lines.csv"))
bins[,side:=fifelse(mid<0,"s","g")];xs<-seq(min(bins$mid),max(bins$mid),length.out=200);lf<-data.table(pct=100*(exp(xs)-1),y=fifelse(xs<0,ln$L*(-xs),ln$G*xs),s=fifelse(xs<0,"a","b"))
th<-theme_classic(base_size=10)+theme(axis.line=element_line(linewidth=.3),axis.ticks=element_line(linewidth=.3),axis.text=element_text(colour="black"),strip.background=element_blank(),strip.text=element_text(face="bold",size=10))
p1<-ggplot(bins,aes(pct,est))+geom_hline(yintercept=0,linetype=2,linewidth=.3,colour="grey45")+geom_vline(xintercept=0,linetype=3,linewidth=.3,colour="grey60")+geom_linerange(aes(ymin=est-1.96*se,ymax=est+1.96*se),linewidth=.3,colour="grey45")+geom_line(data=lf,aes(pct,y,group=s),inherit.aes=FALSE,linewidth=.6)+geom_point(aes(shape=side),size=1.9,stroke=.5,fill="white")+scale_shape_manual(values=c(s=16,g=21),guide="none")+scale_x_continuous(breaks=seq(-40,80,20),labels=function(v)paste0(ifelse(v>0,"+",""),v,"%"))+labs(x="Current income relative to recent benchmark",y="Within-person CES-D difference (SD)")+th
ggsave(file.path(O,"figures/figure1.png"),p1,width=6.27,height=4.07,dpi=300)
ev2[,lab:=fifelse(outcome=="cesd_z","A. CES-D (z)","B. Self-esteem (z)")];ev2[,`:=`(lo=est-1.96*se,hi=est+1.96*se,ulo=est-crit*boot_se,uhi=est+crit*boot_se)]
p2<-ggplot(ev2,aes(k,est))+annotate("rect",xmin=-1.45,xmax=-.55,ymin=-Inf,ymax=Inf,fill="grey90")+geom_hline(yintercept=0,linetype=2,linewidth=.3,colour="grey45")+geom_linerange(aes(ymin=ulo,ymax=uhi),linewidth=.3,colour="grey65",na.rm=TRUE)+geom_linerange(aes(ymin=lo,ymax=hi),linewidth=.7,colour="black",na.rm=TRUE)+geom_line(linewidth=.4)+geom_point(data=ev2[k!=-2],size=1.7)+geom_point(data=ev2[k==-2],shape=21,fill="white",size=1.9)+facet_wrap(~lab)+scale_x_continuous(breaks=-6:6)+labs(x="Income years from first isolated 25% income decline (shaded: transition year)",y="Difference from k = -2 (SD)")+th
ggsave(file.path(O,"figures/figure2.png"),p2,width=6.27,height=4.17,dpi=300)
cat("complete\n")
