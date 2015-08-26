################ rtestall.r ################

		#* ***** pkgTest ***** *#
		# test whether package exists already and install if not
pkgTest <- function(x)
{
  if (!require(x,character.only = TRUE))
  {
    install.packages(x,dep=TRUE,
                     repos='http://cran.stat.ucla.edu/')
      if(!require(x,character.only = TRUE)) stop("Package not found")
  }
}



cat("Clear Data (March 19, 2014) \n")
cat("Copyright Clear Demand 2014 \n\n")

pkgTest("gdata")
pkgTest("gdata")
pkgTest("abind")
pkgTest("gtools")
pkgTest("gtools")
#pkgTest("compare")
#pkgTest("data.table")
pkgTest("stringr")
library(gtools)
#library(compare)
library(data.table)
library(stringr)
options(stringsAsFactors=FALSE)

# unified compilation of cdi common use R functions

		#* ***** xcopy ***** *#
		# use windows X copy directories
		# takes path to whole directory and destination
xcopy <- function(fdir,tdir)
{
  fdir = gsub("/","\\\\", fdir)
  tdir = gsub("/","\\\\", tdir)
  fdir = gsub("/","\\\\", fd)
  tdir = gsub("/","\\\\", td)
  print("Copying")
  print(fdir)
  print("To:")
  print(tdir)
  system(paste("Robocopy /is",fdir,tdir,"/MIR",sep=" "))
}
		#* ***** closestDate ***** *#
		# Find closest date to given "mm/dd/yyyy" date in
		# a list of such dates
closestDate<-function(searchDate,dateList,roundDown=FALSE)
{
  dist2date<-as.Date(dateList)-as.Date(searchDate)
  for(date in dist2date)
  {
    if(date<0)
    {
      index        <- which(dist2date==date)
      dist2date[index] = -1*dist2date[index]
    }
  }
  closest <-which(min(dist2date)==dist2date)
  return(dateList[closest])
}
		#* ***** as.ISOdate ***** *#
		# returns ISOdate from usual "mm/dd/yyyy" string
as.ISOdate <- function (isd)
{
  issd <- strsplit(isd,"/")
  #iso <- ISOdate(sd[3],sd[1],sd[2],0)
#  iso <- NULL
#
#  for (i in 1:length(isd))
#  {
#    sd <- issd[[i]]
#    ist <- ISOdate(sd[3],sd[1],sd[2],0)
#    print(ist)
#    iso <- c(iso,ISOdate(sd[3],sd[1],sd[2],0))
#
#  }
  m  =  sapply(issd[1:length(issd)], function(x) {x[[1]]})
  d  =  sapply(issd[1:length(issd)], function(x) {x[[2]]})
  y  =  sapply(issd[1:length(issd)], function(x) {x[[3]]})
  iso <- ISOdate(y,m,d,0)
  return(iso)
}

		#* ***** df.diff ***** *#
		# df1,df2: data frames, fd: vector of field names
		# returns elements of df1 not in df2
		# helpful to note that names(df) returns the names of
		# fields in df
df.diff <- function(df1, df2, fd)
{
  b = NULL
  for (i in fd)
  {
    a = which(!is.element(df1[[i]],df2[[i]]))
    b = union(b,a)
  }
  dfo <- df1[b,]
  return(dfo)
}
		#* ***** df.same ***** *#
		# df1,df2: data frames, fd: vector of field names
		# returns elements of df1 not in df2
		# helpful to note that names(df) returns the names of
		# fields in df
df.same <- function(df1, df2, fd)
{
  b = 1:length(df1)
  for (i in fd)
  {
    a = which(is.element(df1[[i]],df2[[i]]))
    b = intersect(b,a)
  }
  dfo <- df1[b,]
  return(dfo)
}
		#* ***** ntrim ***** *#
		# returns string without leading or trailing white space
ntrim <- function(is)
{
  ss <- strsplit(is,"")[[1]]
  sp <- which(ss == " ")
  st <- sp[head(which(diff(sp) > 1),1)]+1
  sf <- sp[tail(which(diff(sp) > 1),1)+1]-1
  so <- paste(unlist(ss[st:sf]),collapse="",sep="")
  return(so)
}

dmoney <- function(is)
{
  vo <- as.numeric(gsub("\\$","",is))
  return(vo)
}

hpr <- function(rb)
{
  nol <- names(rb)
  if (is.element("AreaId",nol)){
    AreaId = "AreaId"
} else if (is.element("MarketId",nol)){
    AreaId = "MarketId"
} else if (is.element("LocationNodeId",nol)){
    AreaId = "LocationNodeId"
} else {
    print("No Area/Market Id field!")
    return(data.frame())
}

  if (is.element("RuleRank",nol)){
    RuleRank = "RuleRank"
} else if (is.element("RulePriority",nol)){
    RuleRank = "RulePriority"
} else {
    print("No Area/Market Id field!")
    return(data.frame())
}
		# make monitary fields numeric
  rb$RecPrice   = dmoney(rb$RecPrice)
  rb$CurPrice   = dmoney(rb$CurPrice)
  rb$DecPrice   = dmoney(rb$DecPrice)
  rb$LowerBound = dmoney(rb$LowerBound)
  rb$UpperBound = dmoney(rb$UpperBound)
  rb$UpperBound[which(is.na(rb$UpperBound))] = 10000000.0
  rb$LowerBound[which(is.na(rb$LowerBound))] = 0.0
		# remove meaningless rules from RuleBook
  rb <- rb[which(rb$LowerBound != 0.0 | rb$UpperBound < 100000),]
		# put ProductId and AreaId into one string
  nm  <- paste(rb$ProductId,rb[,AreaId])
  		# find continuous priorityrank
  rr  <- rb$PanelPriority+1/(rb[,RuleRank] + 2)
  rd  <- pmax(rb$RecPrice-rb$UpperBound,0) +
         pmax(rb$LowerBound-rb$RecPrice,0)
 		# sort identifiers by priorityrank and record order
  		# in terms of old indices in i (entries in nm correspond
  		# to indices i in rb)
  nm  <- nm[i<-order(rr,-rd)]
  		# determine last occurence of each identifier in sorted
  		# list (finds highest priority instances)
  ind <- !duplicated(nm,fromLast=TRUE)
  		# select records in rulebook with highest priorityrank
  ob  <- rb[i[ind],]

  return(ob)
}

lsat <- function (rb)
{
  nol <- names(rb)
  if (is.element("AreaId",nol)){
    AreaId = "AreaId"
} else if (is.element("MarketId",nol)){
    AreaId = "MarketId"
} else if (is.element("LocationNodeId",nol)){
    AreaId = "LocationNodeId"
} else {
    print("No Area/Market Id field!")
    break
}

  if (is.element("RuleRank",nol)){
    RuleRank = "RuleRank"
} else if (is.element("RulePriority",nol)){
    RuleRank = "RulePriority"
} else {
    print("No Area/Market Id field!")
    break
}

		# put ProductId and AreaId into one string
  nm  <- paste(rb$ProductId,rb[,AreaId])
  rb$RecPrice   = dmoney(rb$RecPrice)
  rb$CurPrice   = dmoney(rb$CurPrice)
  rb$DecPrice   = dmoney(rb$DecPrice)
  rb$UpperBound = dmoney(rb$UpperBound)
  rb$LowerBound = dmoney(rb$LowerBound)
  rb$UpperBound[which(is.na(rb$UpperBound))] = 10000000.0
  rb$LowerBound[which(is.na(rb$LowerBound))] = 0.0
  		# find continuous priorityrank
  rr   <- rb$PanelPriority+1/(rb[,RuleRank] + 2)
  csf  <- (1:nrow(rb))*0
  csf[which(rb$CurPrice <= rb$UpperBound &
            rb$CurPrice >= rb$LowerBound)] = 1
  csp  <- csf*rr
  rsp  <- rb$RecSat*rr
  cspd <- aggregate(list(csp=csp,rsp=rsp),
           list(ProductId=rb$ProductId,AreaId=rb[,AreaId],jD=rb$jD),
           sum)
  clsd <- cspd$rsp - cspd$csp
  cspd = cbind(cspd,DiffSatWgt=clsd)
  # cspd <- cspd[order(cspd$clsd),]
  cspd <- cspd[order(cspd$ProductId,cspd$AreaId),]
  names(cspd)[4] = "CurSatWgt"
  names(cspd)[5] = "RecSatWgt"
  return(cspd)
}

oldhpr <- function(rb)
{

  ub <- unique(rb[,c("ProductId",AreaId)])
  ub <- paste(ub[,"ProductId"],ub[,AreaId])
  lpa <- 1:length(ub)
  names(lpa) <- ub

#  lp  <- unique(rb$ProductId)
#  la  <- unique(rb[,AreaId])
#  lip <- 1:length(lp)
#  lia <- 1:length(la)
#  names(lip) = as.character(lp)
#  names(lia) = as.character(la)
		# panel of product area
  		# old yopa <- array(0, dim=c(length(lp),length(la)))
		# old yopa <- (1:(length(lp)*length(la)))*0
  yopa <- (1:length(ub))*0
		# rule of product area
  ropa <- (1:length(ub))*0
		# priority of product area
  qopa <- (1:length(ub))*0
		# rule rank of product area
  copa <- (1:length(ub))*0
		# line in RuleBook.csv of product area
  lopa <- (1:length(ub))*0
		# can now reference lp["1"]
  for (i in 1:nrow(rb))
  {
    il = rb[i,]
    ipa = lpa[paste(il$ProductId,il[,AreaId])]
#    ip = lip[as.character(il$ProductId)]
#    ia = lia[as.character(il[,AreaId])]
    iq = il$PanelPriority; ic = il[,RuleRank]
    if (iq > qopa[ipa] || (iq == qopa[ipa] && ic < copa[ipa]))
    {
      yopa[ipa] = il$PanelId
      ropa[ipa] = il$RuleId
      qopa[ipa] = iq
      copa[ipa] = ic
      lopa[ipa] = i
    }
  }
  ob <- rb[lopa,]
}
		# find rules that are not satisfied
hrs <- function(lb)
{


  lrp <- lb$RecPrice
  lup <- lb$UpperBound
  lup[which(is.na(lup))] = 10000000.0
  lsp <- lb$LowerBound
  lsp[which(is.na(lsp))] = 0.0
  ins <- which(lrp > lup | lrp < lsp)
  inn <- which(lrp <= lup & lrp >= lsp)
  return(lb[ins,])
}

hpf <- function(lb)
{
  nol <- names(lb)
  if (is.element("AreaId",nol)){
    AreaId = "AreaId"
} else if (is.element("MarketId",nol)){
    AreaId = "MarketId"
} else if (is.element("LocationNodeId",nol)){
    AreaId = "LocationNodeId"
} else {
    print("No Area/Market Id field!")
    break
}

  if (is.element("RuleRank",nol)){
    RuleRank = "RuleRank"
} else if (is.element("RulePriority",nol)){
    RuleRank = "RulePriority"
} else {
    print("No Area/Market Id field!")
    break
}
  
  hfld <-   c("ProductId",AreaId,"RuleId",RuleRank,"PanelId",
              "PanelPriority","CurPrice","DecPrice","LowerBound",
              "RecPrice","UpperBound","jD", "jB", "iC")
  fields = intersect(hfld,names(lb))

  if (length(fields) < length(hfld)) 
  {
    print("Fields do not completely match!!!")
    difference <- hfld[which(!is.element(hfld,fields))]
    print(paste("                ",difference))
  }

  if (is.element("Case",nol))
    fields = c(fields,"Case")

  ob <- lb[,fields]
  return(ob)
}


lpe <- function(rb)
{
  rb$RecPrice   = dmoney(rb$RecPrice)
  rb$CurPrice   = dmoney(rb$CurPrice)
  rb$LowerCalc  = dmoney(rb$LowerCalc)
  rb$LowerBound = dmoney(rb$LowerBound)
  rb$UpperCalc  = dmoney(rb$UpperCalc)
  rb$UpperBound = dmoney(rb$UpperBound)
  rb$RelatedRecPrice = dmoney(rb$RelatedRecPrice)
  rb$RelatedCurPrice = dmoney(rb$RelatedCurPrice)
  rb$UpperBound[which(rb$UpperBound=="")] = 10000000.0
  rb$LowerBound[which(rb$LowerBound=="")] = 0.0
		# Find PriceChangeFactor
  rv  <- pmax(rb$RecPrice,rb$CurPrice)/pmin(rb$RecPrice,rb$CurPrice)
  		# Find RuleFactor (Multiplier)
  ic <- which(!is.na(rb$RelatedCurPrice))
  sb <- rb$LowerCalc
  nasb <- which(sb == "")
  sb[nasb] = rb$LowerBound[nasb]
  ub <- rb$UpperCalc
  naub <- which(ub == "")
  ub[naub] = rb$UpperBound[naub]
  rp  <- rb$RecPrice;        cp  <- rb$CurPrice
  rrp <- rb$RelatedRecPrice; rcp <- rb$RelatedCurPrice
  bp  <- pmax(sb, pmin(rp, ub, na.rm=TRUE),na.rm=TRUE)
		# RuleMult (Rule ratio between prod and related prod)
  rat <- (1:nrow(rb))*0
  rat <- bp/rrp
  # icn <- setdiff(1:nrow(rb),ic)
  # rat[icn] <- NA

  crat <- (1:nrow(rb))*0
  # ucrat <- pmax(rb$CurPrice[ic],rb$RelatedCurPrice[ic])
  # scrat <- pmin(rb$CurPrice[ic],rb$RelatedCurPrice[ic])
  ucrat <- cp[ic]
  scrat <- rcp[ic]
  crat[ic] <- ucrat/scrat	# crato
  icn <- setdiff(1:nrow(rb),ic)
  crat[icn] <- NA
  		# CurRatio (Ratio of Cur to RelCur)
  crato <- crat
  icnn  <- which(crat < 1)
  		# CurFactor (Factor of Cur to RelCur)
  crat[icnn] = -1/crat[icnn]
  cratp <- crato*rat
  		# RulePriceMult (Product of CurRatio*RuleMult)
  cratr <- cratp
  icnr  <- which(cratp < 1)
  		# RulePriceFactor (Factor of CurRatio*RuleMult)
  cratr[icnr] <- 1/cratr[icnr]
		# add new fields to RB
  rb  <- cbind(rb,CurFactor=crat, CurRatio=crato,
               RuleMult=rat, RulePriceMult = cratp,
               RulePriceFactor=cratr, PriceChangeFactor=rv)

  ico <- which(rb$PriceChangeFactor > ChangeTol)
  ob  <- rb[ico,]

  return (ob)
  if (FALSE)
  {
		# order by price change factor
    rb  <- hprb[order(-hprb$PriceChangeFactor),]
    index <- which(rb$PriceChangeFactor > 20)
    print(paste("Price Changes of factor greater than 20:",length(index)))
    rbo <- hprb[index,]
    write.csv(rbo, "LargePriceChangeBook.csv")
    write.csv(rb,       "PriceChangeBook.csv")
  }
}
