args=(commandArgs(TRUE))
ddir <- args[[1]]
if (!(file.exists(paste(ddir, "data/RuleBook.csv",sep="/"))))
{
  stop(paste("No Rule Book Found in ",ddir, "data/RuleBook.csv",
              sep="/"))
}
source("rtestall.r")
rb <- read.csv(paste(ddir, "data/RuleBook.csv",sep="/"),
               stringsAsFactors=FALSE)
ob <- hpr(rb)
if (nrow(ob) > 0)
{  
  nb <- hrs(ob)
} else 
{
  nb <- data.frame()
}

write.table(nb, paste(ddir,"data/RuleConflictCompleteReport.csv",
            sep="/"), sep=",", row.names=FALSE)
if (nrow(ob) > 0)
{
  vb <- hpf(nb)
} else
{
  vb <- data.frame()
}
write.table(vb, paste(ddir,"data/RuleConflictReport.csv",sep="/"), 
            sep=",", row.names=FALSE)
print(paste("Number of Unsatisfied Rules: ", nrow(vb), sep=""))
