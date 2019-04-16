library(readxl)
library(dplyr)
Qnaire <- read_excel("Qnaire.xlsx",sheet = "Qnaire")
Qnaire <- data.frame(Qnaire)
row_group<-lapply(seq(1,87,2),function(x) c(x,x+1))


#  For loop version
final_score<-data.frame()
for(r in row_group){
  group_Q_equal<-c()
  for(c in 2:13){
    if(identical(Qnaire[r[1],c],Qnaire[r[2],c])){
      group_Q_equal<-c(group_Q_equal,1)
    }else{
      group_Q_equal<-c(group_Q_equal,0)
    }
  }
  final_score<-rbind(final_score,group_Q_equal)
}

colnames(final_score)<-colnames(Qnaire)[2:13]
confidence<-colSums(final_score)

# Apply version
my_fun<-function(coll){
  sapply(row_group,function(r) identical(coll[r[1]],coll[r[2]])) %>% sum
}
Rlbty <- apply(Qnaire,2,my_fun)

List <- apply(Qnaire,2,my_fun)

TotalRlblty <- List %>% sum * 2 / 1056
FstRlblty <- List[c(2,3,4,5)] %>% sum * 2 / 352
SndRlblty <- List[6] %>% sum * 2 / 88
ThrdRlblty <- List[c(7,8,9,10,11,12,13)] %>% sum * 2 / 616
