library(readxl)
library(RODBC)
library(Hmisc)

#S24AC124 <- read_excel("S24AC124.xlsx")
df<- mdb.get("UP_124/AC_124.mdb", tables=TRUE)
#Control <- read_excel("UP_124/Control.xlsx")

table<- function(df){
  X<- colnames(S24AC124)
  modi_df<- matrix(data = NA, nrow = dim(df)[1], ncol = length(X))
  colnames(modi_df)<- X
  modi_df<- as.data.frame(modi_df)
  modi_df[1:dim(df)[1],c(1:18, 19)]<- df[1:dim(df)[1], c(1:18, 20)]
  for (i in 1:dim(Control)[1]) {
    modi_df$PartNameEn[modi_df$PartNo== Control$PART_NO[i]]<- Control$PART_NAME_EN[i]
    modi_df$PartName[modi_df$PartNo== Control$PART_NO[i]]<- Control$PART_NAME_V1[i]
  }
  return(modi_df)
}

sub_tbl<- function(df, Part, section, gender, age_qtl){
  sub<- df
  R<- quantile(df$AGE)
  if(Part!= "All"){sub <- subset(sub, PartNameEn == Part)}
  if(section!= "All"){sub <- subset(sub, SectionNo == section)}
  if(section!= "All"){sub <- subset(sub, Sex == gender)}
  if(section!= "All"){sub <- subset(sub, Age > R[age_qtl] & Age < R[age_qtl+1])}
  return(sub)
}
  

