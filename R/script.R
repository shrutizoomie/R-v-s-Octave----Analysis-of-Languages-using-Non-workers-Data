library(ggplot2)
library(reshape2)
setwd("C:\\Users\\Suraj\\Desktop") #Script Path
Non_Workers <- read.csv(file="Disabled-Non-Workers.csv",head=TRUE,sep=",")
#print(names(Non_Workers));
choice <- ""

print("a.Analysis based on Grand Total b.Analysis based on Gender c.Analysis based on Disability
      d.Analysis based on Rural Area e.Analysis based on Urban Area z.summary and exit\n"); 
print("Enter your choice\n");
choice <-  scan(what=character(),nmax=1)
flag <- 0
print("Enter the Area Name\n");
area_name <-  scan(what=character(),nmax=1)
area_name <-  toupper(area_name)
dflist <- c("INDIA","ANDAMAN", "ANDHRA", "JK", "HP", "PUNJAB", "CHANDIGARH", "UTTARAKHAND",
            "HARYANA", "DELHI", "RAJASTHAN", "UP", "BIHAR", "SIKKIM", "ARUNACHAL", "NAGALAND",
            "MANIPUR", "MIZORAM", "TRIPURA", "MEGHALAYA", "ASSAM", "WB", "JHARKHAND", "ODISHA", 
            "CHHATTISGARH", "MP", "GUJARAT", "DIU", "DADRA", "MAHARASHTRA", "KARNATAKA", "GOA",
            "LAKSHADWEEP", "KERALA", "TN", "PUDUCHERRY")
for (i in dflist) {
  if(area_name == i){
    flag = 1;
    break;
  }   
}

if(flag == 0){
  print("Invalid area name, please execute again\n");
  print("Area Name should be any one displayed\n");
  print(dflist)
  quit("default", 0, TRUE)
}
labels <- c("Student", "Household duties", "Dependent", "Pensioner", "Rentier", "Beggar  Vagrants etc.", "Others");

if(choice == 'a') {
  newdata <- subset(Non_Workers, Area.Name==area_name & Total..Rural..Urban == "Total" & Activity.of.Non.worker != "Total", select=Activity.of.Non.worker:Total.disabled.non.worker...Persons)

  newdata_rural <- subset(Non_Workers, Area.Name==area_name & Total..Rural..Urban == "Rural" & Activity.of.Non.worker != "Total", select=Activity.of.Non.worker:Total.disabled.non.worker...Persons)
  
  newdata_urban <- subset(Non_Workers, Area.Name==area_name & Total..Rural..Urban == "Urban" & Activity.of.Non.worker != "Total", select=Activity.of.Non.worker:Total.disabled.non.worker...Persons)
  
  #code for bar chart - 7 types
  names(newdata)[names(newdata)=="Total.disabled.non.worker...Persons"] <- "two"
  bp<-barplot(newdata$two, las=2, names.arg= newdata$Activity.of.Non.worker, main=paste('Activity of Non-Worker in', area_name), axes = FALSE, col="Green")
  options("scipen"=100)
  par(mfrow=c(1,1))
  usr <- par("usr")
  par(usr=c(usr[1:2], 0, 800000))
  axis(side = 2, at = seq(0, 800000,100000)) 
  text(bp, 0, round(newdata$two, 1),cex=0.6,pos=3)

  # Grouped Bar Plot - Rural/Urban
  combined <- matrix(c(newdata_rural$Total.disabled.non.worker...Persons[1:1],newdata_rural$Total.disabled.non.worker...Persons[2:2],newdata_rural$Total.disabled.non.worker...Persons[3:3],newdata_rural$Total.disabled.non.worker...Persons[4:4],newdata_rural$Total.disabled.non.worker...Persons[5:5],newdata_rural$Total.disabled.non.worker...Persons[6:6],newdata_rural$Total.disabled.non.worker...Persons[7:7],newdata_urban$Total.disabled.non.worker...Persons[1:1],newdata_urban$Total.disabled.non.worker...Persons[2:2],newdata_urban$Total.disabled.non.worker...Persons[3:3],newdata_urban$Total.disabled.non.worker...Persons[4:4],newdata_urban$Total.disabled.non.worker...Persons[5:5],newdata_urban$Total.disabled.non.worker...Persons[6:6],newdata_urban$Total.disabled.non.worker...Persons[7:7]),ncol=2)
  #combined <- table(newdata_rural$Total.disabled.non.worker...Persons,newdata_urban$Total.disabled.non.worker...Persons)
  bp1 <- barplot(t(combined), las=2,
        col=c("darkblue","red"),
        beside=TRUE, axes=FALSE, names.arg=labels, main=paste('Activity of Non-Worker based on Rural/Urban in', area_name))
  options("scipen"=100)
  axis(side = 2, at = seq(0, 6000000,1000000)) 
  text(bp1, 0, round(t(combined), 1),cex=0.7,pos=3)

  #Code for pie chart - 7 types
  slices <- c(newdata$two) 
  lbls <- c("Student", "Household duties", "Dependent", "Pensioner", "Rentier", "Beggar  Vagrants etc.", "Others")
  pct <- round(slices/sum(slices)*100, digits = 2)
  lbls <- paste(lbls, pct) # add percents to labels 
  lbls <- paste(lbls,"%",sep="") # ad % to labels 
  pie(slices,labels = lbls, col=rainbow(length(lbls)),
    main=paste('Activity of Non-Worker in', area_name))

} else if(choice == 'b') { 
  
  newdata_gen <- subset(Non_Workers, Area.Name==area_name & Total..Rural..Urban == "Total" & Activity.of.Non.worker != "Total", select=Total.disabled.non.worker...Male:Total.disabled.non.worker...Female)
  
  names(newdata_gen)[names(newdata_gen)=="Total.disabled.non.worker...Male"] <- "male"
  names(newdata_gen)[names(newdata_gen)=="Total.disabled.non.worker...Female"] <- "Female"
  names(newdata_gen)[names(newdata_gen)=="Activity.of.Non.worker"] <- "type"

  disabled_gen <- matrix(c(newdata_gen$male[1:1],newdata_gen$Female[1:1],newdata_gen$male[2:2],newdata_gen$Female[2:2],newdata_gen$male[3:3],newdata_gen$Female[3:3],newdata_gen$male[4:4],newdata_gen$Female[4:4],newdata_gen$male[5:5],newdata_gen$Female[5:5],newdata_gen$male[6:6],newdata_gen$Female[6:6],newdata_gen$male[7:7],newdata_gen$Female[7:7]),ncol=7,byrow=FALSE)
 
  disabled_gen <- as.table(disabled_gen)

  xx <- barplot(as.matrix(disabled_gen), las=2, main=paste('Disabled Non-Workers Based on Gender in', area_name), names.arg=labels,
        col=c("peachpuff","cadetblue1"), axes = FALSE, legend = c("Male", "Female"),
        args.legend = list(title = "Gender", x = "topright", cex = .7))
  options("scipen"=100)
  par(mfrow=c(1,1))
  usr <- par("usr")
  par(usr=c(usr[1:2], 0, 800000))
  axis(side = 2, at = seq(0, 800000,400000)) 
  
  newdata_gen_rural <- subset(Non_Workers, Area.Name==area_name & Total..Rural..Urban == "Rural" & Activity.of.Non.worker != "Total", select=Total.disabled.non.worker...Male:Total.disabled.non.worker...Female)
  newdata_gen_urban <- subset(Non_Workers, Area.Name==area_name & Total..Rural..Urban == "Urban" & Activity.of.Non.worker != "Total", select=Total.disabled.non.worker...Male:Total.disabled.non.worker...Female)
  
  names(newdata_gen_rural)[names(newdata_gen_rural)=="Total.disabled.non.worker...Male"] <- "male"
  names(newdata_gen_rural)[names(newdata_gen_rural)=="Total.disabled.non.worker...Female"] <- "Female"
  names(newdata_gen_rural)[names(newdata_gen_rural)=="Activity.of.Non.worker"] <- "type"
  
  names(newdata_gen_urban)[names(newdata_gen_urban)=="Total.disabled.non.worker...Male"] <- "male"
  names(newdata_gen_urban)[names(newdata_gen_urban)=="Total.disabled.non.worker...Female"] <- "Female"
  names(newdata_gen_urban)[names(newdata_gen_urban)=="Activity.of.Non.worker"] <- "type"
  
  combined_gen_rural <- matrix(c(newdata_gen_rural$male[1:1],newdata_gen_rural$male[2:2],newdata_gen_rural$male[3:3],newdata_gen_rural$male[4:4],newdata_gen_rural$male[5:5],newdata_gen_rural$male[6:6],newdata_gen_rural$male[7:7],newdata_gen_rural$Female[1:1],newdata_gen_rural$Female[2:2],newdata_gen_rural$Female[3:3],newdata_gen_rural$Female[4:4],newdata_gen_rural$Female[5:5],newdata_gen_rural$Female[6:6],newdata_gen_rural$Female[7:7]),ncol=2)
  combined_gen_urban <- matrix(c(newdata_gen_urban$male[1:1],newdata_gen_urban$male[2:2],newdata_gen_urban$male[3:3],newdata_gen_urban$male[4:4],newdata_gen_urban$male[5:5],newdata_gen_urban$male[6:6],newdata_gen_urban$male[7:7],newdata_gen_urban$Female[1:1],newdata_gen_urban$Female[2:2],newdata_gen_urban$Female[3:3],newdata_gen_urban$Female[4:4],newdata_gen_urban$Female[5:5],newdata_gen_urban$Female[6:6],newdata_gen_urban$Female[7:7]),ncol=2)

  Student <- c(newdata_gen_rural$male[1:1],newdata_gen_rural$Female[1:1],newdata_gen_urban$male[1:1],newdata_gen_urban$Female[1:1])
  Household_duties <- c(newdata_gen_rural$male[2:2],newdata_gen_rural$Female[2:2],newdata_gen_urban$male[2:2],newdata_gen_urban$Female[2:2])
  Dependent <- c(newdata_gen_rural$male[3:3],newdata_gen_rural$Female[3:3],newdata_gen_urban$male[3:3],newdata_gen_urban$Female[3:3])
  Pensioner <- c(newdata_gen_rural$male[4:4],newdata_gen_rural$Female[4:4],newdata_gen_urban$male[4:4],newdata_gen_urban$Female[4:4])
  Rentier <- c(newdata_gen_rural$male[5:5],newdata_gen_rural$Female[5:5],newdata_gen_urban$male[5:5],newdata_gen_urban$Female[5:5])
  Beggar_Vagrants <- c(newdata_gen_rural$male[6:6],newdata_gen_rural$Female[6:6],newdata_gen_urban$male[6:6],newdata_gen_urban$Female[6:6])
  Others <- c(newdata_gen_rural$male[7:7],newdata_gen_rural$Female[7:7],newdata_gen_urban$male[7:7],newdata_gen_urban$Female[7:7])

  dat <- data.frame(Student, Household_duties, Dependent, Pensioner, Rentier, Beggar_Vagrants, Others)
  dat$Labels <- factor(c("Male-Rural", "Female-Rural", "Male-Urban", "Female-Urban"), 
                  levels=c("Male-Rural", "Female-Rural", "Male-Urban", "Female-Urban"))
 

  mdat <- melt(dat, id.vars="Labels")
  head(mdat)
  ggplot(mdat, aes(variable, value, fill=Labels)) + 
  geom_bar(stat="identity", position="dodge") + 
  theme(panel.background = element_rect(colour = "pink", fill = "peachpuff")) +
  ggtitle(paste('Activity of Non-Worker based on Gender in Rural/Urban of', area_name))

} else if(choice == 'c') {
  
  newdata_types_total <- subset(Non_Workers, Area.Name==area_name & Total..Rural..Urban == "Total" & Activity.of.Non.worker == "Total", 
                                select=Type.of.disability...In.seeing...Persons:Type.of.disability...In.hearing...Persons:Type.of.disability...In.speech...Persons:Type.of.disability...In.movement...Persons:Type.of.disability...Mental.Retardation...Persons:Type.of.disability...Mental.Illness...Persons:Type.of.disability...Any.other...Persons:Type.of.disability...Multiple.disability...Persons)
  slices <- c(newdata_types_total$Type.of.disability...In.seeing...Persons, newdata_types_total$Type.of.disability...In.hearing...Persons, newdata_types_total$Type.of.disability...In.speech...Persons, newdata_types_total$Type.of.disability...In.movement...Persons, newdata_types_total$Type.of.disability...Mental.Retardation...Persons, newdata_types_total$Type.of.disability...Mental.Illness...Persons, newdata_types_total$Type.of.disability...Any.other...Persons, newdata_types_total$Type.of.disability...Multiple.disability...Persons) 
  lbls <- c("seeing", "hearing", "speech", "movement", "retardation", "illness", "other", "disability")
  pct <- round(slices/sum(slices)*100, digits = 2)
  lbls <- paste(lbls, pct) # add percents to labels 
  lbls <- paste(lbls,"%",sep="") # ad % to labels 
  pie(slices,labels = lbls, col=rainbow(length(lbls)),
      main=paste('Type of Disability in', area_name))
  
  
  newdata_types <- subset(Non_Workers, Area.Name==area_name & Total..Rural..Urban == "Total" & Activity.of.Non.worker != "Total", 
                          select=Type.of.disability...In.seeing...Persons:Type.of.disability...In.hearing...Persons:Type.of.disability...In.speech...Persons:Type.of.disability...In.movement...Persons:Type.of.disability...Mental.Retardation...Persons:Type.of.disability...Mental.Illness...Persons:Type.of.disability...Any.other...Persons:Type.of.disability...Multiple.disability...Persons)

  Student <- c(newdata_types$Type.of.disability...In.seeing...Persons[1:1], newdata_types$Type.of.disability...In.hearing...Persons[1:1],
                     newdata_types$Type.of.disability...In.speech...Persons[1:1], newdata_types$Type.of.disability...In.movement...Persons[1:1],
                     newdata_types$Type.of.disability...Mental.Retardation...Persons[1:1], newdata_types$Type.of.disability...Mental.Illness...Persons[1:1],
                     newdata_types$Type.of.disability...Any.other...Persons[1:1], newdata_types$Type.of.disability...Multiple.disability...Persons[1:1])
  
  Household_duties <- c(newdata_types$Type.of.disability...In.seeing...Persons[2:2], newdata_types$Type.of.disability...In.hearing...Persons[2:2],
                     newdata_types$Type.of.disability...In.speech...Persons[2:2], newdata_types$Type.of.disability...In.movement...Persons[2:2],
                     newdata_types$Type.of.disability...Mental.Retardation...Persons[2:2], newdata_types$Type.of.disability...Mental.Illness...Persons[2:2],
                     newdata_types$Type.of.disability...Any.other...Persons[2:2], newdata_types$Type.of.disability...Multiple.disability...Persons[2:2])
  
  Dependent <- c(newdata_types$Type.of.disability...In.seeing...Persons[3:3], newdata_types$Type.of.disability...In.hearing...Persons[3:3],
                              newdata_types$Type.of.disability...In.speech...Persons[3:3], newdata_types$Type.of.disability...In.movement...Persons[3:3],
                              newdata_types$Type.of.disability...Mental.Retardation...Persons[3:3], newdata_types$Type.of.disability...Mental.Illness...Persons[3:3],
                              newdata_types$Type.of.disability...Any.other...Persons[3:3], newdata_types$Type.of.disability...Multiple.disability...Persons[3:3])
  
  Pensioner <- c(newdata_types$Type.of.disability...In.seeing...Persons[4:4], newdata_types$Type.of.disability...In.hearing...Persons[4:4],
                       newdata_types$Type.of.disability...In.speech...Persons[4:4], newdata_types$Type.of.disability...In.movement...Persons[4:4],
                       newdata_types$Type.of.disability...Mental.Retardation...Persons[4:4], newdata_types$Type.of.disability...Mental.Illness...Persons[4:4],
                       newdata_types$Type.of.disability...Any.other...Persons[4:4], newdata_types$Type.of.disability...Multiple.disability...Persons[4:4])
  
  Rentier <- c(newdata_types$Type.of.disability...In.seeing...Persons[5:5], newdata_types$Type.of.disability...In.hearing...Persons[5:5],
                       newdata_types$Type.of.disability...In.speech...Persons[5:5], newdata_types$Type.of.disability...In.movement...Persons[5:5],
                       newdata_types$Type.of.disability...Mental.Retardation...Persons[5:5], newdata_types$Type.of.disability...Mental.Illness...Persons[5:5],
                       newdata_types$Type.of.disability...Any.other...Persons[5:5], newdata_types$Type.of.disability...Multiple.disability...Persons[5:5])
  
  Beggar_Vagrants <- c(newdata_types$Type.of.disability...In.seeing...Persons[6:6], newdata_types$Type.of.disability...In.hearing...Persons[6:6],
                     newdata_types$Type.of.disability...In.speech...Persons[6:6], newdata_types$Type.of.disability...In.movement...Persons[6:6],
                     newdata_types$Type.of.disability...Mental.Retardation...Persons[6:6], newdata_types$Type.of.disability...Mental.Illness...Persons[6:6],
                     newdata_types$Type.of.disability...Any.other...Persons[6:6], newdata_types$Type.of.disability...Multiple.disability...Persons[6:6])
  
  Others <- c(newdata_types$Type.of.disability...In.seeing...Persons[7:7], newdata_types$Type.of.disability...In.hearing...Persons[7:7],
                             newdata_types$Type.of.disability...In.speech...Persons[7:7], newdata_types$Type.of.disability...In.movement...Persons[7:7],
                             newdata_types$Type.of.disability...Mental.Retardation...Persons[7:7], newdata_types$Type.of.disability...Mental.Illness...Persons[7:7],
                             newdata_types$Type.of.disability...Any.other...Persons[7:7], newdata_types$Type.of.disability...Multiple.disability...Persons[7:7])
  
  seeing <- c(newdata_types$Type.of.disability...In.seeing...Persons[1:1],newdata_types$Type.of.disability...In.seeing...Persons[2:2], newdata_types$Type.of.disability...In.seeing...Persons[3:3], newdata_types$Type.of.disability...In.seeing...Persons[4:4], newdata_types$Type.of.disability...In.seeing...Persons[5:5], newdata_types$Type.of.disability...In.seeing...Persons[6:6], newdata_types$Type.of.disability...In.seeing...Persons[7:7])
  hearing <- c(newdata_types$Type.of.disability...In.hearing...Persons[1:1],newdata_types$Type.of.disability...In.hearing...Persons[2:2], newdata_types$Type.of.disability...In.hearing...Persons[3:3], newdata_types$Type.of.disability...In.hearing...Persons[4:4], newdata_types$Type.of.disability...In.hearing...Persons[5:5], newdata_types$Type.of.disability...In.hearing...Persons[6:6], newdata_types$Type.of.disability...In.hearing...Persons[7:7])
  speech <- c(newdata_types$Type.of.disability...In.speech...Persons[1:1],newdata_types$Type.of.disability...In.speech...Persons[2:2], newdata_types$Type.of.disability...In.speech...Persons[3:3], newdata_types$Type.of.disability...In.speech...Persons[4:4], newdata_types$Type.of.disability...In.speech...Persons[5:5], newdata_types$Type.of.disability...In.speech...Persons[6:6], newdata_types$Type.of.disability...In.speech...Persons[7:7])
  movement <- c(newdata_types$Type.of.disability...In.movement...Persons[1:1],newdata_types$Type.of.disability...In.movement...Persons[2:2], newdata_types$Type.of.disability...In.movement...Persons[3:3], newdata_types$Type.of.disability...In.movement...Persons[4:4], newdata_types$Type.of.disability...In.movement...Persons[5:5], newdata_types$Type.of.disability...In.movement...Persons[6:6], newdata_types$Type.of.disability...In.movement...Persons[7:7])
  retardation <- c(newdata_types$Type.of.disability...Mental.Retardation...Persons[1:1],newdata_types$Type.of.disability...Mental.Retardation...Persons[2:2], newdata_types$Type.of.disability...Mental.Retardation...Persons[3:3], newdata_types$Type.of.disability...Mental.Retardation...Persons[4:4], newdata_types$Type.of.disability...Mental.Retardation...Persons[5:5], newdata_types$Type.of.disability...Mental.Retardation...Persons[6:6], newdata_types$Type.of.disability...Mental.Retardation...Persons[7:7])
  illness <- c(newdata_types$Type.of.disability...Mental.Illness...Persons[1:1],newdata_types$Type.of.disability...Mental.Illness...Persons[2:2], newdata_types$Type.of.disability...Mental.Illness...Persons[3:3], newdata_types$Type.of.disability...Mental.Illness...Persons[4:4], newdata_types$Type.of.disability...Mental.Illness...Persons[5:5], newdata_types$Type.of.disability...Mental.Illness...Persons[6:6], newdata_types$Type.of.disability...Mental.Illness...Persons[7:7])
  other <- c(newdata_types$Type.of.disability...Any.other...Persons[1:1],newdata_types$Type.of.disability...Any.other...Persons[2:2], newdata_types$Type.of.disability...Any.other...Persons[3:3], newdata_types$Type.of.disability...Any.other...Persons[4:4], newdata_types$Type.of.disability...Any.other...Persons[5:5], newdata_types$Type.of.disability...Any.other...Persons[6:6], newdata_types$Type.of.disability...Any.other...Persons[7:7])
  disability <- c(newdata_types$Type.of.disability...Multiple.disability...Persons[1:1],newdata_types$Type.of.disability...Multiple.disability...Persons[2:2], newdata_types$Type.of.disability...Multiple.disability...Persons[3:3], newdata_types$Type.of.disability...Multiple.disability...Persons[4:4], newdata_types$Type.of.disability...Multiple.disability...Persons[5:5], newdata_types$Type.of.disability...Multiple.disability...Persons[6:6], newdata_types$Type.of.disability...Multiple.disability...Persons[7:7])

  dat <- data.frame(Student, Household_duties, Dependent, Pensioner, Rentier, Beggar_Vagrants, Others)
  dat$Labels <- factor(c("seeing", "hearing", "speech", "movement", "retardation", "illness", "other", "disability"), 
                     levels=c("seeing", "hearing", "speech", "movement", "retardation", "illness", "other", "disability"))


  mdat <- melt(dat, id.vars="Labels")
  head(mdat)
  ggplot(mdat, aes(variable, value, fill=Labels)) + 
  geom_bar(stat="identity", position="dodge") + 
  theme(panel.background = element_rect(colour = "pink", fill = "peachpuff")) +
  ggtitle(paste('Activity of Non-Worker based on Disability in', area_name))
  
} else if(choice == 'd') {

  newdata_rural_urban <- subset(Non_Workers, Area.Name==area_name & Total..Rural..Urban != "Total" & Activity.of.Non.worker != "Total", 
                                select=Type.of.disability...In.seeing...Persons:Type.of.disability...In.hearing...Persons:Type.of.disability...In.speech...Persons:Type.of.disability...In.movement...Persons:Type.of.disability...Mental.Retardation...Persons:Type.of.disability...Mental.Illness...Persons:Type.of.disability...Any.other...Persons:Type.of.disability...Multiple.disability...Persons)
  names(newdata_rural_urban)[names(newdata_rural_urban)=="Type.of.disability...In.seeing...Persons"] <- "dis_seeing"
  names(newdata_rural_urban)[names(newdata_rural_urban)=="Type.of.disability...In.hearing...Persons"] <- "dis_hearing"
  names(newdata_rural_urban)[names(newdata_rural_urban)=="Type.of.disability...In.speech...Persons"] <- "dis_speech"
  names(newdata_rural_urban)[names(newdata_rural_urban)=="Type.of.disability...In.movement...Persons"] <- "dis_movement"
  names(newdata_rural_urban)[names(newdata_rural_urban)=="Type.of.disability...Mental.Retardation...Persons"] <- "dis_retardation"
  names(newdata_rural_urban)[names(newdata_rural_urban)=="Type.of.disability...Mental.Illness...Persons"] <- "dis_illness"
  names(newdata_rural_urban)[names(newdata_rural_urban)=="Type.of.disability...Any.other...Persons"] <- "dis_other"
  names(newdata_rural_urban)[names(newdata_rural_urban)=="Type.of.disability...Multiple.disability...Persons"] <- "dis_multipledisability"
  
  #Groups      <- c(rep(c("Student", "Household duties", "Dependent", "Pensioner", "Rentier", "Beggar  Vagrants etc.", "Others"))
  Groups      <- c(rep(c("Student", "Household duties","Dependent", "Pensioner", "Rentier", "Beggar  Vagrants etc.", "Others"), each = 8))
  Category  <- c(rep(c("seeing", "hearing", "speech","movement", "retardation", "illness", "other", "multipledisability"), times = 7))
  
  #Analysis of all 8 types of disability in Rural Region
  Frequency_Rural <- c(newdata_rural_urban$dis_seeing[1:1],newdata_rural_urban$dis_hearing[1:1],
                 newdata_rural_urban$dis_speech[1:1],newdata_rural_urban$dis_movement[1:1],
                 newdata_rural_urban$dis_retardation[1:1], newdata_rural_urban$dis_illness[1:1],
                 newdata_rural_urban$dis_other[1:1],newdata_rural_urban$dis_multipledisability[1:1],
                 newdata_rural_urban$dis_seeing[2:2],newdata_rural_urban$dis_hearing[2:2],newdata_rural_urban$dis_speech[2:2],newdata_rural_urban$dis_movement[2:2],
                 newdata_rural_urban$dis_retardation[2:2],newdata_rural_urban$dis_illness[2:2],newdata_rural_urban$dis_other[2:2],newdata_rural_urban$dis_multipledisability[2:2],
                 newdata_rural_urban$dis_seeing[3:3],newdata_rural_urban$dis_hearing[3:3],newdata_rural_urban$dis_speech[3:3],newdata_rural_urban$dis_movement[3:3],
                 newdata_rural_urban$dis_retardation[3:3],newdata_rural_urban$dis_illness[3:3],newdata_rural_urban$dis_other[3:3],newdata_rural_urban$dis_multipledisability[3:3],
                 newdata_rural_urban$dis_seeing[4:4],newdata_rural_urban$dis_hearing[4:4],newdata_rural_urban$dis_speech[4:4],newdata_rural_urban$dis_movement[4:4],
                 newdata_rural_urban$dis_retardation[4:4],newdata_rural_urban$dis_illness[4:4],newdata_rural_urban$dis_other[4:4],newdata_rural_urban$dis_multipledisability[4:4],
                 newdata_rural_urban$dis_seeing[5:5],newdata_rural_urban$dis_hearing[5:5],newdata_rural_urban$dis_speech[5:5],newdata_rural_urban$dis_movement[5:5],
                 newdata_rural_urban$dis_retardation[5:5],newdata_rural_urban$dis_illness[5:5],newdata_rural_urban$dis_other[5:5],newdata_rural_urban$dis_multipledisability[5:5],
                 newdata_rural_urban$dis_seeing[6:6],newdata_rural_urban$dis_hearing[6:6],newdata_rural_urban$dis_speech[6:6],newdata_rural_urban$dis_movement[6:6],
                 newdata_rural_urban$dis_retardation[6:6],newdata_rural_urban$dis_illness[6:6],newdata_rural_urban$dis_other[6:6],newdata_rural_urban$dis_multipledisability[6:6],
                 newdata_rural_urban$dis_seeing[7:7],newdata_rural_urban$dis_hearing[7:7],newdata_rural_urban$dis_speech[7:7],newdata_rural_urban$dis_movement[7:7],
                 newdata_rural_urban$dis_retardation[7:7],newdata_rural_urban$dis_illness[7:7],newdata_rural_urban$dis_other[7:7],newdata_rural_urban$dis_multipledisability[7:7])
  Data_Rural <- data.frame(Groups,Category, Frequency_Rural)
  p_rural <- qplot(Groups,Frequency_Rural, data = Data_Rural,
                stat="identity", geom = "bar", fill = Category, theme_set(theme_bw()))
  p_rural + ggtitle(paste('Activity of Non-Worker based on Disability in Rural', area_name))
  p_rural + geom_text(aes(label = Frequency_Rural), size = 3, hjust = 0.2, vjust = 1, position = "stack")           
  

} else if(choice == 'e') {
  newdata_rural_urban <- subset(Non_Workers, Area.Name==area_name & Total..Rural..Urban != "Total" & Activity.of.Non.worker != "Total", 
                                select=Type.of.disability...In.seeing...Persons:Type.of.disability...In.hearing...Persons:Type.of.disability...In.speech...Persons:Type.of.disability...In.movement...Persons:Type.of.disability...Mental.Retardation...Persons:Type.of.disability...Mental.Illness...Persons:Type.of.disability...Any.other...Persons:Type.of.disability...Multiple.disability...Persons)
  names(newdata_rural_urban)[names(newdata_rural_urban)=="Type.of.disability...In.seeing...Persons"] <- "dis_seeing"
  names(newdata_rural_urban)[names(newdata_rural_urban)=="Type.of.disability...In.hearing...Persons"] <- "dis_hearing"
  names(newdata_rural_urban)[names(newdata_rural_urban)=="Type.of.disability...In.speech...Persons"] <- "dis_speech"
  names(newdata_rural_urban)[names(newdata_rural_urban)=="Type.of.disability...In.movement...Persons"] <- "dis_movement"
  names(newdata_rural_urban)[names(newdata_rural_urban)=="Type.of.disability...Mental.Retardation...Persons"] <- "dis_retardation"
  names(newdata_rural_urban)[names(newdata_rural_urban)=="Type.of.disability...Mental.Illness...Persons"] <- "dis_illness"
  names(newdata_rural_urban)[names(newdata_rural_urban)=="Type.of.disability...Any.other...Persons"] <- "dis_other"
  names(newdata_rural_urban)[names(newdata_rural_urban)=="Type.of.disability...Multiple.disability...Persons"] <- "dis_multipledisability"  
  
  #Analysis of all 8 types of disability in Urban Region
  Groups_Urban <- c(rep(c("Student", "Household duties","Dependent", "Pensioner", "Rentier", "Beggar  Vagrants etc.", "Others"), each = 8))
  Category_Urban  <- c(rep(c("seeing", "hearing", "speech","movement", "retardation", "illness", "other", "multipledisability"), times = 7))
  Frequency_Urban <- c(newdata_rural_urban$dis_seeing[8:8],newdata_rural_urban$dis_hearing[8:8],
                       newdata_rural_urban$dis_speech[8:8],newdata_rural_urban$dis_movement[8:8],
                       newdata_rural_urban$dis_retardation[8:8], newdata_rural_urban$dis_illness[8:8],
                       newdata_rural_urban$dis_other[8:8],newdata_rural_urban$dis_multipledisability[8:8],
                       newdata_rural_urban$dis_seeing[9:9],newdata_rural_urban$dis_hearing[9:9],newdata_rural_urban$dis_speech[9:9],newdata_rural_urban$dis_movement[9:9],
                       newdata_rural_urban$dis_retardation[9:9],newdata_rural_urban$dis_illness[9:9],newdata_rural_urban$dis_other[9:9],newdata_rural_urban$dis_multipledisability[9:9],
                       newdata_rural_urban$dis_seeing[10:10],newdata_rural_urban$dis_hearing[10:10],newdata_rural_urban$dis_speech[10:10],newdata_rural_urban$dis_movement[10:10],
                       newdata_rural_urban$dis_retardation[10:10],newdata_rural_urban$dis_illness[10:10],newdata_rural_urban$dis_other[10:10],newdata_rural_urban$dis_multipledisability[10:10],
                       newdata_rural_urban$dis_seeing[11:11],newdata_rural_urban$dis_hearing[11:11],newdata_rural_urban$dis_speech[11:11],newdata_rural_urban$dis_movement[11:11],
                       newdata_rural_urban$dis_retardation[11:11],newdata_rural_urban$dis_illness[11:11],newdata_rural_urban$dis_other[11:11],newdata_rural_urban$dis_multipledisability[11:11],
                       newdata_rural_urban$dis_seeing[12:12],newdata_rural_urban$dis_hearing[12:12],newdata_rural_urban$dis_speech[12:12],newdata_rural_urban$dis_movement[12:12],
                       newdata_rural_urban$dis_retardation[12:12],newdata_rural_urban$dis_illness[12:12],newdata_rural_urban$dis_other[12:12],newdata_rural_urban$dis_multipledisability[12:12],
                       newdata_rural_urban$dis_seeing[13:13],newdata_rural_urban$dis_hearing[13:13],newdata_rural_urban$dis_speech[13:13],newdata_rural_urban$dis_movement[13:13],
                       newdata_rural_urban$dis_retardation[13:13],newdata_rural_urban$dis_illness[13:13],newdata_rural_urban$dis_other[13:13],newdata_rural_urban$dis_multipledisability[13:13],
                       newdata_rural_urban$dis_seeing[14:14],newdata_rural_urban$dis_hearing[14:14],newdata_rural_urban$dis_speech[14:14],newdata_rural_urban$dis_movement[14:14],
                       newdata_rural_urban$dis_retardation[14:14],newdata_rural_urban$dis_illness[14:14],newdata_rural_urban$dis_other[14:14],newdata_rural_urban$dis_multipledisability[14:14])
  
  
  Data_Urban <- data.frame(Groups_Urban,Category_Urban, Frequency_Urban)
  p_urban <- qplot(Groups_Urban,Frequency_Urban, data = Data_Urban,
                   stat="identity", geom = "bar", fill = Category_Urban, theme_set(theme_bw()))
  p_urban + ggtitle(paste('Activity of Non-Worker based on Disability in Urban', area_name))
  p_urban + geom_text(aes(label = Frequency_Urban), size = 3, hjust = 0.2, vjust = 1, position = "stack")
    
} else if(choice == 'z') {
  #write the code for the summary
  newdata_country <- subset(Non_Workers, Area.Name!='INDIA' & Total..Rural..Urban == "Total" & Activity.of.Non.worker == "Total", select=Activity.of.Non.worker:Total.disabled.non.worker...Persons)
  names(newdata_country)[names(newdata_country)=="Total.disabled.non.worker...Persons"] <- "total"
  slices <- c(newdata_country$total) 
  lbls <- c("JK", "HP", "PUNJAB", "CHANDIGARH", "UTTARAKHAND",
            "HARYANA", "DELHI", "RAJASTHAN", "UP", "BIHAR", "SIKKIM", "ARUNACHAL", "NAGALAND",
            "MANIPUR", "MIZORAM", "TRIPURA", "MEGHALAYA", "ASSAM", "WB", "JHARKHAND", "ODISHA", 
            "CHHATTISGARH", "MP", "GUJARAT", "DIU", "DADRA", "MAHARASHTRA", "ANDHRA", "KARNATAKA", "GOA",
            "LAKSHADWEEP", "KERALA", "TN", "PUDUCHERRY", "ANDAMAN")
  pct <- round(slices/sum(slices)*100, digits = 2)
  lbls <- paste(lbls, pct) # add percents to labels 
  lbls <- paste(lbls,"%",sep="") # ad % to labels 
  pie(slices,labels = lbls, col=rainbow(length(lbls)),
      main='Percentage of Non Wokers due to Disability in each state in INDIA', cex.names=0.2)
  
  bp_country<-barplot(newdata_country$total, las=2, names.arg= lbls, main='Number of Non Wokers due to Disability in each state in INDIA', axes = FALSE, col="Green", cex.names=0.5)
  options("scipen"=100)
  par(mfrow=c(1,1))
  usr <- par("usr")
  par(usr=c(usr[1:2], 0, 3000000))
  axis(side = 2, at = seq(0, 3000000,1000000)) 
  text(bp_country, 0, round(newdata_country$total, 1),cex=0.5,pos=3)
} else {
  print("Invalid choice, please execute again\n");
  quit("default", 0, TRUE)
}

