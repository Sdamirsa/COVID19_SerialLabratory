###########Libraries##### 


library(ggrepel)
library(ggplot2)
library(extrafont)
library(dplyr)
library(writexl)
library(readxl)
library(FactoMineR)

library(viridis)
library(hrbrthemes)
#baray save pdf bedone az bein raftane text
library(Cairo)
library(extrafont)
library(outliers)

#sourcaye khob ke sari beri bebini
library(tidyverse)
library(tidyverse)

#to add p values to graph
# see this to draw p values on graph http://www.sthda.com/english/articles/24-ggpubr-publication-ready-plots/76-add-p-values-and-significance-levels-to-ggplots/
library(ggpubr)



#palet haye rangi dare: https://www.datanovia.com/en/blog/top-r-color-palettes-to-know-for-great-data-visualization/
#ba in code neshon dade mishe display.brewer.all()
library(RColorBrewer)

#baraye kenar ham gozashtane chandta figure
library(gridExtra)

#to impute
library(Amelia)
library(Zelig)
library(ZeligChoice)
library(texreg)

library(Hmisc)

library(mice)

#for qq plot
library("car")

###read data#############
Dataset=read.csv("C:/DOCUMENTS/MY WORK/MaGHALE/Big COVID PROJECT/Article Lab/covid.csv")
attach(Dataset)
Dataset$gender=as.factor(Dataset$gender)
Dataset$Outcome=as.factor(Dataset$Outcome)



R.version

###WBC1################################

#SHow me the geom point to get familiar with data
p_WBC1_withoutlier <- ggplot(Dataset, aes(x=Outcome, y=WBC1)) + 
  geom_point(position=position_jitter(0.3), alpha=0.2)
p_WBC1_withoutlier

#calculate and filter data based on Z score < > 3
WBC1_z_scores <- ave(Dataset$WBC1,Dataset$Outcome,FUN=scale)
Dataset$Zscore_WBC1=WBC1_z_scores
DB_WBC1 <- filter(Dataset, between(Zscore_WBC1,-3,3))

#show me cleaned data without outliers
p_WBC1_clnd <- ggplot(DB_WBC1, aes(x=Outcome, y=WBC1)) + 
  geom_point(position=position_jitter(0.3), alpha=0.2)
p_WBC1_clnd

#create violin plot with mean and 95% CI 
p_WBC1 <- ggplot(DB_WBC1, aes(x=Outcome, y=WBC1, fill=Outcome)) + 
  geom_violin(trim = FALSE)+
  stat_summary(fun.data=mean_sdl, geom="pointrange", color="White", size=1,shape=18)+
  scale_color_brewer(palette="Dark2")+
  theme(text = element_text(family = "sans", size = 14),
        legend.position = "None",axis.title.x = element_blank())
p_WBC1



###LYMPHH1################################

#SHow me the geom point to get familiar with data
p_LYMPHH1_withoutlier <- ggplot(Dataset, aes(x=Outcome, y=LYMPHH1)) + 
  geom_point(position=position_jitter(0.3), alpha=0.2)
p_LYMPHH1_withoutlier

#calculate and filter data based on Z score < > 3
LYMPHH1_z_scores <- ave(Dataset$LYMPHH1,Dataset$Outcome,FUN=scale)
Dataset$Zscore_LYMPHH1=LYMPHH1_z_scores
DB_LYMPHH1 <- filter(Dataset, between(Zscore_LYMPHH1,-3,3))

#show me cleaned data without outliers
p_LYMPHH1_clnd <- ggplot(DB_LYMPHH1, aes(x=Outcome, y=LYMPHH1)) + 
  geom_point(position=position_jitter(0.3), alpha=0.2)
p_LYMPHH1_clnd

#create violin plot with mean and 95% CI 
p_LYMPHH1 <- ggplot(DB_LYMPHH1, aes(x=Outcome, y=LYMPHH1, fill=Outcome)) + 
  geom_violin(trim = FALSE)+
  stat_summary(fun.data=mean_sdl, geom="pointrange", color="White", size=1,shape=18)+
  scale_color_brewer(palette="Dark2")+
  theme(text = element_text(family = "sans", size = 14),
        legend.position = "None",axis.title.x = element_blank())
p_LYMPHH1

###NEUT1################################

#SHow me the geom point to get familiar with data
p_NEUT1_withoutlier <- ggplot(Dataset, aes(x=Outcome, y=NEUT1)) + 
  geom_point(position=position_jitter(0.3), alpha=0.2)
p_NEUT1_withoutlier

#calculate and filter data based on Z score < > 3
NEUT1_z_scores <- ave(Dataset$NEUT1,Dataset$Outcome,FUN=scale)
Dataset$Zscore_NEUT1=NEUT1_z_scores
DB_NEUT1 <- filter(Dataset, between(Zscore_NEUT1,-3,3))

#show me cleaned data without outliers
p_NEUT1_clnd <- ggplot(DB_NEUT1, aes(x=Outcome, y=NEUT1)) + 
  geom_point(position=position_jitter(0.3), alpha=0.2)
p_NEUT1_clnd

#create violin plot with mean and 95% CI 
p_NEUT1 <- ggplot(DB_NEUT1, aes(x=Outcome, y=NEUT1, fill=Outcome)) + 
  geom_violin(trim = FALSE)+
  stat_summary(fun.data=mean_sdl, geom="pointrange", color="White", size=1,shape=18)+
  scale_color_brewer(palette="Dark2")+
  theme(text = element_text(family = "sans", size = 14),
        legend.position = "None",axis.title.x = element_blank())
p_NEUT1


###PLT1################################

#SHow me the geom point to get familiar with data
p_PLT1_withoutlier <- ggplot(Dataset, aes(x=Outcome, y=PLT1)) + 
  geom_point(position=position_jitter(0.3), alpha=0.2)
p_PLT1_withoutlier

#calculate and filter data based on Z score < > 3
PLT1_z_scores <- ave(Dataset$PLT1,Dataset$Outcome,FUN=scale)
Dataset$Zscore_PLT1=PLT1_z_scores
DB_PLT1 <- filter(Dataset, between(Zscore_PLT1,-3,3))

#show me cleaned data without outliers
p_PLT1_clnd <- ggplot(DB_PLT1, aes(x=Outcome, y=PLT1)) + 
  geom_point(position=position_jitter(0.3), alpha=0.2)
p_PLT1_clnd

#create violin plot with mean and 95% CI 
p_PLT1 <- ggplot(DB_PLT1, aes(x=Outcome, y=PLT1, fill=Outcome)) + 
  geom_violin(trim = FALSE)+
  stat_summary(fun.data=mean_sdl, geom="pointrange", color="White", size=1,shape=18)+
  scale_color_brewer(palette="Dark2")+
  theme(text = element_text(family = "sans", size = 14),
        legend.position = "None",axis.title.x = element_blank())
p_PLT1











#START PRINT
cairo_pdf(filename = "C:/DOCUMENTS/MY WORK/MaGHALE/Big COVID PROJECT/Article Lab/draft lab.pdf",
          fallback_resolution = 400,
          onefile = TRUE, family = "Helvetica", pointsize=1)

# baraye merge kardan plot ha to ye saf: 
#http://www.sthda.com/english/articles/24-ggpubr-publication-ready-plots/81-ggplot2-easy-way-to-mix-multiple-graphs-on-the-same-page/#:~:text=To%20arrange%20multiple%20ggplot2%20graphs,multiple%20ggplots%20on%20one%20page
grid.arrange(p_WBC1,p_NEUT1,p_LYMPHH1,p_PLT1 , 
             ncol = 2, nrow = 2)

















###############serial LAB VLAUES WBC#############################

#read dataset
Dataset=read.csv("C:/DOCUMENTS/MY WORK/MaGHALE/Big COVID PROJECT/Article Lab/covid.csv")
attach(Dataset)
Dataset$gender=as.factor(Dataset$gender)
Dataset$Outcome=as.factor(Dataset$Outcome)

# create a dataset with just WBCs
data_Labs <- Dataset[c("Outcome", "WBC1", "WBC2", "WBC3","WBC4","WBC5","WBC6")]
Name_of_lab="WBC"

###########Hiddein Serial lab values WBC###############

# we use it for creating labels in ggplot
  #select patient with at least 4 valid points
data_Labs$WBC_NA <- rowSums(is.na(data_Labs))
frValid_data_Labs <- filter(data_Labs, between(WBC_NA, 0,2))

# remove outliers 
frValid_data_Labs$Lab1=frValid_data_Labs$WBC1
frValid_data_Labs$Lab2=frValid_data_Labs$WBC2
frValid_data_Labs$Lab3=frValid_data_Labs$WBC3
frValid_data_Labs$Lab4=frValid_data_Labs$WBC4
frValid_data_Labs$Lab5=frValid_data_Labs$WBC5
frValid_data_Labs$Lab6=frValid_data_Labs$WBC6
frValid_data_Labs <- frValid_data_Labs
summary(frValid_data_Labs$Outcome)

f1 <- ave(frValid_data_Labs$Lab1,frValid_data_Labs$Outcome,FUN=scale)
f2 <- ave(frValid_data_Labs$Lab2,frValid_data_Labs$Outcome,FUN=scale)
f3 <- ave(frValid_data_Labs$Lab3,frValid_data_Labs$Outcome,FUN=scale)
f4 <- ave(frValid_data_Labs$Lab4,frValid_data_Labs$Outcome,FUN=scale)
f5 <- ave(frValid_data_Labs$Lab5,frValid_data_Labs$Outcome,FUN=scale)
f6 <- ave(frValid_data_Labs$Lab6,frValid_data_Labs$Outcome,FUN=scale)

frValid_data_Labs$f1=f1
frValid_data_Labs$f2=f2
frValid_data_Labs$f3=f3
frValid_data_Labs$f4=f4
frValid_data_Labs$f5=f5
frValid_data_Labs$f6=f6

outremovd_frValid_data_Labs <- filter (frValid_data_Labs, 
                                       (between(f6, -2,3)| is.na(f6)) & (between(f5, -3,3)| is.na(f5)) &
                                         (between(f4, -3,3)| is.na(f4)) & (between(f3, -3,3)| is.na(f3)) &
                                         (between(f2, -3,3)| is.na(f2)) & (between(f1, -3,3)| is.na(f1)) )


#tamiz kardan data
seriallab<- outremovd_frValid_data_Labs[c("Outcome", "Lab1","Lab2","Lab3","Lab4","Lab5","Lab6")]
seriallab$Outcome=as.factor(seriallab$Outcome)

#impute non_valids va data sete tamiz FAILED: DATA KHAILI BAD TAR HAM SHOD BAD AZ IMPUTE

#seriallab_bamissing<- outremovd_frValid_data_WBCs[c("Outcome", "Lab1","Lab2","Lab3","Lab4","Lab5","Lab6")]
#seriallab_bamissing$Outcome=as.factor(seriallab_bamissing$Outcome)
#imputed_seriallab <- mice(seriallab_bamissing, m=5, maxit = 50, method = 'pmm', seed = 500)
#summary(imputed_seriallab)
#a=imputed_seriallab$imp$Lab1
#seriallab_imputed <- complete(imputed_seriallab,1)
#seriallab <- seriallab_imputed

#long kardan
long_outremovd_frValid_data_WBCs <- seriallab %>%
  pivot_longer(cols=c("Lab1","Lab2","Lab3","Lab4","Lab5","Lab6"),
               names_to = "serial_lab", values_to = "lab_value")


# mohasebe p value beine moteghaiere serial dar 1 dataye paired
long_outremovd_frValid_data_WBCs$serial_lab = as.factor(long_outremovd_frValid_data_WBCs$serial_lab)
compare_means(lab_value ~ serial_lab,  data = long_outremovd_frValid_data_WBCs)

#keshidan shekle p value ha 
my_comparisons <- list( c("Lab1", "Lab2"),  c("Lab1", "Lab3"), c("Lab1", "Lab4"), c("Lab1", "Lab5"),c("Lab1", "Lab6")
                        #c("Lab2", "Lab3"), c("Lab2", "Lab4"), c("Lab2", "Lab5"),c("Lab2", "Lab6"),
                        #c("Lab3", "Lab4"), c("Lab3", "Lab5"),c("Lab3", "Lab6"),
                        #c("Lab4", "Lab5"),c("Lab4", "Lab6"),
                        #c("Lab5", "Lab6")
                        )

ggplot(long_outremovd_frValid_data_WBCs, aes(x=serial_lab, y=lab_value, group=serial_lab, fill=Outcome))+
  geom_violin()+
  facet_grid(. ~ Outcome)+
  stat_compare_means(comparisons = my_comparisons, label = "p.signif")+ 
  stat_summary(fun.data=mean_sdl, geom="pointrange", color="White", size=0.7,shape=18)





# average and confidence interval of survived
seriallab_survvd = filter(seriallab, Outcome=="Survived")


survvd_avrg1=mean(seriallab_survvd$Lab1,na.rm = TRUE)
survvd_avrg2=mean(seriallab_survvd$Lab2,na.rm = TRUE)
survvd_avrg3=mean(seriallab_survvd$Lab3,na.rm = TRUE)
survvd_avrg4=mean(seriallab_survvd$Lab4,na.rm = TRUE)
survvd_avrg5=mean(seriallab_survvd$Lab5,na.rm = TRUE)
survvd_avrg6=mean(seriallab_survvd$Lab6,na.rm = TRUE)


model1 <- lm(Lab1 ~ 1, seriallab_survvd)
survvd_ci1 <- confint(model1, level=0.95)
survvd_low1 = survvd_ci1[1]
survvd_upr1 = survvd_ci1[2]

model2 <- lm(Lab2 ~ 1, seriallab_survvd)
survvd_ci2 <- confint(model2, level=0.95)
survvd_low2 =  survvd_ci2[1]
survvd_upr2 = survvd_ci2[2]

model3 <- lm(Lab3 ~ 1, seriallab_survvd)
survvd_ci3 <- confint(model3, level=0.95)
survvd_low3 =  survvd_ci3[1]
survvd_upr3 = survvd_ci3[2]

model4 <- lm(Lab4 ~ 1, seriallab_survvd)
survvd_ci4 <- confint(model4, level=0.95)
survvd_low4 =  survvd_ci4[1]
survvd_upr4 = survvd_ci4[2]

model5 <- lm(Lab5 ~ 1, seriallab_survvd)
survvd_ci5 <- confint(model5, level=0.95)
survvd_low5 =  survvd_ci5[1]
survvd_upr5 = survvd_ci5[2]

model6 <- lm(Lab6 ~ 1, seriallab_survvd)
survvd_ci6 <- confint(model6, level=0.95)
survvd_low6 =  survvd_ci6[1]
survvd_upr6 = survvd_ci6[2]


############ average and confidence interval of mortality
seriallab_mort = filter(seriallab, Outcome=="Mortality")


mort_avrg1=mean(seriallab_mort$Lab1,na.rm = TRUE)
mort_avrg2=mean(seriallab_mort$Lab2,na.rm = TRUE)
mort_avrg3=mean(seriallab_mort$Lab3,na.rm = TRUE)
mort_avrg4=mean(seriallab_mort$Lab4,na.rm = TRUE)
mort_avrg5=mean(seriallab_mort$Lab5,na.rm = TRUE)
mort_avrg6=mean(seriallab_mort$Lab6,na.rm = TRUE)


model1 <- lm(Lab1 ~ 1, seriallab_mort)
mort_ci1 <- confint(model1, level=0.95)
mort_low1 = mort_ci1[1]
mort_upr1 = mort_ci1[2]

model2 <- lm(Lab2 ~ 1, seriallab_mort)
mort_ci2 <- confint(model2, level=0.95)
mort_low2 =  mort_ci2[1]
mort_upr2 = mort_ci2[2]

model3 <- lm(Lab3 ~ 1, seriallab_mort)
mort_ci3 <- confint(model3, level=0.95)
mort_low3 =  mort_ci3[1]
mort_upr3 = mort_ci3[2]

model4 <- lm(Lab4 ~ 1, seriallab_mort)
mort_ci4 <- confint(model4, level=0.95)
mort_low4 =  mort_ci4[1]
mort_upr4 = mort_ci4[2]

model5 <- lm(Lab5 ~ 1, seriallab_mort)
mort_ci5 <- confint(model5, level=0.95)
mort_low5 =  mort_ci5[1]
mort_upr5 = mort_ci5[2]

model6 <- lm(Lab6 ~ 1, seriallab_mort)
mort_ci6 <- confint(model6, level=0.95)
mort_low6=  mort_ci6[1]
mort_upr6 = mort_ci6[2]

################# sotre text of mean (lci, uci) for survvied
txt_survvd_avrg1=as.character(round(survvd_avrg1,2))
txt_survvd_low1= as.character(round(survvd_low1,2))
txt_survvd_upr1 = as.character(round(survvd_upr1,2))
label__survvd1=paste(txt_survvd_avrg1,"\n(",txt_survvd_low1,", ",txt_survvd_upr1,")", sep = "")


txt_survvd_avrg2=as.character(round(survvd_avrg2,2))
txt_survvd_low2= as.character(round(survvd_low2,2))
txt_survvd_upr2 = as.character(round(survvd_upr2,2))
label__survvd2=paste(txt_survvd_avrg2,"\n(",txt_survvd_low2,", ",txt_survvd_upr2,")", sep = "")

txt_survvd_avrg3=as.character(round(survvd_avrg3,2))
txt_survvd_low3= as.character(round(survvd_low3,2))
txt_survvd_upr3 = as.character(round(survvd_upr3,2))
label__survvd3=paste(txt_survvd_avrg3,"\n(",txt_survvd_low3,", ",txt_survvd_upr3,")", sep = "")

txt_survvd_avrg4=as.character(round(survvd_avrg4,2))
txt_survvd_low4= as.character(round(survvd_low4,2))
txt_survvd_upr4 = as.character(round(survvd_upr4,2))
label__survvd4=paste(txt_survvd_avrg4,"\n(",txt_survvd_low4,", ",txt_survvd_upr4,")", sep = "")

txt_survvd_avrg5=as.character(round(survvd_avrg5,2))
txt_survvd_low5= as.character(round(survvd_low5,2))
txt_survvd_upr5 = as.character(round(survvd_upr5,2))
label__survvd5=paste(txt_survvd_avrg5,"\n(",txt_survvd_low5,", ",txt_survvd_upr5,")", sep = "")

txt_survvd_avrg6=as.character(round(survvd_avrg6,2))
txt_survvd_low6= as.character(round(survvd_low6,2))
txt_survvd_upr6 = as.character(round(survvd_upr6,2))
label__survvd6=paste(txt_survvd_avrg6,"\n(",txt_survvd_low6,", ",txt_survvd_upr6,")", sep = "")

################## sotre text of mean (lci, uci) for mortality

txt_mort_avrg1=as.character(round(mort_avrg1,2))
txt_mort_low1= as.character(round(mort_low1,2))
txt_mort_upr1 = as.character(round(mort_upr1,2))
label__mort1=paste(txt_mort_avrg1,"\n(",txt_mort_low1,", ",txt_mort_upr1,")", sep = "")

txt_mort_avrg2=as.character(round(mort_avrg2,2))
txt_mort_low2= as.character(round(mort_low2,2))
txt_mort_upr2 = as.character(round(mort_upr2,2))
label__mort2=paste(txt_mort_avrg2,"\n(",txt_mort_low2,", ",txt_mort_upr2,")", sep = "")

txt_mort_avrg3=as.character(round(mort_avrg3,2))
txt_mort_low3= as.character(round(mort_low3,2))
txt_mort_upr3 = as.character(round(mort_upr3,2))
label__mort3=paste(txt_mort_avrg3,"\n(",txt_mort_low3,", ",txt_mort_upr3,")", sep = "")

txt_mort_avrg4=as.character(round(mort_avrg4,2))
txt_mort_low4= as.character(round(mort_low4,2))
txt_mort_upr4 = as.character(round(mort_upr4,2))
label__mort4=paste(txt_mort_avrg4,"\n(",txt_mort_low4,", ",txt_mort_upr4,")", sep = "")

txt_mort_avrg5=as.character(round(mort_avrg5,2))
txt_mort_low5= as.character(round(mort_low5,2))
txt_mort_upr5 = as.character(round(mort_upr5,2))
label__mort5=paste(txt_mort_avrg5,"\n(",txt_mort_low5,", ",txt_mort_upr5,")", sep = "")

txt_mort_avrg6=as.character(round(mort_avrg6,2))
txt_mort_low6= as.character(round(mort_low6,2))
txt_mort_upr6 = as.character(round(mort_upr6,2))
label__mort6=paste(txt_mort_avrg6,"\n(",txt_mort_low6,", ",txt_mort_upr6,")", sep = "")


########### add these mean and Ci as a label of your table


Serial_plot<- ggplot(long_outremovd_frValid_data_WBCs, aes(x=serial_lab, y=lab_value, group=serial_lab, fill=Outcome))+
  geom_violin(alpha=0.3, color="grey")+
  geom_boxplot(notch=TRUE,color="black", size =1, alpha=1,outlier.alpha = 0.0)+
  facet_grid(. ~ Outcome)+
  stat_compare_means(comparisons = my_comparisons,label = "p.signif", method="wilcox.test")+ # or  method="t.test"
  stat_summary(fun.data=mean_sdl, geom="line", color="White", size=0.5,shape=18)+
  theme(axis.title.y = element_blank())
Serial_plot



dat_text <- data.frame(
  label = c(label__survvd1,label__survvd2,label__survvd3,label__survvd4,label__survvd5,label__survvd6,
            label__mort1,label__mort2,label__mort3,label__mort4,label__mort5,label__mort6),
  Outcome = c("Survived","Survived","Survived","Survived","Survived","Survived",
            "Mortality","Mortality","Mortality","Mortality","Mortality","Mortality"),
  x     = c(1,2,3,4,5,6,
            1,2,3,4,5,6),
  y     = c(-4,-4,-4,-4,-4,-4,
            -4,-4,-4,-4,-4,-4)
)


serial_plot_wconfidence<-Serial_plot+
  annotate("text", x =1, y=-4,
           label= label__survvd1,
           col="dark blue",
           size=3)+
  annotate("text", x =2, y=-4,
           label= label__survvd2,
           col="dark blue",
           size=3)+
  annotate("text", x =3, y=-4,
           label= label__survvd3,
           col="dark blue",
           size=3)+
  annotate("text", x =4, y=-4,
           label= label__survvd4,
           col="dark blue",
           size=3)+
  annotate("text", x =5, y=-4,
           label= label__survvd5,
           col="dark blue",
           size=3)+
  annotate("text", x =6, y=-4,
           label= label__survvd6,
           col="dark blue",
           size=3)+
  annotate("text", x =1, y=-10,
           label= label__mort1,
           col="dark red",
           size=3)+
  annotate("text", x =2, y=-10,
           label= label__mort2,
           col="dark red",
           size=3)+
  annotate("text", x =3, y=-10,
           label= label__mort3,
           col="dark red",
           size=3)+
  annotate("text", x =4, y=-10,
           label= label__mort4,
           col="dark red",
           size=3)+
  annotate("text", x =5, y=-10,
           label= label__mort5,
           col="dark red",
           size=3)+
  annotate("text", x =6, y=-10,
           label= label__mort6,
           col="dark red",
           size=3)
serial_plot_wconfidence

#change X axis labels 
Name_of_lab1=paste(Name_of_lab,"1")
Name_of_lab2=paste(Name_of_lab,"2")
Name_of_lab3=paste(Name_of_lab,"3")
Name_of_lab4=paste(Name_of_lab,"4")
Name_of_lab5=paste(Name_of_lab,"5")
Name_of_lab6=paste(Name_of_lab,"6")

Final_Plot_serial_lab<-serial_plot_wconfidence+
  scale_x_discrete(labels=c("Lab1" = Name_of_lab1,
                            "Lab2" = Name_of_lab2,
                            "Lab3" = Name_of_lab3,
                            "Lab4" = Name_of_lab4,
                            "Lab5" = Name_of_lab5,
                            "Lab6" = Name_of_lab6
  ))+
  annotate("text", x= 6.5, y=-7, label = "mean and 95% CI",angle="90")


#######show serial lab values result WBC###############

Final_Plot_serial_WBC <- Final_Plot_serial_lab
Final_Plot_serial_WBC

Average=c(mort_avrg1,
          mort_avrg2,
          mort_avrg3,
          mort_avrg4,
          mort_avrg5,
          mort_avrg6,
          survvd_avrg1,
          survvd_avrg2,
          survvd_avrg3,
          survvd_avrg4,
          survvd_avrg5,
          survvd_avrg6)

LowerCI=c(mort_low1,
          mort_low2,
          mort_low3,
          mort_low4,
          mort_low5,
          mort_low6,
          survvd_low1,
          survvd_low2,
          survvd_low3,
          survvd_low4,
          survvd_low5,
          survvd_low6)

UpperCI=c(mort_upr1,
          mort_upr2,
          mort_upr3,
          mort_upr4,
          mort_upr5,
          mort_upr6,
          survvd_upr1,
          survvd_upr2,
          survvd_upr3,
          survvd_upr4,
          survvd_upr5,
          survvd_upr6)

OUTCOME=c("Mortality",
          "Mortality",
          "Mortality",
          "Mortality",
          "Mortality",
          "Mortality",
          "Survived",
          "Survived",
          "Survived",
          "Survived",
          "Survived",
          "Survived")

Lab_Names=c(Name_of_lab,
            Name_of_lab,
            Name_of_lab,
            Name_of_lab,
            Name_of_lab,
            Name_of_lab,
            Name_of_lab,
            Name_of_lab,
            Name_of_lab,
            Name_of_lab,
            Name_of_lab,
            Name_of_lab)

Day_of_Lab=c(1,2,3,4,5,6,1,2,3,4,5,6)
For_joinpoint_WBC=data.frame(mortAverage=Average,
                             LowerCI=LowerCI,
                             UpperCI=UpperCI,
                             Lab_Names=Lab_Names,
                             Day_of_Lab=Day_of_Lab,
                             OUTCOME=OUTCOME)

qqPlot(long_outremovd_frValid_data_WBCs$lab_value) #currently method is wilcoxon, change to t.test if normalized

###############serial LAB VLAUES lymph#############################

#read dataset
Dataset=read.csv("C:/DOCUMENTS/MY WORK/MaGHALE/Big COVID PROJECT/Article Lab/covid.csv")
attach(Dataset)
Dataset$gender=as.factor(Dataset$gender)
Dataset$Outcome=as.factor(Dataset$Outcome)

# create a dataset with just WBCs
data_Labs <- Dataset[c("Outcome","lymphcount1", "lymphcount2", "lymphcount3","lymphcount4","lymphcount5","lymphcount6")]
Name_of_lab="Lymphocyte"

data_Labs$Lab1<-data_Labs$lymphcount1
data_Labs$Lab2<-data_Labs$lymphcount2
data_Labs$Lab3<-data_Labs$lymphcount3
data_Labs$Lab4<-data_Labs$lymphcount4
data_Labs$Lab5<-data_Labs$lymphcount5
data_Labs$Lab6<-data_Labs$lymphcount6

# we use it for creating labels in ggplot
#select patient with at least 4 valid points
data_Labs$labs_NA <- rowSums(is.na(data_Labs))
frValid_data_Labs <- filter(data_Labs, between(labs_NA, 0,2))

# remove outliers 
frValid_data_Labs <- frValid_data_Labs
frValid_data_Labs$Outcome=as.factor(frValid_data_Labs$Outcome)
summary(frValid_data_Labs$Outcome)
###########Hidden Serial lab values lymph###############

f1 <- ave(frValid_data_Labs$Lab1,frValid_data_Labs$Outcome,FUN=scale)
f2 <- ave(frValid_data_Labs$Lab2,frValid_data_Labs$Outcome,FUN=scale)
f3 <- ave(frValid_data_Labs$Lab3,frValid_data_Labs$Outcome,FUN=scale)
f4 <- ave(frValid_data_Labs$Lab4,frValid_data_Labs$Outcome,FUN=scale)
f5 <- ave(frValid_data_Labs$Lab5,frValid_data_Labs$Outcome,FUN=scale)
f6 <- ave(frValid_data_Labs$Lab6,frValid_data_Labs$Outcome,FUN=scale)

frValid_data_Labs$f1=f1
frValid_data_Labs$f2=f2
frValid_data_Labs$f3=f3
frValid_data_Labs$f4=f4
frValid_data_Labs$f5=f5
frValid_data_Labs$f6=f6

outremovd_frValid_data_Labs <- filter (frValid_data_Labs, 
                                       (between(f6, -3,3)| is.na(f6)) & (between(f5, -3,3)| is.na(f5)) &
                                         (between(f4, -3,3)| is.na(f4)) & (between(f3, -3,3)| is.na(f3)) &
                                         (between(f2, -3,3)| is.na(f2)) & (between(f1, -3,3)| is.na(f1)) )


#tamiz kardan data
seriallab<- outremovd_frValid_data_Labs[c("Outcome", "Lab1","Lab2","Lab3","Lab4","Lab5","Lab6")]
seriallab$Outcome=as.factor(seriallab$Outcome)

#impute non_valids va data sete tamiz FAILED: DATA KHAILI BAD TAR HAM SHOD BAD AZ IMPUTE

#seriallab_bamissing<- outremovd_frValid_data_WBCs[c("Outcome", "Lab1","Lab2","Lab3","Lab4","Lab5","Lab6")]
#seriallab_bamissing$Outcome=as.factor(seriallab_bamissing$Outcome)
#imputed_seriallab <- mice(seriallab_bamissing, m=5, maxit = 50, method = 'pmm', seed = 500)
#summary(imputed_seriallab)
#a=imputed_seriallab$imp$Lab1
#seriallab_imputed <- complete(imputed_seriallab,1)
#seriallab <- seriallab_imputed

#long kardan
long_outremovd_frValid_data_WBCs <- seriallab %>%
  pivot_longer(cols=c("Lab1","Lab2","Lab3","Lab4","Lab5","Lab6"),
               names_to = "serial_lab", values_to = "lab_value")


# mohasebe p value beine moteghaiere serial dar 1 dataye paired
long_outremovd_frValid_data_WBCs$serial_lab = as.factor(long_outremovd_frValid_data_WBCs$serial_lab)
compare_means(lab_value ~ serial_lab,  data = long_outremovd_frValid_data_WBCs)

#keshidan shekle p value ha 
my_comparisons <- list( c("Lab1", "Lab2"),  c("Lab1", "Lab3"), c("Lab1", "Lab4"), c("Lab1", "Lab5"),c("Lab1", "Lab6")
                        #c("Lab2", "Lab3"), c("Lab2", "Lab4"), c("Lab2", "Lab5"),c("Lab2", "Lab6"),
                        #c("Lab3", "Lab4"), c("Lab3", "Lab5"),c("Lab3", "Lab6"),
                        #c("Lab4", "Lab5"),c("Lab4", "Lab6"),
                        #c("Lab5", "Lab6")
)

ggplot(long_outremovd_frValid_data_WBCs, aes(x=serial_lab, y=lab_value, group=serial_lab, fill=Outcome))+
  geom_violin()+
  facet_grid(. ~ Outcome)+
  stat_compare_means(comparisons = my_comparisons, label = "p.signif")+ 
  stat_summary(fun.data=mean_sdl, geom="pointrange", color="White", size=0.7,shape=18)





# average and confidence interval of survived
seriallab_survvd = filter(seriallab, Outcome=="Survived")


survvd_avrg1=mean(seriallab_survvd$Lab1,na.rm = TRUE)
survvd_avrg2=mean(seriallab_survvd$Lab2,na.rm = TRUE)
survvd_avrg3=mean(seriallab_survvd$Lab3,na.rm = TRUE)
survvd_avrg4=mean(seriallab_survvd$Lab4,na.rm = TRUE)
survvd_avrg5=mean(seriallab_survvd$Lab5,na.rm = TRUE)
survvd_avrg6=mean(seriallab_survvd$Lab6,na.rm = TRUE)


model1 <- lm(Lab1 ~ 1, seriallab_survvd)
survvd_ci1 <- confint(model1, level=0.95)
survvd_low1 = survvd_ci1[1]
survvd_upr1 = survvd_ci1[2]

model2 <- lm(Lab2 ~ 1, seriallab_survvd)
survvd_ci2 <- confint(model2, level=0.95)
survvd_low2 =  survvd_ci2[1]
survvd_upr2 = survvd_ci2[2]

model3 <- lm(Lab3 ~ 1, seriallab_survvd)
survvd_ci3 <- confint(model3, level=0.95)
survvd_low3 =  survvd_ci3[1]
survvd_upr3 = survvd_ci3[2]

model4 <- lm(Lab4 ~ 1, seriallab_survvd)
survvd_ci4 <- confint(model4, level=0.95)
survvd_low4 =  survvd_ci4[1]
survvd_upr4 = survvd_ci4[2]

model5 <- lm(Lab5 ~ 1, seriallab_survvd)
survvd_ci5 <- confint(model5, level=0.95)
survvd_low5 =  survvd_ci5[1]
survvd_upr5 = survvd_ci5[2]

model6 <- lm(Lab6 ~ 1, seriallab_survvd)
survvd_ci6 <- confint(model6, level=0.95)
survvd_low6 =  survvd_ci6[1]
survvd_upr6 = survvd_ci6[2]


############ average and confidence interval of mortality
seriallab_mort = filter(seriallab, Outcome=="Mortality")


mort_avrg1=mean(seriallab_mort$Lab1,na.rm = TRUE)
mort_avrg2=mean(seriallab_mort$Lab2,na.rm = TRUE)
mort_avrg3=mean(seriallab_mort$Lab3,na.rm = TRUE)
mort_avrg4=mean(seriallab_mort$Lab4,na.rm = TRUE)
mort_avrg5=mean(seriallab_mort$Lab5,na.rm = TRUE)
mort_avrg6=mean(seriallab_mort$Lab6,na.rm = TRUE)


model1 <- lm(Lab1 ~ 1, seriallab_mort)
mort_ci1 <- confint(model1, level=0.95)
mort_low1 = mort_ci1[1]
mort_upr1 = mort_ci1[2]

model2 <- lm(Lab2 ~ 1, seriallab_mort)
mort_ci2 <- confint(model2, level=0.95)
mort_low2 =  mort_ci2[1]
mort_upr2 = mort_ci2[2]

model3 <- lm(Lab3 ~ 1, seriallab_mort)
mort_ci3 <- confint(model3, level=0.95)
mort_low3 =  mort_ci3[1]
mort_upr3 = mort_ci3[2]

model4 <- lm(Lab4 ~ 1, seriallab_mort)
mort_ci4 <- confint(model4, level=0.95)
mort_low4 =  mort_ci4[1]
mort_upr4 = mort_ci4[2]

model5 <- lm(Lab5 ~ 1, seriallab_mort)
mort_ci5 <- confint(model5, level=0.95)
mort_low5 =  mort_ci5[1]
mort_upr5 = mort_ci5[2]

model6 <- lm(Lab6 ~ 1, seriallab_mort)
mort_ci6 <- confint(model6, level=0.95)
mort_low6=  mort_ci6[1]
mort_upr6 = mort_ci6[2]

################# sotre text of mean (lci, uci) for survvied
txt_survvd_avrg1=as.character(round(survvd_avrg1,2))
txt_survvd_low1= as.character(round(survvd_low1,2))
txt_survvd_upr1 = as.character(round(survvd_upr1,2))
label__survvd1=paste(txt_survvd_avrg1,"\n(",txt_survvd_low1,", ",txt_survvd_upr1,")", sep = "")


txt_survvd_avrg2=as.character(round(survvd_avrg2,2))
txt_survvd_low2= as.character(round(survvd_low2,2))
txt_survvd_upr2 = as.character(round(survvd_upr2,2))
label__survvd2=paste(txt_survvd_avrg2,"\n(",txt_survvd_low2,", ",txt_survvd_upr2,")", sep = "")

txt_survvd_avrg3=as.character(round(survvd_avrg3,2))
txt_survvd_low3= as.character(round(survvd_low3,2))
txt_survvd_upr3 = as.character(round(survvd_upr3,2))
label__survvd3=paste(txt_survvd_avrg3,"\n(",txt_survvd_low3,", ",txt_survvd_upr3,")", sep = "")

txt_survvd_avrg4=as.character(round(survvd_avrg4,2))
txt_survvd_low4= as.character(round(survvd_low4,2))
txt_survvd_upr4 = as.character(round(survvd_upr4,2))
label__survvd4=paste(txt_survvd_avrg4,"\n(",txt_survvd_low4,", ",txt_survvd_upr4,")", sep = "")

txt_survvd_avrg5=as.character(round(survvd_avrg5,2))
txt_survvd_low5= as.character(round(survvd_low5,2))
txt_survvd_upr5 = as.character(round(survvd_upr5,2))
label__survvd5=paste(txt_survvd_avrg5,"\n(",txt_survvd_low5,", ",txt_survvd_upr5,")", sep = "")

txt_survvd_avrg6=as.character(round(survvd_avrg6,2))
txt_survvd_low6= as.character(round(survvd_low6,2))
txt_survvd_upr6 = as.character(round(survvd_upr6,2))
label__survvd6=paste(txt_survvd_avrg6,"\n(",txt_survvd_low6,", ",txt_survvd_upr6,")", sep = "")

################## sotre text of mean (lci, uci) for mortality

txt_mort_avrg1=as.character(round(mort_avrg1,2))
txt_mort_low1= as.character(round(mort_low1,2))
txt_mort_upr1 = as.character(round(mort_upr1,2))
label__mort1=paste(txt_mort_avrg1,"\n(",txt_mort_low1,", ",txt_mort_upr1,")", sep = "")

txt_mort_avrg2=as.character(round(mort_avrg2,2))
txt_mort_low2= as.character(round(mort_low2,2))
txt_mort_upr2 = as.character(round(mort_upr2,2))
label__mort2=paste(txt_mort_avrg2,"\n(",txt_mort_low2,", ",txt_mort_upr2,")", sep = "")

txt_mort_avrg3=as.character(round(mort_avrg3,2))
txt_mort_low3= as.character(round(mort_low3,2))
txt_mort_upr3 = as.character(round(mort_upr3,2))
label__mort3=paste(txt_mort_avrg3,"\n(",txt_mort_low3,", ",txt_mort_upr3,")", sep = "")

txt_mort_avrg4=as.character(round(mort_avrg4,2))
txt_mort_low4= as.character(round(mort_low4,2))
txt_mort_upr4 = as.character(round(mort_upr4,2))
label__mort4=paste(txt_mort_avrg4,"\n(",txt_mort_low4,", ",txt_mort_upr4,")", sep = "")

txt_mort_avrg5=as.character(round(mort_avrg5,2))
txt_mort_low5= as.character(round(mort_low5,2))
txt_mort_upr5 = as.character(round(mort_upr5,2))
label__mort5=paste(txt_mort_avrg5,"\n(",txt_mort_low5,", ",txt_mort_upr5,")", sep = "")

txt_mort_avrg6=as.character(round(mort_avrg6,2))
txt_mort_low6= as.character(round(mort_low6,2))
txt_mort_upr6 = as.character(round(mort_upr6,2))
label__mort6=paste(txt_mort_avrg6,"\n(",txt_mort_low6,", ",txt_mort_upr6,")", sep = "")


########### add these mean and Ci as a label of your table

Serial_plot<- ggplot(long_outremovd_frValid_data_WBCs, aes(x=serial_lab, y=lab_value, group=serial_lab, fill=Outcome))+
  geom_violin(alpha=0.3, color="grey")+
  geom_boxplot(notch=TRUE,color="black", size =1, alpha=1,outlier.alpha = 0.0)+
  facet_grid(. ~ Outcome)+
  stat_compare_means(comparisons = my_comparisons,label = "p.signif", method="wilcox.test")+ # or  method="t.test"
  stat_summary(fun.data=mean_sdl, geom="line", color="White", size=0.5,shape=18)+
  theme(axis.title.y = element_blank())
Serial_plot




dat_text <- data.frame(
  label = c(label__survvd1,label__survvd2,label__survvd3,label__survvd4,label__survvd5,label__survvd6,
            label__mort1,label__mort2,label__mort3,label__mort4,label__mort5,label__mort6),
  Outcome = c("Survived","Survived","Survived","Survived","Survived","Survived",
              "Mortality","Mortality","Mortality","Mortality","Mortality","Mortality"),
  x     = c(1,2,3,4,5,6,
            1,2,3,4,5,6),
  y     = c(-4,-4,-4,-4,-4,-4,
            -4,-4,-4,-4,-4,-4)
)


serial_plot_wconfidence<-Serial_plot+
  annotate("text", x =1, y=-4,
           label= label__survvd1,
           col="dark blue",
           size=3)+
  annotate("text", x =2, y=-4,
           label= label__survvd2,
           col="dark blue",
           size=3)+
  annotate("text", x =3, y=-4,
           label= label__survvd3,
           col="dark blue",
           size=3)+
  annotate("text", x =4, y=-4,
           label= label__survvd4,
           col="dark blue",
           size=3)+
  annotate("text", x =5, y=-4,
           label= label__survvd5,
           col="dark blue",
           size=3)+
  annotate("text", x =6, y=-4,
           label= label__survvd6,
           col="dark blue",
           size=3)+
  annotate("text", x =1, y=-100,
           label= label__mort1,
           col="dark red",
           size=3)+
  annotate("text", x =2, y=-10,
           label= label__mort2,
           col="dark red",
           size=3)+
  annotate("text", x =3, y=-10,
           label= label__mort3,
           col="dark red",
           size=3)+
  annotate("text", x =4, y=-10,
           label= label__mort4,
           col="dark red",
           size=3)+
  annotate("text", x =5, y=-10,
           label= label__mort5,
           col="dark red",
           size=3)+
  annotate("text", x =6, y=-10,
           label= label__mort6,
           col="dark red",
           size=3)
serial_plot_wconfidence

#change X axis labels 
Name_of_lab1=paste(Name_of_lab,"1")
Name_of_lab2=paste(Name_of_lab,"2")
Name_of_lab3=paste(Name_of_lab,"3")
Name_of_lab4=paste(Name_of_lab,"4")
Name_of_lab5=paste(Name_of_lab,"5")
Name_of_lab6=paste(Name_of_lab,"6")

Final_Plot_serial_lab<-serial_plot_wconfidence+
  scale_x_discrete(labels=c("Lab1" = Name_of_lab1,
                            "Lab2" = Name_of_lab2,
                            "Lab3" = Name_of_lab3,
                            "Lab4" = Name_of_lab4,
                            "Lab5" = Name_of_lab5,
                            "Lab6" = Name_of_lab6
  ))+
  annotate("text", x= 6.5, y=-7, label = "mean and 95% CI",angle="90")


#######show serial lab values result lymph###############

Final_Plot_serial_lymph <- Final_Plot_serial_lab
Final_Plot_serial_lymph 

#create average and CI for joinpoint
Average=c(mort_avrg1,
          mort_avrg2,
          mort_avrg3,
          mort_avrg4,
          mort_avrg5,
          mort_avrg6,
          survvd_avrg1,
          survvd_avrg2,
          survvd_avrg3,
          survvd_avrg4,
          survvd_avrg5,
          survvd_avrg6)

LowerCI=c(mort_low1,
          mort_low2,
          mort_low3,
          mort_low4,
          mort_low5,
          mort_low6,
          survvd_low1,
          survvd_low2,
          survvd_low3,
          survvd_low4,
          survvd_low5,
          survvd_low6)

UpperCI=c(mort_upr1,
          mort_upr2,
          mort_upr3,
          mort_upr4,
          mort_upr5,
          mort_upr6,
          survvd_upr1,
          survvd_upr2,
          survvd_upr3,
          survvd_upr4,
          survvd_upr5,
          survvd_upr6)

OUTCOME=c("Mortality",
          "Mortality",
          "Mortality",
          "Mortality",
          "Mortality",
          "Mortality",
          "Survived",
          "Survived",
          "Survived",
          "Survived",
          "Survived",
          "Survived")

Lab_Names=c(Name_of_lab,
           Name_of_lab,
           Name_of_lab,
           Name_of_lab,
           Name_of_lab,
           Name_of_lab,
           Name_of_lab,
           Name_of_lab,
           Name_of_lab,
           Name_of_lab,
           Name_of_lab,
           Name_of_lab)

Day_of_Lab=c(1,2,3,4,5,6,1,2,3,4,5,6)
For_joinpoint_lymph=data.frame(mortAverage=Average,
                             LowerCI=LowerCI,
                             UpperCI=UpperCI,
                             Lab_Names=Lab_Names,
                             Day_of_Lab=Day_of_Lab,
                             OUTCOME=OUTCOME)


qqPlot(long_outremovd_frValid_data_WBCs$lab_value) #currently method is wilcoxon, change to t.test if normalized


###############serial LAB VLAUES neut#############################

#read dataset
Dataset=read.csv("C:/DOCUMENTS/MY WORK/MaGHALE/Big COVID PROJECT/Article Lab/covid.csv")
attach(Dataset)
Dataset$gender=as.factor(Dataset$gender)
Dataset$Outcome=as.factor(Dataset$Outcome)

# create a dataset with just WBCs
data_Labs <- Dataset[c("Outcome","Neutcount1", "Neutcount2", "Neutcount3","Neutcount4","Neutcount5","Neutcount6")]
Name_of_lab="Neutrophil"

data_Labs$Lab1<-data_Labs$Neutcount1
data_Labs$Lab2<-data_Labs$Neutcount2
data_Labs$Lab3<-data_Labs$Neutcount3
data_Labs$Lab4<-data_Labs$Neutcount4
data_Labs$Lab5<-data_Labs$Neutcount5
data_Labs$Lab6<-data_Labs$Neutcount6

# we use it for creating labels in ggplot
#select patient with at least 4 valid points
data_Labs$labs_NA <- rowSums(is.na(data_Labs))
frValid_data_Labs <- filter(data_Labs, between(labs_NA, 0,2))

# remove outliers 
frValid_data_Labs <- frValid_data_Labs
frValid_data_Labs$Outcome=as.factor(frValid_data_Labs$Outcome)
summary(frValid_data_Labs$Outcome)
###########Hidden Serial lab values neut###############

f1 <- ave(frValid_data_Labs$Lab1,frValid_data_Labs$Outcome,FUN=scale)
f2 <- ave(frValid_data_Labs$Lab2,frValid_data_Labs$Outcome,FUN=scale)
f3 <- ave(frValid_data_Labs$Lab3,frValid_data_Labs$Outcome,FUN=scale)
f4 <- ave(frValid_data_Labs$Lab4,frValid_data_Labs$Outcome,FUN=scale)
f5 <- ave(frValid_data_Labs$Lab5,frValid_data_Labs$Outcome,FUN=scale)
f6 <- ave(frValid_data_Labs$Lab6,frValid_data_Labs$Outcome,FUN=scale)

frValid_data_Labs$f1=f1
frValid_data_Labs$f2=f2
frValid_data_Labs$f3=f3
frValid_data_Labs$f4=f4
frValid_data_Labs$f5=f5
frValid_data_Labs$f6=f6

outremovd_frValid_data_Labs <- filter (frValid_data_Labs, 
                                       (between(f6, -3,3)| is.na(f6)) & (between(f5, -3,3)| is.na(f5)) &
                                         (between(f4, -3,3)| is.na(f4)) & (between(f3, -3,3)| is.na(f3)) &
                                         (between(f2, -3,3)| is.na(f2)) & (between(f1, -3,3)| is.na(f1)) )


#tamiz kardan data
seriallab<- outremovd_frValid_data_Labs[c("Outcome", "Lab1","Lab2","Lab3","Lab4","Lab5","Lab6")]
seriallab$Outcome=as.factor(seriallab$Outcome)

#impute non_valids va data sete tamiz FAILED: DATA KHAILI BAD TAR HAM SHOD BAD AZ IMPUTE

#seriallab_bamissing<- outremovd_frValid_data_WBCs[c("Outcome", "Lab1","Lab2","Lab3","Lab4","Lab5","Lab6")]
#seriallab_bamissing$Outcome=as.factor(seriallab_bamissing$Outcome)
#imputed_seriallab <- mice(seriallab_bamissing, m=5, maxit = 50, method = 'pmm', seed = 500)
#summary(imputed_seriallab)
#a=imputed_seriallab$imp$Lab1
#seriallab_imputed <- complete(imputed_seriallab,1)
#seriallab <- seriallab_imputed

#long kardan
long_outremovd_frValid_data_WBCs <- seriallab %>%
  pivot_longer(cols=c("Lab1","Lab2","Lab3","Lab4","Lab5","Lab6"),
               names_to = "serial_lab", values_to = "lab_value")


# mohasebe p value beine moteghaiere serial dar 1 dataye paired
long_outremovd_frValid_data_WBCs$serial_lab = as.factor(long_outremovd_frValid_data_WBCs$serial_lab)
compare_means(lab_value ~ serial_lab,  data = long_outremovd_frValid_data_WBCs)

#keshidan shekle p value ha 
my_comparisons <- list( c("Lab1", "Lab2"),  c("Lab1", "Lab3"), c("Lab1", "Lab4"), c("Lab1", "Lab5"),c("Lab1", "Lab6")
                        #c("Lab2", "Lab3"), c("Lab2", "Lab4"), c("Lab2", "Lab5"),c("Lab2", "Lab6"),
                        #c("Lab3", "Lab4"), c("Lab3", "Lab5"),c("Lab3", "Lab6"),
                        #c("Lab4", "Lab5"),c("Lab4", "Lab6"),
                        #c("Lab5", "Lab6")
)

ggplot(long_outremovd_frValid_data_WBCs, aes(x=serial_lab, y=lab_value, group=serial_lab, fill=Outcome))+
  geom_violin()+
  facet_grid(. ~ Outcome)+
  stat_compare_means(comparisons = my_comparisons, label = "p.signif")+ 
  stat_summary(fun.data=mean_sdl, geom="pointrange", color="White", size=0.7,shape=18)





# average and confidence interval of survived
seriallab_survvd = filter(seriallab, Outcome=="Survived")


survvd_avrg1=mean(seriallab_survvd$Lab1,na.rm = TRUE)
survvd_avrg2=mean(seriallab_survvd$Lab2,na.rm = TRUE)
survvd_avrg3=mean(seriallab_survvd$Lab3,na.rm = TRUE)
survvd_avrg4=mean(seriallab_survvd$Lab4,na.rm = TRUE)
survvd_avrg5=mean(seriallab_survvd$Lab5,na.rm = TRUE)
survvd_avrg6=mean(seriallab_survvd$Lab6,na.rm = TRUE)


model1 <- lm(Lab1 ~ 1, seriallab_survvd)
survvd_ci1 <- confint(model1, level=0.95)
survvd_low1 = survvd_ci1[1]
survvd_upr1 = survvd_ci1[2]

model2 <- lm(Lab2 ~ 1, seriallab_survvd)
survvd_ci2 <- confint(model2, level=0.95)
survvd_low2 =  survvd_ci2[1]
survvd_upr2 = survvd_ci2[2]

model3 <- lm(Lab3 ~ 1, seriallab_survvd)
survvd_ci3 <- confint(model3, level=0.95)
survvd_low3 =  survvd_ci3[1]
survvd_upr3 = survvd_ci3[2]

model4 <- lm(Lab4 ~ 1, seriallab_survvd)
survvd_ci4 <- confint(model4, level=0.95)
survvd_low4 =  survvd_ci4[1]
survvd_upr4 = survvd_ci4[2]

model5 <- lm(Lab5 ~ 1, seriallab_survvd)
survvd_ci5 <- confint(model5, level=0.95)
survvd_low5 =  survvd_ci5[1]
survvd_upr5 = survvd_ci5[2]

model6 <- lm(Lab6 ~ 1, seriallab_survvd)
survvd_ci6 <- confint(model6, level=0.95)
survvd_low6 =  survvd_ci6[1]
survvd_upr6 = survvd_ci6[2]


############ average and confidence interval of mortality
seriallab_mort = filter(seriallab, Outcome=="Mortality")


mort_avrg1=mean(seriallab_mort$Lab1,na.rm = TRUE)
mort_avrg2=mean(seriallab_mort$Lab2,na.rm = TRUE)
mort_avrg3=mean(seriallab_mort$Lab3,na.rm = TRUE)
mort_avrg4=mean(seriallab_mort$Lab4,na.rm = TRUE)
mort_avrg5=mean(seriallab_mort$Lab5,na.rm = TRUE)
mort_avrg6=mean(seriallab_mort$Lab6,na.rm = TRUE)


model1 <- lm(Lab1 ~ 1, seriallab_mort)
mort_ci1 <- confint(model1, level=0.95)
mort_low1 = mort_ci1[1]
mort_upr1 = mort_ci1[2]

model2 <- lm(Lab2 ~ 1, seriallab_mort)
mort_ci2 <- confint(model2, level=0.95)
mort_low2 =  mort_ci2[1]
mort_upr2 = mort_ci2[2]

model3 <- lm(Lab3 ~ 1, seriallab_mort)
mort_ci3 <- confint(model3, level=0.95)
mort_low3 =  mort_ci3[1]
mort_upr3 = mort_ci3[2]

model4 <- lm(Lab4 ~ 1, seriallab_mort)
mort_ci4 <- confint(model4, level=0.95)
mort_low4 =  mort_ci4[1]
mort_upr4 = mort_ci4[2]

model5 <- lm(Lab5 ~ 1, seriallab_mort)
mort_ci5 <- confint(model5, level=0.95)
mort_low5 =  mort_ci5[1]
mort_upr5 = mort_ci5[2]

model6 <- lm(Lab6 ~ 1, seriallab_mort)
mort_ci6 <- confint(model6, level=0.95)
mort_low6=  mort_ci6[1]
mort_upr6 = mort_ci6[2]

################# sotre text of mean (lci, uci) for survvied
txt_survvd_avrg1=as.character(round(survvd_avrg1,2))
txt_survvd_low1= as.character(round(survvd_low1,2))
txt_survvd_upr1 = as.character(round(survvd_upr1,2))
label__survvd1=paste(txt_survvd_avrg1,"\n(",txt_survvd_low1,", ",txt_survvd_upr1,")", sep = "")


txt_survvd_avrg2=as.character(round(survvd_avrg2,2))
txt_survvd_low2= as.character(round(survvd_low2,2))
txt_survvd_upr2 = as.character(round(survvd_upr2,2))
label__survvd2=paste(txt_survvd_avrg2,"\n(",txt_survvd_low2,", ",txt_survvd_upr2,")", sep = "")

txt_survvd_avrg3=as.character(round(survvd_avrg3,2))
txt_survvd_low3= as.character(round(survvd_low3,2))
txt_survvd_upr3 = as.character(round(survvd_upr3,2))
label__survvd3=paste(txt_survvd_avrg3,"\n(",txt_survvd_low3,", ",txt_survvd_upr3,")", sep = "")

txt_survvd_avrg4=as.character(round(survvd_avrg4,2))
txt_survvd_low4= as.character(round(survvd_low4,2))
txt_survvd_upr4 = as.character(round(survvd_upr4,2))
label__survvd4=paste(txt_survvd_avrg4,"\n(",txt_survvd_low4,", ",txt_survvd_upr4,")", sep = "")

txt_survvd_avrg5=as.character(round(survvd_avrg5,2))
txt_survvd_low5= as.character(round(survvd_low5,2))
txt_survvd_upr5 = as.character(round(survvd_upr5,2))
label__survvd5=paste(txt_survvd_avrg5,"\n(",txt_survvd_low5,", ",txt_survvd_upr5,")", sep = "")

txt_survvd_avrg6=as.character(round(survvd_avrg6,2))
txt_survvd_low6= as.character(round(survvd_low6,2))
txt_survvd_upr6 = as.character(round(survvd_upr6,2))
label__survvd6=paste(txt_survvd_avrg6,"\n(",txt_survvd_low6,", ",txt_survvd_upr6,")", sep = "")

################## sotre text of mean (lci, uci) for mortality

txt_mort_avrg1=as.character(round(mort_avrg1,2))
txt_mort_low1= as.character(round(mort_low1,2))
txt_mort_upr1 = as.character(round(mort_upr1,2))
label__mort1=paste(txt_mort_avrg1,"\n(",txt_mort_low1,", ",txt_mort_upr1,")", sep = "")

txt_mort_avrg2=as.character(round(mort_avrg2,2))
txt_mort_low2= as.character(round(mort_low2,2))
txt_mort_upr2 = as.character(round(mort_upr2,2))
label__mort2=paste(txt_mort_avrg2,"\n(",txt_mort_low2,", ",txt_mort_upr2,")", sep = "")

txt_mort_avrg3=as.character(round(mort_avrg3,2))
txt_mort_low3= as.character(round(mort_low3,2))
txt_mort_upr3 = as.character(round(mort_upr3,2))
label__mort3=paste(txt_mort_avrg3,"\n(",txt_mort_low3,", ",txt_mort_upr3,")", sep = "")

txt_mort_avrg4=as.character(round(mort_avrg4,2))
txt_mort_low4= as.character(round(mort_low4,2))
txt_mort_upr4 = as.character(round(mort_upr4,2))
label__mort4=paste(txt_mort_avrg4,"\n(",txt_mort_low4,", ",txt_mort_upr4,")", sep = "")

txt_mort_avrg5=as.character(round(mort_avrg5,2))
txt_mort_low5= as.character(round(mort_low5,2))
txt_mort_upr5 = as.character(round(mort_upr5,2))
label__mort5=paste(txt_mort_avrg5,"\n(",txt_mort_low5,", ",txt_mort_upr5,")", sep = "")

txt_mort_avrg6=as.character(round(mort_avrg6,2))
txt_mort_low6= as.character(round(mort_low6,2))
txt_mort_upr6 = as.character(round(mort_upr6,2))
label__mort6=paste(txt_mort_avrg6,"\n(",txt_mort_low6,", ",txt_mort_upr6,")", sep = "")


########### add these mean and Ci as a label of your table

Serial_plot<- ggplot(long_outremovd_frValid_data_WBCs, aes(x=serial_lab, y=lab_value, group=serial_lab, fill=Outcome))+
  geom_violin(alpha=0.3, color="grey")+
  geom_boxplot(notch=TRUE,color="black", size =1, alpha=1,outlier.alpha = 0.0)+
  facet_grid(. ~ Outcome)+
  stat_compare_means(comparisons = my_comparisons,label = "p.signif", method="wilcox.test")+ # or  method="t.test"
  stat_summary(fun.data=mean_sdl, geom="line", color="White", size=0.5,shape=18)+
  theme(axis.title.y = element_blank())
Serial_plot



dat_text <- data.frame(
  label = c(label__survvd1,label__survvd2,label__survvd3,label__survvd4,label__survvd5,label__survvd6,
            label__mort1,label__mort2,label__mort3,label__mort4,label__mort5,label__mort6),
  Outcome = c("Survived","Survived","Survived","Survived","Survived","Survived",
              "Mortality","Mortality","Mortality","Mortality","Mortality","Mortality"),
  x     = c(1,2,3,4,5,6,
            1,2,3,4,5,6),
  y     = c(-4,-4,-4,-4,-4,-4,
            -4,-4,-4,-4,-4,-4)
)


serial_plot_wconfidence<-Serial_plot+
  annotate("text", x =1, y=-4,
           label= label__survvd1,
           col="dark blue",
           size=3)+
  annotate("text", x =2, y=-4,
           label= label__survvd2,
           col="dark blue",
           size=3)+
  annotate("text", x =3, y=-4,
           label= label__survvd3,
           col="dark blue",
           size=3)+
  annotate("text", x =4, y=-4,
           label= label__survvd4,
           col="dark blue",
           size=3)+
  annotate("text", x =5, y=-4,
           label= label__survvd5,
           col="dark blue",
           size=3)+
  annotate("text", x =6, y=-4,
           label= label__survvd6,
           col="dark blue",
           size=3)+
  annotate("text", x =1, y=-10,
           label= label__mort1,
           col="dark red",
           size=3)+
  annotate("text", x =2, y=-10,
           label= label__mort2,
           col="dark red",
           size=3)+
  annotate("text", x =3, y=-10,
           label= label__mort3,
           col="dark red",
           size=3)+
  annotate("text", x =4, y=-10,
           label= label__mort4,
           col="dark red",
           size=3)+
  annotate("text", x =5, y=-10,
           label= label__mort5,
           col="dark red",
           size=3)+
  annotate("text", x =6, y=-10,
           label= label__mort6,
           col="dark red",
           size=3)
serial_plot_wconfidence

#change X axis labels 
Name_of_lab1=paste(Name_of_lab,"1")
Name_of_lab2=paste(Name_of_lab,"2")
Name_of_lab3=paste(Name_of_lab,"3")
Name_of_lab4=paste(Name_of_lab,"4")
Name_of_lab5=paste(Name_of_lab,"5")
Name_of_lab6=paste(Name_of_lab,"6")

Final_Plot_serial_lab<-serial_plot_wconfidence+
  scale_x_discrete(labels=c("Lab1" = Name_of_lab1,
                            "Lab2" = Name_of_lab2,
                            "Lab3" = Name_of_lab3,
                            "Lab4" = Name_of_lab4,
                            "Lab5" = Name_of_lab5,
                            "Lab6" = Name_of_lab6
  ))+
  annotate("text", x= 6.5, y=-7, label = "mean and 95% CI",angle="90")


#######show serial lab values result neut###############

Final_Plot_serial_neut<- Final_Plot_serial_lab
Final_Plot_serial_neut 

Average=c(mort_avrg1,
          mort_avrg2,
          mort_avrg3,
          mort_avrg4,
          mort_avrg5,
          mort_avrg6,
          survvd_avrg1,
          survvd_avrg2,
          survvd_avrg3,
          survvd_avrg4,
          survvd_avrg5,
          survvd_avrg6)

LowerCI=c(mort_low1,
          mort_low2,
          mort_low3,
          mort_low4,
          mort_low5,
          mort_low6,
          survvd_low1,
          survvd_low2,
          survvd_low3,
          survvd_low4,
          survvd_low5,
          survvd_low6)

UpperCI=c(mort_upr1,
          mort_upr2,
          mort_upr3,
          mort_upr4,
          mort_upr5,
          mort_upr6,
          survvd_upr1,
          survvd_upr2,
          survvd_upr3,
          survvd_upr4,
          survvd_upr5,
          survvd_upr6)

OUTCOME=c("Mortality",
          "Mortality",
          "Mortality",
          "Mortality",
          "Mortality",
          "Mortality",
          "Survived",
          "Survived",
          "Survived",
          "Survived",
          "Survived",
          "Survived")

Lab_Names=c(Name_of_lab,
            Name_of_lab,
            Name_of_lab,
            Name_of_lab,
            Name_of_lab,
            Name_of_lab,
            Name_of_lab,
            Name_of_lab,
            Name_of_lab,
            Name_of_lab,
            Name_of_lab,
            Name_of_lab)

Day_of_Lab=c(1,2,3,4,5,6,1,2,3,4,5,6)
For_joinpoint_neut=data.frame(mortAverage=Average,
                             LowerCI=LowerCI,
                             UpperCI=UpperCI,
                             Lab_Names=Lab_Names,
                             Day_of_Lab=Day_of_Lab,
                             OUTCOME=OUTCOME)
For_joinpoint_neut
qqPlot(long_outremovd_frValid_data_WBCs$lab_value) #currently method is wilcoxon, change to t.test if normalized


###############serial LAB VLAUES plt#############################

#read dataset
Dataset=read.csv("C:/DOCUMENTS/MY WORK/MaGHALE/Big COVID PROJECT/Article Lab/covid.csv")
attach(Dataset)
Dataset$gender=as.factor(Dataset$gender)
Dataset$Outcome=as.factor(Dataset$Outcome)

# create a dataset with just WBCs
data_Labs <- Dataset[c("Outcome","PLT1", "PLT2", "PLT3","PLT4","PLT5","PLT6")]
Name_of_lab="PLT"

data_Labs$Lab1<-data_Labs$PLT1
data_Labs$Lab2<-data_Labs$PLT2
data_Labs$Lab3<-data_Labs$PLT3
data_Labs$Lab4<-data_Labs$PLT4
data_Labs$Lab5<-data_Labs$PLT5
data_Labs$Lab6<-data_Labs$PLT6

# we use it for creating labels in ggplot
#select patient with at least 4 valid points
data_Labs$labs_NA <- rowSums(is.na(data_Labs))
frValid_data_Labs <- filter(data_Labs, between(labs_NA, 0,2))

# remove outliers 
frValid_data_Labs <- frValid_data_Labs
frValid_data_Labs$Outcome=as.factor(frValid_data_Labs$Outcome)
summary(frValid_data_Labs$Outcome)
###########Hidden Serial lab values plt###############

f1 <- ave(frValid_data_Labs$Lab1,frValid_data_Labs$Outcome,FUN=scale)
f2 <- ave(frValid_data_Labs$Lab2,frValid_data_Labs$Outcome,FUN=scale)
f3 <- ave(frValid_data_Labs$Lab3,frValid_data_Labs$Outcome,FUN=scale)
f4 <- ave(frValid_data_Labs$Lab4,frValid_data_Labs$Outcome,FUN=scale)
f5 <- ave(frValid_data_Labs$Lab5,frValid_data_Labs$Outcome,FUN=scale)
f6 <- ave(frValid_data_Labs$Lab6,frValid_data_Labs$Outcome,FUN=scale)

frValid_data_Labs$f1=f1
frValid_data_Labs$f2=f2
frValid_data_Labs$f3=f3
frValid_data_Labs$f4=f4
frValid_data_Labs$f5=f5
frValid_data_Labs$f6=f6

outremovd_frValid_data_Labs <- filter (frValid_data_Labs, 
                                       (between(f6, -3,3)| is.na(f6)) & (between(f5, -3,3)| is.na(f5)) &
                                         (between(f4, -3,3)| is.na(f4)) & (between(f3, -3,3)| is.na(f3)) &
                                         (between(f2, -3,3)| is.na(f2)) & (between(f1, -3,3)| is.na(f1)) )


#tamiz kardan data
seriallab<- outremovd_frValid_data_Labs[c("Outcome", "Lab1","Lab2","Lab3","Lab4","Lab5","Lab6")]
seriallab$Outcome=as.factor(seriallab$Outcome)

#impute non_valids va data sete tamiz FAILED: DATA KHAILI BAD TAR HAM SHOD BAD AZ IMPUTE

#seriallab_bamissing<- outremovd_frValid_data_WBCs[c("Outcome", "Lab1","Lab2","Lab3","Lab4","Lab5","Lab6")]
#seriallab_bamissing$Outcome=as.factor(seriallab_bamissing$Outcome)
#imputed_seriallab <- mice(seriallab_bamissing, m=5, maxit = 50, method = 'pmm', seed = 500)
#summary(imputed_seriallab)
#a=imputed_seriallab$imp$Lab1
#seriallab_imputed <- complete(imputed_seriallab,1)
#seriallab <- seriallab_imputed

#long kardan
long_outremovd_frValid_data_WBCs <- seriallab %>%
  pivot_longer(cols=c("Lab1","Lab2","Lab3","Lab4","Lab5","Lab6"),
               names_to = "serial_lab", values_to = "lab_value")


# mohasebe p value beine moteghaiere serial dar 1 dataye paired
long_outremovd_frValid_data_WBCs$serial_lab = as.factor(long_outremovd_frValid_data_WBCs$serial_lab)
compare_means(lab_value ~ serial_lab,  data = long_outremovd_frValid_data_WBCs)

#keshidan shekle p value ha 
my_comparisons <- list( c("Lab1", "Lab2"),  c("Lab1", "Lab3"), c("Lab1", "Lab4"), c("Lab1", "Lab5"),c("Lab1", "Lab6")
                        #c("Lab2", "Lab3"), c("Lab2", "Lab4"), c("Lab2", "Lab5"),c("Lab2", "Lab6"),
                        #c("Lab3", "Lab4"), c("Lab3", "Lab5"),c("Lab3", "Lab6"),
                        #c("Lab4", "Lab5"),c("Lab4", "Lab6"),
                        #c("Lab5", "Lab6")
)

ggplot(long_outremovd_frValid_data_WBCs, aes(x=serial_lab, y=lab_value, group=serial_lab, fill=Outcome))+
  geom_violin()+
  facet_grid(. ~ Outcome)+
  stat_compare_means(comparisons = my_comparisons, label = "p.signif")+ 
  stat_summary(fun.data=mean_sdl, geom="pointrange", color="White", size=0.7,shape=18)





# average and confidence interval of survived
seriallab_survvd = filter(seriallab, Outcome=="Survived")


survvd_avrg1=mean(seriallab_survvd$Lab1,na.rm = TRUE)
survvd_avrg2=mean(seriallab_survvd$Lab2,na.rm = TRUE)
survvd_avrg3=mean(seriallab_survvd$Lab3,na.rm = TRUE)
survvd_avrg4=mean(seriallab_survvd$Lab4,na.rm = TRUE)
survvd_avrg5=mean(seriallab_survvd$Lab5,na.rm = TRUE)
survvd_avrg6=mean(seriallab_survvd$Lab6,na.rm = TRUE)


model1 <- lm(Lab1 ~ 1, seriallab_survvd)
survvd_ci1 <- confint(model1, level=0.95)
survvd_low1 = survvd_ci1[1]
survvd_upr1 = survvd_ci1[2]

model2 <- lm(Lab2 ~ 1, seriallab_survvd)
survvd_ci2 <- confint(model2, level=0.95)
survvd_low2 =  survvd_ci2[1]
survvd_upr2 = survvd_ci2[2]

model3 <- lm(Lab3 ~ 1, seriallab_survvd)
survvd_ci3 <- confint(model3, level=0.95)
survvd_low3 =  survvd_ci3[1]
survvd_upr3 = survvd_ci3[2]

model4 <- lm(Lab4 ~ 1, seriallab_survvd)
survvd_ci4 <- confint(model4, level=0.95)
survvd_low4 =  survvd_ci4[1]
survvd_upr4 = survvd_ci4[2]

model5 <- lm(Lab5 ~ 1, seriallab_survvd)
survvd_ci5 <- confint(model5, level=0.95)
survvd_low5 =  survvd_ci5[1]
survvd_upr5 = survvd_ci5[2]

model6 <- lm(Lab6 ~ 1, seriallab_survvd)
survvd_ci6 <- confint(model6, level=0.95)
survvd_low6 =  survvd_ci6[1]
survvd_upr6 = survvd_ci6[2]


############ average and confidence interval of mortality
seriallab_mort = filter(seriallab, Outcome=="Mortality")


mort_avrg1=mean(seriallab_mort$Lab1,na.rm = TRUE)
mort_avrg2=mean(seriallab_mort$Lab2,na.rm = TRUE)
mort_avrg3=mean(seriallab_mort$Lab3,na.rm = TRUE)
mort_avrg4=mean(seriallab_mort$Lab4,na.rm = TRUE)
mort_avrg5=mean(seriallab_mort$Lab5,na.rm = TRUE)
mort_avrg6=mean(seriallab_mort$Lab6,na.rm = TRUE)


model1 <- lm(Lab1 ~ 1, seriallab_mort)
mort_ci1 <- confint(model1, level=0.95)
mort_low1 = mort_ci1[1]
mort_upr1 = mort_ci1[2]

model2 <- lm(Lab2 ~ 1, seriallab_mort)
mort_ci2 <- confint(model2, level=0.95)
mort_low2 =  mort_ci2[1]
mort_upr2 = mort_ci2[2]

model3 <- lm(Lab3 ~ 1, seriallab_mort)
mort_ci3 <- confint(model3, level=0.95)
mort_low3 =  mort_ci3[1]
mort_upr3 = mort_ci3[2]

model4 <- lm(Lab4 ~ 1, seriallab_mort)
mort_ci4 <- confint(model4, level=0.95)
mort_low4 =  mort_ci4[1]
mort_upr4 = mort_ci4[2]

model5 <- lm(Lab5 ~ 1, seriallab_mort)
mort_ci5 <- confint(model5, level=0.95)
mort_low5 =  mort_ci5[1]
mort_upr5 = mort_ci5[2]

model6 <- lm(Lab6 ~ 1, seriallab_mort)
mort_ci6 <- confint(model6, level=0.95)
mort_low6=  mort_ci6[1]
mort_upr6 = mort_ci6[2]

################# sotre text of mean (lci, uci) for survvied
txt_survvd_avrg1=as.character(round(survvd_avrg1,2))
txt_survvd_low1= as.character(round(survvd_low1,2))
txt_survvd_upr1 = as.character(round(survvd_upr1,2))
label__survvd1=paste(txt_survvd_avrg1,"\n(",txt_survvd_low1,", ",txt_survvd_upr1,")", sep = "")


txt_survvd_avrg2=as.character(round(survvd_avrg2,2))
txt_survvd_low2= as.character(round(survvd_low2,2))
txt_survvd_upr2 = as.character(round(survvd_upr2,2))
label__survvd2=paste(txt_survvd_avrg2,"\n(",txt_survvd_low2,", ",txt_survvd_upr2,")", sep = "")

txt_survvd_avrg3=as.character(round(survvd_avrg3,2))
txt_survvd_low3= as.character(round(survvd_low3,2))
txt_survvd_upr3 = as.character(round(survvd_upr3,2))
label__survvd3=paste(txt_survvd_avrg3,"\n(",txt_survvd_low3,", ",txt_survvd_upr3,")", sep = "")

txt_survvd_avrg4=as.character(round(survvd_avrg4,2))
txt_survvd_low4= as.character(round(survvd_low4,2))
txt_survvd_upr4 = as.character(round(survvd_upr4,2))
label__survvd4=paste(txt_survvd_avrg4,"\n(",txt_survvd_low4,", ",txt_survvd_upr4,")", sep = "")

txt_survvd_avrg5=as.character(round(survvd_avrg5,2))
txt_survvd_low5= as.character(round(survvd_low5,2))
txt_survvd_upr5 = as.character(round(survvd_upr5,2))
label__survvd5=paste(txt_survvd_avrg5,"\n(",txt_survvd_low5,", ",txt_survvd_upr5,")", sep = "")

txt_survvd_avrg6=as.character(round(survvd_avrg6,2))
txt_survvd_low6= as.character(round(survvd_low6,2))
txt_survvd_upr6 = as.character(round(survvd_upr6,2))
label__survvd6=paste(txt_survvd_avrg6,"\n(",txt_survvd_low6,", ",txt_survvd_upr6,")", sep = "")

################## sotre text of mean (lci, uci) for mortality

txt_mort_avrg1=as.character(round(mort_avrg1,2))
txt_mort_low1= as.character(round(mort_low1,2))
txt_mort_upr1 = as.character(round(mort_upr1,2))
label__mort1=paste(txt_mort_avrg1,"\n(",txt_mort_low1,", ",txt_mort_upr1,")", sep = "")

txt_mort_avrg2=as.character(round(mort_avrg2,2))
txt_mort_low2= as.character(round(mort_low2,2))
txt_mort_upr2 = as.character(round(mort_upr2,2))
label__mort2=paste(txt_mort_avrg2,"\n(",txt_mort_low2,", ",txt_mort_upr2,")", sep = "")

txt_mort_avrg3=as.character(round(mort_avrg3,2))
txt_mort_low3= as.character(round(mort_low3,2))
txt_mort_upr3 = as.character(round(mort_upr3,2))
label__mort3=paste(txt_mort_avrg3,"\n(",txt_mort_low3,", ",txt_mort_upr3,")", sep = "")

txt_mort_avrg4=as.character(round(mort_avrg4,2))
txt_mort_low4= as.character(round(mort_low4,2))
txt_mort_upr4 = as.character(round(mort_upr4,2))
label__mort4=paste(txt_mort_avrg4,"\n(",txt_mort_low4,", ",txt_mort_upr4,")", sep = "")

txt_mort_avrg5=as.character(round(mort_avrg5,2))
txt_mort_low5= as.character(round(mort_low5,2))
txt_mort_upr5 = as.character(round(mort_upr5,2))
label__mort5=paste(txt_mort_avrg5,"\n(",txt_mort_low5,", ",txt_mort_upr5,")", sep = "")

txt_mort_avrg6=as.character(round(mort_avrg6,2))
txt_mort_low6= as.character(round(mort_low6,2))
txt_mort_upr6 = as.character(round(mort_upr6,2))
label__mort6=paste(txt_mort_avrg6,"\n(",txt_mort_low6,", ",txt_mort_upr6,")", sep = "")


########### add these mean and Ci as a label of your table

Serial_plot<- ggplot(long_outremovd_frValid_data_WBCs, aes(x=serial_lab, y=lab_value, group=serial_lab, fill=Outcome))+
  geom_violin(alpha=0.3, color="grey")+
  geom_boxplot(notch=TRUE,color="black", size =1, alpha=1,outlier.alpha = 0.0)+
  facet_grid(. ~ Outcome)+
  stat_compare_means(comparisons = my_comparisons,label = "p.signif", method="wilcox.test")+ # or  method="t.test"
  stat_summary(fun.data=mean_sdl, geom="line", color="White", size=0.5,shape=18)+
  theme(axis.title.y = element_blank())
Serial_plot




dat_text <- data.frame(
  label = c(label__survvd1,label__survvd2,label__survvd3,label__survvd4,label__survvd5,label__survvd6,
            label__mort1,label__mort2,label__mort3,label__mort4,label__mort5,label__mort6),
  Outcome = c("Survived","Survived","Survived","Survived","Survived","Survived",
              "Mortality","Mortality","Mortality","Mortality","Mortality","Mortality"),
  x     = c(1,2,3,4,5,6,
            1,2,3,4,5,6),
  y     = c(-4,-4,-4,-4,-4,-4,
            -4,-4,-4,-4,-4,-4)
)


serial_plot_wconfidence<-Serial_plot+
  annotate("text", x =1, y=-4,
           label= label__survvd1,
           col="dark blue",
           size=3)+
  annotate("text", x =2, y=-4,
           label= label__survvd2,
           col="dark blue",
           size=3)+
  annotate("text", x =3, y=-4,
           label= label__survvd3,
           col="dark blue",
           size=3)+
  annotate("text", x =4, y=-4,
           label= label__survvd4,
           col="dark blue",
           size=3)+
  annotate("text", x =5, y=-4,
           label= label__survvd5,
           col="dark blue",
           size=3)+
  annotate("text", x =6, y=-4,
           label= label__survvd6,
           col="dark blue",
           size=3)+
  annotate("text", x =1, y=-10,
           label= label__mort1,
           col="dark red",
           size=3)+
  annotate("text", x =2, y=-10,
           label= label__mort2,
           col="dark red",
           size=3)+
  annotate("text", x =3, y=-10,
           label= label__mort3,
           col="dark red",
           size=3)+
  annotate("text", x =4, y=-10,
           label= label__mort4,
           col="dark red",
           size=3)+
  annotate("text", x =5, y=-10,
           label= label__mort5,
           col="dark red",
           size=3)+
  annotate("text", x =6, y=-10,
           label= label__mort6,
           col="dark red",
           size=3)
serial_plot_wconfidence

#change X axis labels 
Name_of_lab1=paste(Name_of_lab,"1")
Name_of_lab2=paste(Name_of_lab,"2")
Name_of_lab3=paste(Name_of_lab,"3")
Name_of_lab4=paste(Name_of_lab,"4")
Name_of_lab5=paste(Name_of_lab,"5")
Name_of_lab6=paste(Name_of_lab,"6")

Final_Plot_serial_lab<-serial_plot_wconfidence+
  scale_x_discrete(labels=c("Lab1" = Name_of_lab1,
                            "Lab2" = Name_of_lab2,
                            "Lab3" = Name_of_lab3,
                            "Lab4" = Name_of_lab4,
                            "Lab5" = Name_of_lab5,
                            "Lab6" = Name_of_lab6
  ))+
  annotate("text", x= 6.5, y=-7, label = "mean and 95% CI",angle="90")


#######show serial lab values result PLT###############

Final_Plot_serial_PLT<- Final_Plot_serial_lab
Final_Plot_serial_PLT 


Average=c(mort_avrg1,
          mort_avrg2,
          mort_avrg3,
          mort_avrg4,
          mort_avrg5,
          mort_avrg6,
          survvd_avrg1,
          survvd_avrg2,
          survvd_avrg3,
          survvd_avrg4,
          survvd_avrg5,
          survvd_avrg6)

LowerCI=c(mort_low1,
          mort_low2,
          mort_low3,
          mort_low4,
          mort_low5,
          mort_low6,
          survvd_low1,
          survvd_low2,
          survvd_low3,
          survvd_low4,
          survvd_low5,
          survvd_low6)

UpperCI=c(mort_upr1,
          mort_upr2,
          mort_upr3,
          mort_upr4,
          mort_upr5,
          mort_upr6,
          survvd_upr1,
          survvd_upr2,
          survvd_upr3,
          survvd_upr4,
          survvd_upr5,
          survvd_upr6)

OUTCOME=c("Mortality",
          "Mortality",
          "Mortality",
          "Mortality",
          "Mortality",
          "Mortality",
          "Survived",
          "Survived",
          "Survived",
          "Survived",
          "Survived",
          "Survived")

Lab_Names=c(Name_of_lab,
            Name_of_lab,
            Name_of_lab,
            Name_of_lab,
            Name_of_lab,
            Name_of_lab,
            Name_of_lab,
            Name_of_lab,
            Name_of_lab,
            Name_of_lab,
            Name_of_lab,
            Name_of_lab)

Day_of_Lab=c(1,2,3,4,5,6,1,2,3,4,5,6)
For_joinpoint_plt=data.frame(mortAverage=Average,
                              LowerCI=LowerCI,
                              UpperCI=UpperCI,
                              Lab_Names=Lab_Names,
                              Day_of_Lab=Day_of_Lab,
                              OUTCOME=OUTCOME)

qqPlot(long_outremovd_frValid_data_WBCs$lab_value) #currently method is wilcoxon, change to t.test if normalized


###############serial LAB VLAUES Hb#############################

#read dataset
Dataset=read.csv("C:/DOCUMENTS/MY WORK/MaGHALE/Big COVID PROJECT/Article Lab/covid.csv")
attach(Dataset)
Dataset$gender=as.factor(Dataset$gender)
Dataset$Outcome=as.factor(Dataset$Outcome)

# create a dataset with just WBCs
data_Labs <- Dataset[c("Outcome","HB1", "HB2", "HB3","HB4","HB5","HB6")]
Name_of_lab="Hb"

data_Labs$Lab1<-data_Labs$HB1
data_Labs$Lab2<-data_Labs$HB2
data_Labs$Lab3<-data_Labs$HB3
data_Labs$Lab4<-data_Labs$HB4
data_Labs$Lab5<-data_Labs$HB5
data_Labs$Lab6<-data_Labs$HB6

# we use it for creating labels in ggplot
#select patient with at least 4 valid points
data_Labs$labs_NA <- rowSums(is.na(data_Labs))
frValid_data_Labs <- filter(data_Labs, between(labs_NA, 0,2))

# remove outliers 
frValid_data_Labs <- frValid_data_Labs
frValid_data_Labs$Outcome=as.factor(frValid_data_Labs$Outcome)
summary(frValid_data_Labs$Outcome)
###########Hidden Serial lab values Hb###############

f1 <- ave(frValid_data_Labs$Lab1,frValid_data_Labs$Outcome,FUN=scale)
f2 <- ave(frValid_data_Labs$Lab2,frValid_data_Labs$Outcome,FUN=scale)
f3 <- ave(frValid_data_Labs$Lab3,frValid_data_Labs$Outcome,FUN=scale)
f4 <- ave(frValid_data_Labs$Lab4,frValid_data_Labs$Outcome,FUN=scale)
f5 <- ave(frValid_data_Labs$Lab5,frValid_data_Labs$Outcome,FUN=scale)
f6 <- ave(frValid_data_Labs$Lab6,frValid_data_Labs$Outcome,FUN=scale)

frValid_data_Labs$f1=f1
frValid_data_Labs$f2=f2
frValid_data_Labs$f3=f3
frValid_data_Labs$f4=f4
frValid_data_Labs$f5=f5
frValid_data_Labs$f6=f6

outremovd_frValid_data_Labs <- filter (frValid_data_Labs, 
                                       (between(f6, -3,3)| is.na(f6)) & (between(f5, -3,3)| is.na(f5)) &
                                         (between(f4, -3,3)| is.na(f4)) & (between(f3, -3,3)| is.na(f3)) &
                                         (between(f2, -3,3)| is.na(f2)) & (between(f1, -3,3)| is.na(f1)) )


#tamiz kardan data
seriallab<- outremovd_frValid_data_Labs[c("Outcome", "Lab1","Lab2","Lab3","Lab4","Lab5","Lab6")]
seriallab$Outcome=as.factor(seriallab$Outcome)

#impute non_valids va data sete tamiz FAILED: DATA KHAILI BAD TAR HAM SHOD BAD AZ IMPUTE

#seriallab_bamissing<- outremovd_frValid_data_WBCs[c("Outcome", "Lab1","Lab2","Lab3","Lab4","Lab5","Lab6")]
#seriallab_bamissing$Outcome=as.factor(seriallab_bamissing$Outcome)
#imputed_seriallab <- mice(seriallab_bamissing, m=5, maxit = 50, method = 'pmm', seed = 500)
#summary(imputed_seriallab)
#a=imputed_seriallab$imp$Lab1
#seriallab_imputed <- complete(imputed_seriallab,1)
#seriallab <- seriallab_imputed

#long kardan
long_outremovd_frValid_data_WBCs <- seriallab %>%
  pivot_longer(cols=c("Lab1","Lab2","Lab3","Lab4","Lab5","Lab6"),
               names_to = "serial_lab", values_to = "lab_value")


# mohasebe p value beine moteghaiere serial dar 1 dataye paired
long_outremovd_frValid_data_WBCs$serial_lab = as.factor(long_outremovd_frValid_data_WBCs$serial_lab)
compare_means(lab_value ~ serial_lab,  data = long_outremovd_frValid_data_WBCs)

#keshidan shekle p value ha 
my_comparisons <- list( c("Lab1", "Lab2"),  c("Lab1", "Lab3"), c("Lab1", "Lab4"), c("Lab1", "Lab5"),c("Lab1", "Lab6")
                        #c("Lab2", "Lab3"), c("Lab2", "Lab4"), c("Lab2", "Lab5"),c("Lab2", "Lab6"),
                        #c("Lab3", "Lab4"), c("Lab3", "Lab5"),c("Lab3", "Lab6"),
                        #c("Lab4", "Lab5"),c("Lab4", "Lab6"),
                        #c("Lab5", "Lab6")
)

ggplot(long_outremovd_frValid_data_WBCs, aes(x=serial_lab, y=lab_value, group=serial_lab, fill=Outcome))+
  geom_violin()+
  facet_grid(. ~ Outcome)+
  stat_compare_means(comparisons = my_comparisons, label = "p.signif")+ 
  stat_summary(fun.data=mean_sdl, geom="pointrange", color="White", size=0.7,shape=18)





# average and confidence interval of survived
seriallab_survvd = filter(seriallab, Outcome=="Survived")


survvd_avrg1=mean(seriallab_survvd$Lab1,na.rm = TRUE)
survvd_avrg2=mean(seriallab_survvd$Lab2,na.rm = TRUE)
survvd_avrg3=mean(seriallab_survvd$Lab3,na.rm = TRUE)
survvd_avrg4=mean(seriallab_survvd$Lab4,na.rm = TRUE)
survvd_avrg5=mean(seriallab_survvd$Lab5,na.rm = TRUE)
survvd_avrg6=mean(seriallab_survvd$Lab6,na.rm = TRUE)


model1 <- lm(Lab1 ~ 1, seriallab_survvd)
survvd_ci1 <- confint(model1, level=0.95)
survvd_low1 = survvd_ci1[1]
survvd_upr1 = survvd_ci1[2]

model2 <- lm(Lab2 ~ 1, seriallab_survvd)
survvd_ci2 <- confint(model2, level=0.95)
survvd_low2 =  survvd_ci2[1]
survvd_upr2 = survvd_ci2[2]

model3 <- lm(Lab3 ~ 1, seriallab_survvd)
survvd_ci3 <- confint(model3, level=0.95)
survvd_low3 =  survvd_ci3[1]
survvd_upr3 = survvd_ci3[2]

model4 <- lm(Lab4 ~ 1, seriallab_survvd)
survvd_ci4 <- confint(model4, level=0.95)
survvd_low4 =  survvd_ci4[1]
survvd_upr4 = survvd_ci4[2]

model5 <- lm(Lab5 ~ 1, seriallab_survvd)
survvd_ci5 <- confint(model5, level=0.95)
survvd_low5 =  survvd_ci5[1]
survvd_upr5 = survvd_ci5[2]

model6 <- lm(Lab6 ~ 1, seriallab_survvd)
survvd_ci6 <- confint(model6, level=0.95)
survvd_low6 =  survvd_ci6[1]
survvd_upr6 = survvd_ci6[2]


############ average and confidence interval of mortality
seriallab_mort = filter(seriallab, Outcome=="Mortality")


mort_avrg1=mean(seriallab_mort$Lab1,na.rm = TRUE)
mort_avrg2=mean(seriallab_mort$Lab2,na.rm = TRUE)
mort_avrg3=mean(seriallab_mort$Lab3,na.rm = TRUE)
mort_avrg4=mean(seriallab_mort$Lab4,na.rm = TRUE)
mort_avrg5=mean(seriallab_mort$Lab5,na.rm = TRUE)
mort_avrg6=mean(seriallab_mort$Lab6,na.rm = TRUE)


model1 <- lm(Lab1 ~ 1, seriallab_mort)
mort_ci1 <- confint(model1, level=0.95)
mort_low1 = mort_ci1[1]
mort_upr1 = mort_ci1[2]

model2 <- lm(Lab2 ~ 1, seriallab_mort)
mort_ci2 <- confint(model2, level=0.95)
mort_low2 =  mort_ci2[1]
mort_upr2 = mort_ci2[2]

model3 <- lm(Lab3 ~ 1, seriallab_mort)
mort_ci3 <- confint(model3, level=0.95)
mort_low3 =  mort_ci3[1]
mort_upr3 = mort_ci3[2]

model4 <- lm(Lab4 ~ 1, seriallab_mort)
mort_ci4 <- confint(model4, level=0.95)
mort_low4 =  mort_ci4[1]
mort_upr4 = mort_ci4[2]

model5 <- lm(Lab5 ~ 1, seriallab_mort)
mort_ci5 <- confint(model5, level=0.95)
mort_low5 =  mort_ci5[1]
mort_upr5 = mort_ci5[2]

model6 <- lm(Lab6 ~ 1, seriallab_mort)
mort_ci6 <- confint(model6, level=0.95)
mort_low6=  mort_ci6[1]
mort_upr6 = mort_ci6[2]

################# sotre text of mean (lci, uci) for survvied
txt_survvd_avrg1=as.character(round(survvd_avrg1,2))
txt_survvd_low1= as.character(round(survvd_low1,2))
txt_survvd_upr1 = as.character(round(survvd_upr1,2))
label__survvd1=paste(txt_survvd_avrg1,"\n(",txt_survvd_low1,", ",txt_survvd_upr1,")", sep = "")


txt_survvd_avrg2=as.character(round(survvd_avrg2,2))
txt_survvd_low2= as.character(round(survvd_low2,2))
txt_survvd_upr2 = as.character(round(survvd_upr2,2))
label__survvd2=paste(txt_survvd_avrg2,"\n(",txt_survvd_low2,", ",txt_survvd_upr2,")", sep = "")

txt_survvd_avrg3=as.character(round(survvd_avrg3,2))
txt_survvd_low3= as.character(round(survvd_low3,2))
txt_survvd_upr3 = as.character(round(survvd_upr3,2))
label__survvd3=paste(txt_survvd_avrg3,"\n(",txt_survvd_low3,", ",txt_survvd_upr3,")", sep = "")

txt_survvd_avrg4=as.character(round(survvd_avrg4,2))
txt_survvd_low4= as.character(round(survvd_low4,2))
txt_survvd_upr4 = as.character(round(survvd_upr4,2))
label__survvd4=paste(txt_survvd_avrg4,"\n(",txt_survvd_low4,", ",txt_survvd_upr4,")", sep = "")

txt_survvd_avrg5=as.character(round(survvd_avrg5,2))
txt_survvd_low5= as.character(round(survvd_low5,2))
txt_survvd_upr5 = as.character(round(survvd_upr5,2))
label__survvd5=paste(txt_survvd_avrg5,"\n(",txt_survvd_low5,", ",txt_survvd_upr5,")", sep = "")

txt_survvd_avrg6=as.character(round(survvd_avrg6,2))
txt_survvd_low6= as.character(round(survvd_low6,2))
txt_survvd_upr6 = as.character(round(survvd_upr6,2))
label__survvd6=paste(txt_survvd_avrg6,"\n(",txt_survvd_low6,", ",txt_survvd_upr6,")", sep = "")

################## sotre text of mean (lci, uci) for mortality

txt_mort_avrg1=as.character(round(mort_avrg1,2))
txt_mort_low1= as.character(round(mort_low1,2))
txt_mort_upr1 = as.character(round(mort_upr1,2))
label__mort1=paste(txt_mort_avrg1,"\n(",txt_mort_low1,", ",txt_mort_upr1,")", sep = "")

txt_mort_avrg2=as.character(round(mort_avrg2,2))
txt_mort_low2= as.character(round(mort_low2,2))
txt_mort_upr2 = as.character(round(mort_upr2,2))
label__mort2=paste(txt_mort_avrg2,"\n(",txt_mort_low2,", ",txt_mort_upr2,")", sep = "")

txt_mort_avrg3=as.character(round(mort_avrg3,2))
txt_mort_low3= as.character(round(mort_low3,2))
txt_mort_upr3 = as.character(round(mort_upr3,2))
label__mort3=paste(txt_mort_avrg3,"\n(",txt_mort_low3,", ",txt_mort_upr3,")", sep = "")

txt_mort_avrg4=as.character(round(mort_avrg4,2))
txt_mort_low4= as.character(round(mort_low4,2))
txt_mort_upr4 = as.character(round(mort_upr4,2))
label__mort4=paste(txt_mort_avrg4,"\n(",txt_mort_low4,", ",txt_mort_upr4,")", sep = "")

txt_mort_avrg5=as.character(round(mort_avrg5,2))
txt_mort_low5= as.character(round(mort_low5,2))
txt_mort_upr5 = as.character(round(mort_upr5,2))
label__mort5=paste(txt_mort_avrg5,"\n(",txt_mort_low5,", ",txt_mort_upr5,")", sep = "")

txt_mort_avrg6=as.character(round(mort_avrg6,2))
txt_mort_low6= as.character(round(mort_low6,2))
txt_mort_upr6 = as.character(round(mort_upr6,2))
label__mort6=paste(txt_mort_avrg6,"\n(",txt_mort_low6,", ",txt_mort_upr6,")", sep = "")


########### add these mean and Ci as a label of your table

Serial_plot<- ggplot(long_outremovd_frValid_data_WBCs, aes(x=serial_lab, y=lab_value, group=serial_lab, fill=Outcome))+
  geom_violin(alpha=0.3, color="grey")+
  geom_boxplot(notch=TRUE,color="black", size =1, alpha=1,outlier.alpha = 0.0)+
  facet_grid(. ~ Outcome)+
  stat_compare_means(comparisons = my_comparisons,label = "p.signif", method="t.test")+ # or  method="t.test"
  stat_summary(fun.data=mean_sdl, geom="line", color="White", size=0.5,shape=18)+
  theme(axis.title.y = element_blank())
Serial_plot




dat_text <- data.frame(
  label = c(label__survvd1,label__survvd2,label__survvd3,label__survvd4,label__survvd5,label__survvd6,
            label__mort1,label__mort2,label__mort3,label__mort4,label__mort5,label__mort6),
  Outcome = c("Survived","Survived","Survived","Survived","Survived","Survived",
              "Mortality","Mortality","Mortality","Mortality","Mortality","Mortality"),
  x     = c(1,2,3,4,5,6,
            1,2,3,4,5,6),
  y     = c(-4,-4,-4,-4,-4,-4,
            -4,-4,-4,-4,-4,-4)
)


serial_plot_wconfidence<-Serial_plot+
  annotate("text", x =1, y=-4,
           label= label__survvd1,
           col="dark blue",
           size=3)+
  annotate("text", x =2, y=-4,
           label= label__survvd2,
           col="dark blue",
           size=3)+
  annotate("text", x =3, y=-4,
           label= label__survvd3,
           col="dark blue",
           size=3)+
  annotate("text", x =4, y=-4,
           label= label__survvd4,
           col="dark blue",
           size=3)+
  annotate("text", x =5, y=-4,
           label= label__survvd5,
           col="dark blue",
           size=3)+
  annotate("text", x =6, y=-4,
           label= label__survvd6,
           col="dark blue",
           size=3)+
  annotate("text", x =1, y=-10,
           label= label__mort1,
           col="dark red",
           size=3)+
  annotate("text", x =2, y=-10,
           label= label__mort2,
           col="dark red",
           size=3)+
  annotate("text", x =3, y=-10,
           label= label__mort3,
           col="dark red",
           size=3)+
  annotate("text", x =4, y=-10,
           label= label__mort4,
           col="dark red",
           size=3)+
  annotate("text", x =5, y=-10,
           label= label__mort5,
           col="dark red",
           size=3)+
  annotate("text", x =6, y=-10,
           label= label__mort6,
           col="dark red",
           size=3)
serial_plot_wconfidence

#change X axis labels 
Name_of_lab1=paste(Name_of_lab,"1")
Name_of_lab2=paste(Name_of_lab,"2")
Name_of_lab3=paste(Name_of_lab,"3")
Name_of_lab4=paste(Name_of_lab,"4")
Name_of_lab5=paste(Name_of_lab,"5")
Name_of_lab6=paste(Name_of_lab,"6")

Final_Plot_serial_lab<-serial_plot_wconfidence+
  scale_x_discrete(labels=c("Lab1" = Name_of_lab1,
                            "Lab2" = Name_of_lab2,
                            "Lab3" = Name_of_lab3,
                            "Lab4" = Name_of_lab4,
                            "Lab5" = Name_of_lab5,
                            "Lab6" = Name_of_lab6
  ))+
  annotate("text", x= 6.5, y=-7, label = "mean and 95% CI",angle="90")


#######show serial lab values result Hb###############

Final_Plot_serial_Hb<- Final_Plot_serial_lab
Final_Plot_serial_Hb 


Average=c(mort_avrg1,
          mort_avrg2,
          mort_avrg3,
          mort_avrg4,
          mort_avrg5,
          mort_avrg6,
          survvd_avrg1,
          survvd_avrg2,
          survvd_avrg3,
          survvd_avrg4,
          survvd_avrg5,
          survvd_avrg6)

LowerCI=c(mort_low1,
          mort_low2,
          mort_low3,
          mort_low4,
          mort_low5,
          mort_low6,
          survvd_low1,
          survvd_low2,
          survvd_low3,
          survvd_low4,
          survvd_low5,
          survvd_low6)

UpperCI=c(mort_upr1,
          mort_upr2,
          mort_upr3,
          mort_upr4,
          mort_upr5,
          mort_upr6,
          survvd_upr1,
          survvd_upr2,
          survvd_upr3,
          survvd_upr4,
          survvd_upr5,
          survvd_upr6)

OUTCOME=c("Mortality",
          "Mortality",
          "Mortality",
          "Mortality",
          "Mortality",
          "Mortality",
          "Survived",
          "Survived",
          "Survived",
          "Survived",
          "Survived",
          "Survived")

Lab_Names=c(Name_of_lab,
            Name_of_lab,
            Name_of_lab,
            Name_of_lab,
            Name_of_lab,
            Name_of_lab,
            Name_of_lab,
            Name_of_lab,
            Name_of_lab,
            Name_of_lab,
            Name_of_lab,
            Name_of_lab)

Day_of_Lab=c(1,2,3,4,5,6,1,2,3,4,5,6)
For_joinpoint_Hb=data.frame(mortAverage=Average,
                             LowerCI=LowerCI,
                             UpperCI=UpperCI,
                             Lab_Names=Lab_Names,
                             Day_of_Lab=Day_of_Lab,
                             OUTCOME=OUTCOME)

qqPlot(long_outremovd_frValid_data_WBCs$lab_value) #currently method is wilcoxon, change to t.test if normalized

###############serial LAB VLAUES MCV#############################

#read dataset
Dataset=read.csv("C:/DOCUMENTS/MY WORK/MaGHALE/Big COVID PROJECT/Article Lab/covid.csv")
attach(Dataset)
Dataset$gender=as.factor(Dataset$gender)
Dataset$Outcome=as.factor(Dataset$Outcome)

# create a dataset with just WBCs
data_Labs <- Dataset[c("Outcome","MCV1", "MCV2", "MCV3","MCV4","MCV5","MCV6")]
Name_of_lab="MCV"

data_Labs$Lab1<-data_Labs$MCV1
data_Labs$Lab2<-data_Labs$MCV2
data_Labs$Lab3<-data_Labs$MCV3
data_Labs$Lab4<-data_Labs$MCV4
data_Labs$Lab5<-data_Labs$MCV5
data_Labs$Lab6<-data_Labs$MCV6

# we use it for creating labels in ggplot
#select patient with at least 4 valid points
data_Labs$labs_NA <- rowSums(is.na(data_Labs))
frValid_data_Labs <- filter(data_Labs, between(labs_NA, 0,2))

# remove outliers 
frValid_data_Labs <- frValid_data_Labs
frValid_data_Labs$Outcome=as.factor(frValid_data_Labs$Outcome)
summary(frValid_data_Labs$Outcome)
###########Hidden Serial lab values MCV###############

f1 <- ave(frValid_data_Labs$Lab1,frValid_data_Labs$Outcome,FUN=scale)
f2 <- ave(frValid_data_Labs$Lab2,frValid_data_Labs$Outcome,FUN=scale)
f3 <- ave(frValid_data_Labs$Lab3,frValid_data_Labs$Outcome,FUN=scale)
f4 <- ave(frValid_data_Labs$Lab4,frValid_data_Labs$Outcome,FUN=scale)
f5 <- ave(frValid_data_Labs$Lab5,frValid_data_Labs$Outcome,FUN=scale)
f6 <- ave(frValid_data_Labs$Lab6,frValid_data_Labs$Outcome,FUN=scale)

frValid_data_Labs$f1=f1
frValid_data_Labs$f2=f2
frValid_data_Labs$f3=f3
frValid_data_Labs$f4=f4
frValid_data_Labs$f5=f5
frValid_data_Labs$f6=f6

outremovd_frValid_data_Labs <- filter (frValid_data_Labs, 
                                       (between(f6, -3,3)| is.na(f6)) & (between(f5, -3,3)| is.na(f5)) &
                                         (between(f4, -3,3)| is.na(f4)) & (between(f3, -3,3)| is.na(f3)) &
                                         (between(f2, -3,3)| is.na(f2)) & (between(f1, -3,3)| is.na(f1)) )


#tamiz kardan data
seriallab<- outremovd_frValid_data_Labs[c("Outcome", "Lab1","Lab2","Lab3","Lab4","Lab5","Lab6")]
seriallab$Outcome=as.factor(seriallab$Outcome)

#impute non_valids va data sete tamiz FAILED: DATA KHAILI BAD TAR HAM SHOD BAD AZ IMPUTE

#seriallab_bamissing<- outremovd_frValid_data_WBCs[c("Outcome", "Lab1","Lab2","Lab3","Lab4","Lab5","Lab6")]
#seriallab_bamissing$Outcome=as.factor(seriallab_bamissing$Outcome)
#imputed_seriallab <- mice(seriallab_bamissing, m=5, maxit = 50, method = 'pmm', seed = 500)
#summary(imputed_seriallab)
#a=imputed_seriallab$imp$Lab1
#seriallab_imputed <- complete(imputed_seriallab,1)
#seriallab <- seriallab_imputed

#long kardan
long_outremovd_frValid_data_WBCs <- seriallab %>%
  pivot_longer(cols=c("Lab1","Lab2","Lab3","Lab4","Lab5","Lab6"),
               names_to = "serial_lab", values_to = "lab_value")


# mohasebe p value beine moteghaiere serial dar 1 dataye paired
long_outremovd_frValid_data_WBCs$serial_lab = as.factor(long_outremovd_frValid_data_WBCs$serial_lab)
compare_means(lab_value ~ serial_lab,  data = long_outremovd_frValid_data_WBCs)

#keshidan shekle p value ha 
my_comparisons <- list( c("Lab1", "Lab2"),  c("Lab1", "Lab3"), c("Lab1", "Lab4"), c("Lab1", "Lab5"),c("Lab1", "Lab6")
                        #c("Lab2", "Lab3"), c("Lab2", "Lab4"), c("Lab2", "Lab5"),c("Lab2", "Lab6"),
                        #c("Lab3", "Lab4"), c("Lab3", "Lab5"),c("Lab3", "Lab6"),
                        #c("Lab4", "Lab5"),c("Lab4", "Lab6"),
                        #c("Lab5", "Lab6")
)

ggplot(long_outremovd_frValid_data_WBCs, aes(x=serial_lab, y=lab_value, group=serial_lab, fill=Outcome))+
  geom_violin()+
  facet_grid(. ~ Outcome)+
  stat_compare_means(comparisons = my_comparisons, label = "p.signif")+ 
  stat_summary(fun.data=mean_sdl, geom="pointrange", color="White", size=0.7,shape=18)





# average and confidence interval of survived
seriallab_survvd = filter(seriallab, Outcome=="Survived")


survvd_avrg1=mean(seriallab_survvd$Lab1,na.rm = TRUE)
survvd_avrg2=mean(seriallab_survvd$Lab2,na.rm = TRUE)
survvd_avrg3=mean(seriallab_survvd$Lab3,na.rm = TRUE)
survvd_avrg4=mean(seriallab_survvd$Lab4,na.rm = TRUE)
survvd_avrg5=mean(seriallab_survvd$Lab5,na.rm = TRUE)
survvd_avrg6=mean(seriallab_survvd$Lab6,na.rm = TRUE)


model1 <- lm(Lab1 ~ 1, seriallab_survvd)
survvd_ci1 <- confint(model1, level=0.95)
survvd_low1 = survvd_ci1[1]
survvd_upr1 = survvd_ci1[2]

model2 <- lm(Lab2 ~ 1, seriallab_survvd)
survvd_ci2 <- confint(model2, level=0.95)
survvd_low2 =  survvd_ci2[1]
survvd_upr2 = survvd_ci2[2]

model3 <- lm(Lab3 ~ 1, seriallab_survvd)
survvd_ci3 <- confint(model3, level=0.95)
survvd_low3 =  survvd_ci3[1]
survvd_upr3 = survvd_ci3[2]

model4 <- lm(Lab4 ~ 1, seriallab_survvd)
survvd_ci4 <- confint(model4, level=0.95)
survvd_low4 =  survvd_ci4[1]
survvd_upr4 = survvd_ci4[2]

model5 <- lm(Lab5 ~ 1, seriallab_survvd)
survvd_ci5 <- confint(model5, level=0.95)
survvd_low5 =  survvd_ci5[1]
survvd_upr5 = survvd_ci5[2]

model6 <- lm(Lab6 ~ 1, seriallab_survvd)
survvd_ci6 <- confint(model6, level=0.95)
survvd_low6 =  survvd_ci6[1]
survvd_upr6 = survvd_ci6[2]


############ average and confidence interval of mortality
seriallab_mort = filter(seriallab, Outcome=="Mortality")


mort_avrg1=mean(seriallab_mort$Lab1,na.rm = TRUE)
mort_avrg2=mean(seriallab_mort$Lab2,na.rm = TRUE)
mort_avrg3=mean(seriallab_mort$Lab3,na.rm = TRUE)
mort_avrg4=mean(seriallab_mort$Lab4,na.rm = TRUE)
mort_avrg5=mean(seriallab_mort$Lab5,na.rm = TRUE)
mort_avrg6=mean(seriallab_mort$Lab6,na.rm = TRUE)


model1 <- lm(Lab1 ~ 1, seriallab_mort)
mort_ci1 <- confint(model1, level=0.95)
mort_low1 = mort_ci1[1]
mort_upr1 = mort_ci1[2]

model2 <- lm(Lab2 ~ 1, seriallab_mort)
mort_ci2 <- confint(model2, level=0.95)
mort_low2 =  mort_ci2[1]
mort_upr2 = mort_ci2[2]

model3 <- lm(Lab3 ~ 1, seriallab_mort)
mort_ci3 <- confint(model3, level=0.95)
mort_low3 =  mort_ci3[1]
mort_upr3 = mort_ci3[2]

model4 <- lm(Lab4 ~ 1, seriallab_mort)
mort_ci4 <- confint(model4, level=0.95)
mort_low4 =  mort_ci4[1]
mort_upr4 = mort_ci4[2]

model5 <- lm(Lab5 ~ 1, seriallab_mort)
mort_ci5 <- confint(model5, level=0.95)
mort_low5 =  mort_ci5[1]
mort_upr5 = mort_ci5[2]

model6 <- lm(Lab6 ~ 1, seriallab_mort)
mort_ci6 <- confint(model6, level=0.95)
mort_low6=  mort_ci6[1]
mort_upr6 = mort_ci6[2]

################# sotre text of mean (lci, uci) for survvied
txt_survvd_avrg1=as.character(round(survvd_avrg1,2))
txt_survvd_low1= as.character(round(survvd_low1,2))
txt_survvd_upr1 = as.character(round(survvd_upr1,2))
label__survvd1=paste(txt_survvd_avrg1,"\n(",txt_survvd_low1,", ",txt_survvd_upr1,")", sep = "")


txt_survvd_avrg2=as.character(round(survvd_avrg2,2))
txt_survvd_low2= as.character(round(survvd_low2,2))
txt_survvd_upr2 = as.character(round(survvd_upr2,2))
label__survvd2=paste(txt_survvd_avrg2,"\n(",txt_survvd_low2,", ",txt_survvd_upr2,")", sep = "")

txt_survvd_avrg3=as.character(round(survvd_avrg3,2))
txt_survvd_low3= as.character(round(survvd_low3,2))
txt_survvd_upr3 = as.character(round(survvd_upr3,2))
label__survvd3=paste(txt_survvd_avrg3,"\n(",txt_survvd_low3,", ",txt_survvd_upr3,")", sep = "")

txt_survvd_avrg4=as.character(round(survvd_avrg4,2))
txt_survvd_low4= as.character(round(survvd_low4,2))
txt_survvd_upr4 = as.character(round(survvd_upr4,2))
label__survvd4=paste(txt_survvd_avrg4,"\n(",txt_survvd_low4,", ",txt_survvd_upr4,")", sep = "")

txt_survvd_avrg5=as.character(round(survvd_avrg5,2))
txt_survvd_low5= as.character(round(survvd_low5,2))
txt_survvd_upr5 = as.character(round(survvd_upr5,2))
label__survvd5=paste(txt_survvd_avrg5,"\n(",txt_survvd_low5,", ",txt_survvd_upr5,")", sep = "")

txt_survvd_avrg6=as.character(round(survvd_avrg6,2))
txt_survvd_low6= as.character(round(survvd_low6,2))
txt_survvd_upr6 = as.character(round(survvd_upr6,2))
label__survvd6=paste(txt_survvd_avrg6,"\n(",txt_survvd_low6,", ",txt_survvd_upr6,")", sep = "")

################## sotre text of mean (lci, uci) for mortality

txt_mort_avrg1=as.character(round(mort_avrg1,2))
txt_mort_low1= as.character(round(mort_low1,2))
txt_mort_upr1 = as.character(round(mort_upr1,2))
label__mort1=paste(txt_mort_avrg1,"\n(",txt_mort_low1,", ",txt_mort_upr1,")", sep = "")

txt_mort_avrg2=as.character(round(mort_avrg2,2))
txt_mort_low2= as.character(round(mort_low2,2))
txt_mort_upr2 = as.character(round(mort_upr2,2))
label__mort2=paste(txt_mort_avrg2,"\n(",txt_mort_low2,", ",txt_mort_upr2,")", sep = "")

txt_mort_avrg3=as.character(round(mort_avrg3,2))
txt_mort_low3= as.character(round(mort_low3,2))
txt_mort_upr3 = as.character(round(mort_upr3,2))
label__mort3=paste(txt_mort_avrg3,"\n(",txt_mort_low3,", ",txt_mort_upr3,")", sep = "")

txt_mort_avrg4=as.character(round(mort_avrg4,2))
txt_mort_low4= as.character(round(mort_low4,2))
txt_mort_upr4 = as.character(round(mort_upr4,2))
label__mort4=paste(txt_mort_avrg4,"\n(",txt_mort_low4,", ",txt_mort_upr4,")", sep = "")

txt_mort_avrg5=as.character(round(mort_avrg5,2))
txt_mort_low5= as.character(round(mort_low5,2))
txt_mort_upr5 = as.character(round(mort_upr5,2))
label__mort5=paste(txt_mort_avrg5,"\n(",txt_mort_low5,", ",txt_mort_upr5,")", sep = "")

txt_mort_avrg6=as.character(round(mort_avrg6,2))
txt_mort_low6= as.character(round(mort_low6,2))
txt_mort_upr6 = as.character(round(mort_upr6,2))
label__mort6=paste(txt_mort_avrg6,"\n(",txt_mort_low6,", ",txt_mort_upr6,")", sep = "")


########### add these mean and Ci as a label of your table

Serial_plot<- ggplot(long_outremovd_frValid_data_WBCs, aes(x=serial_lab, y=lab_value, group=serial_lab, fill=Outcome))+
  geom_violin(alpha=0.3, color="grey")+
  geom_boxplot(notch=TRUE,color="black", size =1, alpha=1,outlier.alpha = 0.0)+
  facet_grid(. ~ Outcome)+
  stat_compare_means(comparisons = my_comparisons,label = "p.signif", method="wilcox.test")+ # or  method="t.test"
  stat_summary(fun.data=mean_sdl, geom="line", color="White", size=0.5,shape=18)+
  theme(axis.title.y = element_blank())
Serial_plot




dat_text <- data.frame(
  label = c(label__survvd1,label__survvd2,label__survvd3,label__survvd4,label__survvd5,label__survvd6,
            label__mort1,label__mort2,label__mort3,label__mort4,label__mort5,label__mort6),
  Outcome = c("Survived","Survived","Survived","Survived","Survived","Survived",
              "Mortality","Mortality","Mortality","Mortality","Mortality","Mortality"),
  x     = c(1,2,3,4,5,6,
            1,2,3,4,5,6),
  y     = c(-4,-4,-4,-4,-4,-4,
            -4,-4,-4,-4,-4,-4)
)


serial_plot_wconfidence<-Serial_plot+
  annotate("text", x =1, y=-4,
           label= label__survvd1,
           col="dark blue",
           size=3)+
  annotate("text", x =2, y=-4,
           label= label__survvd2,
           col="dark blue",
           size=3)+
  annotate("text", x =3, y=-4,
           label= label__survvd3,
           col="dark blue",
           size=3)+
  annotate("text", x =4, y=-4,
           label= label__survvd4,
           col="dark blue",
           size=3)+
  annotate("text", x =5, y=-4,
           label= label__survvd5,
           col="dark blue",
           size=3)+
  annotate("text", x =6, y=-4,
           label= label__survvd6,
           col="dark blue",
           size=3)+
  annotate("text", x =1, y=-10,
           label= label__mort1,
           col="dark red",
           size=3)+
  annotate("text", x =2, y=-10,
           label= label__mort2,
           col="dark red",
           size=3)+
  annotate("text", x =3, y=-10,
           label= label__mort3,
           col="dark red",
           size=3)+
  annotate("text", x =4, y=-10,
           label= label__mort4,
           col="dark red",
           size=3)+
  annotate("text", x =5, y=-10,
           label= label__mort5,
           col="dark red",
           size=3)+
  annotate("text", x =6, y=-10,
           label= label__mort6,
           col="dark red",
           size=3)
serial_plot_wconfidence

#change X axis labels 
Name_of_lab1=paste(Name_of_lab,"1")
Name_of_lab2=paste(Name_of_lab,"2")
Name_of_lab3=paste(Name_of_lab,"3")
Name_of_lab4=paste(Name_of_lab,"4")
Name_of_lab5=paste(Name_of_lab,"5")
Name_of_lab6=paste(Name_of_lab,"6")

Final_Plot_serial_lab<-serial_plot_wconfidence+
  scale_x_discrete(labels=c("Lab1" = Name_of_lab1,
                            "Lab2" = Name_of_lab2,
                            "Lab3" = Name_of_lab3,
                            "Lab4" = Name_of_lab4,
                            "Lab5" = Name_of_lab5,
                            "Lab6" = Name_of_lab6
  ))+
  annotate("text", x= 6.5, y=-7, label = "mean and 95% CI",angle="90")


#######show serial lab values result MCV###############

Final_Plot_serial_MCV<- Final_Plot_serial_lab
Final_Plot_serial_MCV


Average=c(mort_avrg1,
          mort_avrg2,
          mort_avrg3,
          mort_avrg4,
          mort_avrg5,
          mort_avrg6,
          survvd_avrg1,
          survvd_avrg2,
          survvd_avrg3,
          survvd_avrg4,
          survvd_avrg5,
          survvd_avrg6)

LowerCI=c(mort_low1,
          mort_low2,
          mort_low3,
          mort_low4,
          mort_low5,
          mort_low6,
          survvd_low1,
          survvd_low2,
          survvd_low3,
          survvd_low4,
          survvd_low5,
          survvd_low6)

UpperCI=c(mort_upr1,
          mort_upr2,
          mort_upr3,
          mort_upr4,
          mort_upr5,
          mort_upr6,
          survvd_upr1,
          survvd_upr2,
          survvd_upr3,
          survvd_upr4,
          survvd_upr5,
          survvd_upr6)

OUTCOME=c("Mortality",
          "Mortality",
          "Mortality",
          "Mortality",
          "Mortality",
          "Mortality",
          "Survived",
          "Survived",
          "Survived",
          "Survived",
          "Survived",
          "Survived")

Lab_Names=c(Name_of_lab,
            Name_of_lab,
            Name_of_lab,
            Name_of_lab,
            Name_of_lab,
            Name_of_lab,
            Name_of_lab,
            Name_of_lab,
            Name_of_lab,
            Name_of_lab,
            Name_of_lab,
            Name_of_lab)

Day_of_Lab=c(1,2,3,4,5,6,1,2,3,4,5,6)
For_joinpoint_MCV=data.frame(mortAverage=Average,
                            LowerCI=LowerCI,
                            UpperCI=UpperCI,
                            Lab_Names=Lab_Names,
                            Day_of_Lab=Day_of_Lab,
                            OUTCOME=OUTCOME)



##  #############serial LAB VLAUES urea#############################

#read dataset
Dataset=read.csv("C:/DOCUMENTS/MY WORK/MaGHALE/Big COVID PROJECT/Article Lab/covid.csv")
attach(Dataset)
Dataset$gender=as.factor(Dataset$gender)
Dataset$Outcome=as.factor(Dataset$Outcome)

# create a dataset with just WBCs
data_Labs <- Dataset[c("Outcome","All_UREA1", "All_UREA2", "All_UREA3","All_UREA4","All_UREA5","All_UREA6")]
Name_of_lab="Urea"

data_Labs$Lab1<-data_Labs$All_UREA1
data_Labs$Lab2<-data_Labs$All_UREA2
data_Labs$Lab3<-data_Labs$All_UREA3
data_Labs$Lab4<-data_Labs$All_UREA4
data_Labs$Lab5<-data_Labs$All_UREA5
data_Labs$Lab6<-data_Labs$All_UREA6

# we use it for creating labels in ggplot
#select patient with at least 4 valid points
data_Labs$labs_NA <- rowSums(is.na(data_Labs))
frValid_data_Labs <- filter(data_Labs, between(labs_NA, 0,2))

# remove outliers 
frValid_data_Labs <- frValid_data_Labs
frValid_data_Labs$Outcome=as.factor(frValid_data_Labs$Outcome)
summary(frValid_data_Labs$Outcome)
###########Hidden Serial lab values urea###############

f1 <- ave(frValid_data_Labs$Lab1,frValid_data_Labs$Outcome,FUN=scale)
f2 <- ave(frValid_data_Labs$Lab2,frValid_data_Labs$Outcome,FUN=scale)
f3 <- ave(frValid_data_Labs$Lab3,frValid_data_Labs$Outcome,FUN=scale)
f4 <- ave(frValid_data_Labs$Lab4,frValid_data_Labs$Outcome,FUN=scale)
f5 <- ave(frValid_data_Labs$Lab5,frValid_data_Labs$Outcome,FUN=scale)
f6 <- ave(frValid_data_Labs$Lab6,frValid_data_Labs$Outcome,FUN=scale)

frValid_data_Labs$f1=f1
frValid_data_Labs$f2=f2
frValid_data_Labs$f3=f3
frValid_data_Labs$f4=f4
frValid_data_Labs$f5=f5
frValid_data_Labs$f6=f6

outremovd_frValid_data_Labs <- filter (frValid_data_Labs, 
                                       (between(f6, -3,3)| is.na(f6)) & (between(f5, -3,3)| is.na(f5)) &
                                         (between(f4, -3,3)| is.na(f4)) & (between(f3, -3,3)| is.na(f3)) &
                                         (between(f2, -3,3)| is.na(f2)) & (between(f1, -3,3)| is.na(f1)) )


#tamiz kardan data
seriallab<- outremovd_frValid_data_Labs[c("Outcome", "Lab1","Lab2","Lab3","Lab4","Lab5","Lab6")]
seriallab$Outcome=as.factor(seriallab$Outcome)

#impute non_valids va data sete tamiz FAILED: DATA KHAILI BAD TAR HAM SHOD BAD AZ IMPUTE

#seriallab_bamissing<- outremovd_frValid_data_WBCs[c("Outcome", "Lab1","Lab2","Lab3","Lab4","Lab5","Lab6")]
#seriallab_bamissing$Outcome=as.factor(seriallab_bamissing$Outcome)
#imputed_seriallab <- mice(seriallab_bamissing, m=5, maxit = 50, method = 'pmm', seed = 500)
#summary(imputed_seriallab)
#a=imputed_seriallab$imp$Lab1
#seriallab_imputed <- complete(imputed_seriallab,1)
#seriallab <- seriallab_imputed

#long kardan
long_outremovd_frValid_data_WBCs <- seriallab %>%
  pivot_longer(cols=c("Lab1","Lab2","Lab3","Lab4","Lab5","Lab6"),
               names_to = "serial_lab", values_to = "lab_value")


# mohasebe p value beine moteghaiere serial dar 1 dataye paired
long_outremovd_frValid_data_WBCs$serial_lab = as.factor(long_outremovd_frValid_data_WBCs$serial_lab)
compare_means(lab_value ~ serial_lab,  data = long_outremovd_frValid_data_WBCs)

#keshidan shekle p value ha 
my_comparisons <- list( c("Lab1", "Lab2"),  c("Lab1", "Lab3"), c("Lab1", "Lab4"), c("Lab1", "Lab5"),c("Lab1", "Lab6")
                        #c("Lab2", "Lab3"), c("Lab2", "Lab4"), c("Lab2", "Lab5"),c("Lab2", "Lab6"),
                        #c("Lab3", "Lab4"), c("Lab3", "Lab5"),c("Lab3", "Lab6"),
                        #c("Lab4", "Lab5"),c("Lab4", "Lab6"),
                        #c("Lab5", "Lab6")
)

ggplot(long_outremovd_frValid_data_WBCs, aes(x=serial_lab, y=lab_value, group=serial_lab, fill=Outcome))+
  geom_violin()+
  facet_grid(. ~ Outcome)+
  stat_compare_means(comparisons = my_comparisons, label = "p.signif")+ 
  stat_summary(fun.data=mean_sdl, geom="pointrange", color="White", size=0.7,shape=18)





# average and confidence interval of survived
seriallab_survvd = filter(seriallab, Outcome=="Survived")


survvd_avrg1=mean(seriallab_survvd$Lab1,na.rm = TRUE)
survvd_avrg2=mean(seriallab_survvd$Lab2,na.rm = TRUE)
survvd_avrg3=mean(seriallab_survvd$Lab3,na.rm = TRUE)
survvd_avrg4=mean(seriallab_survvd$Lab4,na.rm = TRUE)
survvd_avrg5=mean(seriallab_survvd$Lab5,na.rm = TRUE)
survvd_avrg6=mean(seriallab_survvd$Lab6,na.rm = TRUE)


model1 <- lm(Lab1 ~ 1, seriallab_survvd)
survvd_ci1 <- confint(model1, level=0.95)
survvd_low1 = survvd_ci1[1]
survvd_upr1 = survvd_ci1[2]

model2 <- lm(Lab2 ~ 1, seriallab_survvd)
survvd_ci2 <- confint(model2, level=0.95)
survvd_low2 =  survvd_ci2[1]
survvd_upr2 = survvd_ci2[2]

model3 <- lm(Lab3 ~ 1, seriallab_survvd)
survvd_ci3 <- confint(model3, level=0.95)
survvd_low3 =  survvd_ci3[1]
survvd_upr3 = survvd_ci3[2]

model4 <- lm(Lab4 ~ 1, seriallab_survvd)
survvd_ci4 <- confint(model4, level=0.95)
survvd_low4 =  survvd_ci4[1]
survvd_upr4 = survvd_ci4[2]

model5 <- lm(Lab5 ~ 1, seriallab_survvd)
survvd_ci5 <- confint(model5, level=0.95)
survvd_low5 =  survvd_ci5[1]
survvd_upr5 = survvd_ci5[2]

model6 <- lm(Lab6 ~ 1, seriallab_survvd)
survvd_ci6 <- confint(model6, level=0.95)
survvd_low6 =  survvd_ci6[1]
survvd_upr6 = survvd_ci6[2]


############ average and confidence interval of mortality
seriallab_mort = filter(seriallab, Outcome=="Mortality")


mort_avrg1=mean(seriallab_mort$Lab1,na.rm = TRUE)
mort_avrg2=mean(seriallab_mort$Lab2,na.rm = TRUE)
mort_avrg3=mean(seriallab_mort$Lab3,na.rm = TRUE)
mort_avrg4=mean(seriallab_mort$Lab4,na.rm = TRUE)
mort_avrg5=mean(seriallab_mort$Lab5,na.rm = TRUE)
mort_avrg6=mean(seriallab_mort$Lab6,na.rm = TRUE)


model1 <- lm(Lab1 ~ 1, seriallab_mort)
mort_ci1 <- confint(model1, level=0.95)
mort_low1 = mort_ci1[1]
mort_upr1 = mort_ci1[2]

model2 <- lm(Lab2 ~ 1, seriallab_mort)
mort_ci2 <- confint(model2, level=0.95)
mort_low2 =  mort_ci2[1]
mort_upr2 = mort_ci2[2]

model3 <- lm(Lab3 ~ 1, seriallab_mort)
mort_ci3 <- confint(model3, level=0.95)
mort_low3 =  mort_ci3[1]
mort_upr3 = mort_ci3[2]

model4 <- lm(Lab4 ~ 1, seriallab_mort)
mort_ci4 <- confint(model4, level=0.95)
mort_low4 =  mort_ci4[1]
mort_upr4 = mort_ci4[2]

model5 <- lm(Lab5 ~ 1, seriallab_mort)
mort_ci5 <- confint(model5, level=0.95)
mort_low5 =  mort_ci5[1]
mort_upr5 = mort_ci5[2]

model6 <- lm(Lab6 ~ 1, seriallab_mort)
mort_ci6 <- confint(model6, level=0.95)
mort_low6=  mort_ci6[1]
mort_upr6 = mort_ci6[2]

################# sotre text of mean (lci, uci) for survvied
txt_survvd_avrg1=as.character(round(survvd_avrg1,2))
txt_survvd_low1= as.character(round(survvd_low1,2))
txt_survvd_upr1 = as.character(round(survvd_upr1,2))
label__survvd1=paste(txt_survvd_avrg1,"\n(",txt_survvd_low1,", ",txt_survvd_upr1,")", sep = "")


txt_survvd_avrg2=as.character(round(survvd_avrg2,2))
txt_survvd_low2= as.character(round(survvd_low2,2))
txt_survvd_upr2 = as.character(round(survvd_upr2,2))
label__survvd2=paste(txt_survvd_avrg2,"\n(",txt_survvd_low2,", ",txt_survvd_upr2,")", sep = "")

txt_survvd_avrg3=as.character(round(survvd_avrg3,2))
txt_survvd_low3= as.character(round(survvd_low3,2))
txt_survvd_upr3 = as.character(round(survvd_upr3,2))
label__survvd3=paste(txt_survvd_avrg3,"\n(",txt_survvd_low3,", ",txt_survvd_upr3,")", sep = "")

txt_survvd_avrg4=as.character(round(survvd_avrg4,2))
txt_survvd_low4= as.character(round(survvd_low4,2))
txt_survvd_upr4 = as.character(round(survvd_upr4,2))
label__survvd4=paste(txt_survvd_avrg4,"\n(",txt_survvd_low4,", ",txt_survvd_upr4,")", sep = "")

txt_survvd_avrg5=as.character(round(survvd_avrg5,2))
txt_survvd_low5= as.character(round(survvd_low5,2))
txt_survvd_upr5 = as.character(round(survvd_upr5,2))
label__survvd5=paste(txt_survvd_avrg5,"\n(",txt_survvd_low5,", ",txt_survvd_upr5,")", sep = "")

txt_survvd_avrg6=as.character(round(survvd_avrg6,2))
txt_survvd_low6= as.character(round(survvd_low6,2))
txt_survvd_upr6 = as.character(round(survvd_upr6,2))
label__survvd6=paste(txt_survvd_avrg6,"\n(",txt_survvd_low6,", ",txt_survvd_upr6,")", sep = "")

################## sotre text of mean (lci, uci) for mortality

txt_mort_avrg1=as.character(round(mort_avrg1,2))
txt_mort_low1= as.character(round(mort_low1,2))
txt_mort_upr1 = as.character(round(mort_upr1,2))
label__mort1=paste(txt_mort_avrg1,"\n(",txt_mort_low1,", ",txt_mort_upr1,")", sep = "")

txt_mort_avrg2=as.character(round(mort_avrg2,2))
txt_mort_low2= as.character(round(mort_low2,2))
txt_mort_upr2 = as.character(round(mort_upr2,2))
label__mort2=paste(txt_mort_avrg2,"\n(",txt_mort_low2,", ",txt_mort_upr2,")", sep = "")

txt_mort_avrg3=as.character(round(mort_avrg3,2))
txt_mort_low3= as.character(round(mort_low3,2))
txt_mort_upr3 = as.character(round(mort_upr3,2))
label__mort3=paste(txt_mort_avrg3,"\n(",txt_mort_low3,", ",txt_mort_upr3,")", sep = "")

txt_mort_avrg4=as.character(round(mort_avrg4,2))
txt_mort_low4= as.character(round(mort_low4,2))
txt_mort_upr4 = as.character(round(mort_upr4,2))
label__mort4=paste(txt_mort_avrg4,"\n(",txt_mort_low4,", ",txt_mort_upr4,")", sep = "")

txt_mort_avrg5=as.character(round(mort_avrg5,2))
txt_mort_low5= as.character(round(mort_low5,2))
txt_mort_upr5 = as.character(round(mort_upr5,2))
label__mort5=paste(txt_mort_avrg5,"\n(",txt_mort_low5,", ",txt_mort_upr5,")", sep = "")

txt_mort_avrg6=as.character(round(mort_avrg6,2))
txt_mort_low6= as.character(round(mort_low6,2))
txt_mort_upr6 = as.character(round(mort_upr6,2))
label__mort6=paste(txt_mort_avrg6,"\n(",txt_mort_low6,", ",txt_mort_upr6,")", sep = "")


########### add these mean and Ci as a label of your table

Serial_plot<- ggplot(long_outremovd_frValid_data_WBCs, aes(x=serial_lab, y=lab_value, group=serial_lab, fill=Outcome))+
  geom_violin(alpha=0.3, color="grey")+
  geom_boxplot(notch=TRUE,color="black", size =1, alpha=1,outlier.alpha = 0.0)+
  facet_grid(. ~ Outcome)+
  stat_compare_means(comparisons = my_comparisons,label = "p.signif", method="wilcox.test")+ # or  method="t.test"
  stat_summary(fun.data=mean_sdl, geom="line", color="White", size=0.5,shape=18)+
  theme(axis.title.y = element_blank())
Serial_plot


dat_text <- data.frame(
  label = c(label__survvd1,label__survvd2,label__survvd3,label__survvd4,label__survvd5,label__survvd6,
            label__mort1,label__mort2,label__mort3,label__mort4,label__mort5,label__mort6),
  Outcome = c("Survived","Survived","Survived","Survived","Survived","Survived",
              "Mortality","Mortality","Mortality","Mortality","Mortality","Mortality"),
  x     = c(1,2,3,4,5,6,
            1,2,3,4,5,6),
  y     = c(-4,-4,-4,-4,-4,-4,
            -4,-4,-4,-4,-4,-4)
)


serial_plot_wconfidence<-Serial_plot+
  annotate("text", x =1, y=-4,
           label= label__survvd1,
           col="dark blue",
           size=3)+
  annotate("text", x =2, y=-4,
           label= label__survvd2,
           col="dark blue",
           size=3)+
  annotate("text", x =3, y=-4,
           label= label__survvd3,
           col="dark blue",
           size=3)+
  annotate("text", x =4, y=-4,
           label= label__survvd4,
           col="dark blue",
           size=3)+
  annotate("text", x =5, y=-4,
           label= label__survvd5,
           col="dark blue",
           size=3)+
  annotate("text", x =6, y=-4,
           label= label__survvd6,
           col="dark blue",
           size=3)+
  annotate("text", x =1, y=-10,
           label= label__mort1,
           col="dark red",
           size=3)+
  annotate("text", x =2, y=-10,
           label= label__mort2,
           col="dark red",
           size=3)+
  annotate("text", x =3, y=-10,
           label= label__mort3,
           col="dark red",
           size=3)+
  annotate("text", x =4, y=-10,
           label= label__mort4,
           col="dark red",
           size=3)+
  annotate("text", x =5, y=-10,
           label= label__mort5,
           col="dark red",
           size=3)+
  annotate("text", x =6, y=-10,
           label= label__mort6,
           col="dark red",
           size=3)
serial_plot_wconfidence

#change X axis labels 
Name_of_lab1=paste(Name_of_lab,"1")
Name_of_lab2=paste(Name_of_lab,"2")
Name_of_lab3=paste(Name_of_lab,"3")
Name_of_lab4=paste(Name_of_lab,"4")
Name_of_lab5=paste(Name_of_lab,"5")
Name_of_lab6=paste(Name_of_lab,"6")

Final_Plot_serial_lab<-serial_plot_wconfidence+
  scale_x_discrete(labels=c("Lab1" = Name_of_lab1,
                            "Lab2" = Name_of_lab2,
                            "Lab3" = Name_of_lab3,
                            "Lab4" = Name_of_lab4,
                            "Lab5" = Name_of_lab5,
                            "Lab6" = Name_of_lab6
  ))+
  annotate("text", x= 6.5, y=-7, label = "mean and 95% CI",angle="90")


#######show serial lab values result urea###############

Final_Plot_serial_urea<- Final_Plot_serial_lab
Final_Plot_serial_urea


Average=c(mort_avrg1,
          mort_avrg2,
          mort_avrg3,
          mort_avrg4,
          mort_avrg5,
          mort_avrg6,
          survvd_avrg1,
          survvd_avrg2,
          survvd_avrg3,
          survvd_avrg4,
          survvd_avrg5,
          survvd_avrg6)

LowerCI=c(mort_low1,
          mort_low2,
          mort_low3,
          mort_low4,
          mort_low5,
          mort_low6,
          survvd_low1,
          survvd_low2,
          survvd_low3,
          survvd_low4,
          survvd_low5,
          survvd_low6)

UpperCI=c(mort_upr1,
          mort_upr2,
          mort_upr3,
          mort_upr4,
          mort_upr5,
          mort_upr6,
          survvd_upr1,
          survvd_upr2,
          survvd_upr3,
          survvd_upr4,
          survvd_upr5,
          survvd_upr6)

OUTCOME=c("Mortality",
          "Mortality",
          "Mortality",
          "Mortality",
          "Mortality",
          "Mortality",
          "Survived",
          "Survived",
          "Survived",
          "Survived",
          "Survived",
          "Survived")

Lab_Names=c(Name_of_lab,
            Name_of_lab,
            Name_of_lab,
            Name_of_lab,
            Name_of_lab,
            Name_of_lab,
            Name_of_lab,
            Name_of_lab,
            Name_of_lab,
            Name_of_lab,
            Name_of_lab,
            Name_of_lab)

Day_of_Lab=c(1,2,3,4,5,6,1,2,3,4,5,6)
For_joinpoint_urea=data.frame(mortAverage=Average,
                             LowerCI=LowerCI,
                             UpperCI=UpperCI,
                             Lab_Names=Lab_Names,
                             Day_of_Lab=Day_of_Lab,
                             OUTCOME=OUTCOME)

qqPlot(long_outremovd_frValid_data_WBCs$lab_value) #currently method is wilcoxon, change to t.test if normalized


##  #############serial LAB VLAUES cr#############################

#read dataset
Dataset=read.csv("C:/DOCUMENTS/MY WORK/MaGHALE/Big COVID PROJECT/Article Lab/covid.csv")
attach(Dataset)
Dataset$gender=as.factor(Dataset$gender)
Dataset$Outcome=as.factor(Dataset$Outcome)

# create a dataset with just WBCs
data_Labs <- Dataset[c("Outcome","CR1", "CR2", "CR3","CR4","CR5","CR6")]
Name_of_lab="creatinine"

data_Labs$Lab1<-data_Labs$CR1
data_Labs$Lab2<-data_Labs$CR2
data_Labs$Lab3<-data_Labs$CR3
data_Labs$Lab4<-data_Labs$CR4
data_Labs$Lab5<-data_Labs$CR5
data_Labs$Lab6<-data_Labs$CR6

# we use it for creating labels in ggplot
#select patient with at least 4 valid points
data_Labs$labs_NA <- rowSums(is.na(data_Labs))
frValid_data_Labs <- filter(data_Labs, between(labs_NA, 0,2))

# remove outliers 
frValid_data_Labs <- frValid_data_Labs
frValid_data_Labs$Outcome=as.factor(frValid_data_Labs$Outcome)
summary(frValid_data_Labs$Outcome)

###########Hidden Serial lab values cr###############

f1 <- ave(frValid_data_Labs$Lab1,frValid_data_Labs$Outcome,FUN=scale)
f2 <- ave(frValid_data_Labs$Lab2,frValid_data_Labs$Outcome,FUN=scale)
f3 <- ave(frValid_data_Labs$Lab3,frValid_data_Labs$Outcome,FUN=scale)
f4 <- ave(frValid_data_Labs$Lab4,frValid_data_Labs$Outcome,FUN=scale)
f5 <- ave(frValid_data_Labs$Lab5,frValid_data_Labs$Outcome,FUN=scale)
f6 <- ave(frValid_data_Labs$Lab6,frValid_data_Labs$Outcome,FUN=scale)

frValid_data_Labs$f1=f1
frValid_data_Labs$f2=f2
frValid_data_Labs$f3=f3
frValid_data_Labs$f4=f4
frValid_data_Labs$f5=f5
frValid_data_Labs$f6=f6

outremovd_frValid_data_Labs <- filter (frValid_data_Labs, 
                                       (between(f6, -3,3)| is.na(f6)) & (between(f5, -3,3)| is.na(f5)) &
                                         (between(f4, -3,3)| is.na(f4)) & (between(f3, -3,3)| is.na(f3)) &
                                         (between(f2, -3,3)| is.na(f2)) & (between(f1, -3,3)| is.na(f1)) )


#tamiz kardan data
seriallab<- outremovd_frValid_data_Labs[c("Outcome", "Lab1","Lab2","Lab3","Lab4","Lab5","Lab6")]
seriallab$Outcome=as.factor(seriallab$Outcome)

#impute non_valids va data sete tamiz FAILED: DATA KHAILI BAD TAR HAM SHOD BAD AZ IMPUTE

#seriallab_bamissing<- outremovd_frValid_data_WBCs[c("Outcome", "Lab1","Lab2","Lab3","Lab4","Lab5","Lab6")]
#seriallab_bamissing$Outcome=as.factor(seriallab_bamissing$Outcome)
#imputed_seriallab <- mice(seriallab_bamissing, m=5, maxit = 50, method = 'pmm', seed = 500)
#summary(imputed_seriallab)
#a=imputed_seriallab$imp$Lab1
#seriallab_imputed <- complete(imputed_seriallab,1)
#seriallab <- seriallab_imputed

#long kardan
long_outremovd_frValid_data_WBCs <- seriallab %>%
  pivot_longer(cols=c("Lab1","Lab2","Lab3","Lab4","Lab5","Lab6"),
               names_to = "serial_lab", values_to = "lab_value")


# mohasebe p value beine moteghaiere serial dar 1 dataye paired
long_outremovd_frValid_data_WBCs$serial_lab = as.factor(long_outremovd_frValid_data_WBCs$serial_lab)
compare_means(lab_value ~ serial_lab,  data = long_outremovd_frValid_data_WBCs)

#keshidan shekle p value ha 
my_comparisons <- list( c("Lab1", "Lab2"),  c("Lab1", "Lab3"), c("Lab1", "Lab4"), c("Lab1", "Lab5"),c("Lab1", "Lab6")
                        #c("Lab2", "Lab3"), c("Lab2", "Lab4"), c("Lab2", "Lab5"),c("Lab2", "Lab6"),
                        #c("Lab3", "Lab4"), c("Lab3", "Lab5"),c("Lab3", "Lab6"),
                        #c("Lab4", "Lab5"),c("Lab4", "Lab6"),
                        #c("Lab5", "Lab6")
)

ggplot(long_outremovd_frValid_data_WBCs, aes(x=serial_lab, y=lab_value, group=serial_lab, fill=Outcome))+
  geom_violin()+
  facet_grid(. ~ Outcome)+
  stat_compare_means(comparisons = my_comparisons, label = "p.signif")+ 
  stat_summary(fun.data=mean_sdl, geom="pointrange", color="White", size=0.7,shape=18)





# average and confidence interval of survived
seriallab_survvd = filter(seriallab, Outcome=="Survived")


survvd_avrg1=mean(seriallab_survvd$Lab1,na.rm = TRUE)
survvd_avrg2=mean(seriallab_survvd$Lab2,na.rm = TRUE)
survvd_avrg3=mean(seriallab_survvd$Lab3,na.rm = TRUE)
survvd_avrg4=mean(seriallab_survvd$Lab4,na.rm = TRUE)
survvd_avrg5=mean(seriallab_survvd$Lab5,na.rm = TRUE)
survvd_avrg6=mean(seriallab_survvd$Lab6,na.rm = TRUE)


model1 <- lm(Lab1 ~ 1, seriallab_survvd)
survvd_ci1 <- confint(model1, level=0.95)
survvd_low1 = survvd_ci1[1]
survvd_upr1 = survvd_ci1[2]

model2 <- lm(Lab2 ~ 1, seriallab_survvd)
survvd_ci2 <- confint(model2, level=0.95)
survvd_low2 =  survvd_ci2[1]
survvd_upr2 = survvd_ci2[2]

model3 <- lm(Lab3 ~ 1, seriallab_survvd)
survvd_ci3 <- confint(model3, level=0.95)
survvd_low3 =  survvd_ci3[1]
survvd_upr3 = survvd_ci3[2]

model4 <- lm(Lab4 ~ 1, seriallab_survvd)
survvd_ci4 <- confint(model4, level=0.95)
survvd_low4 =  survvd_ci4[1]
survvd_upr4 = survvd_ci4[2]

model5 <- lm(Lab5 ~ 1, seriallab_survvd)
survvd_ci5 <- confint(model5, level=0.95)
survvd_low5 =  survvd_ci5[1]
survvd_upr5 = survvd_ci5[2]

model6 <- lm(Lab6 ~ 1, seriallab_survvd)
survvd_ci6 <- confint(model6, level=0.95)
survvd_low6 =  survvd_ci6[1]
survvd_upr6 = survvd_ci6[2]


############ average and confidence interval of mortality
seriallab_mort = filter(seriallab, Outcome=="Mortality")


mort_avrg1=mean(seriallab_mort$Lab1,na.rm = TRUE)
mort_avrg2=mean(seriallab_mort$Lab2,na.rm = TRUE)
mort_avrg3=mean(seriallab_mort$Lab3,na.rm = TRUE)
mort_avrg4=mean(seriallab_mort$Lab4,na.rm = TRUE)
mort_avrg5=mean(seriallab_mort$Lab5,na.rm = TRUE)
mort_avrg6=mean(seriallab_mort$Lab6,na.rm = TRUE)


model1 <- lm(Lab1 ~ 1, seriallab_mort)
mort_ci1 <- confint(model1, level=0.95)
mort_low1 = mort_ci1[1]
mort_upr1 = mort_ci1[2]

model2 <- lm(Lab2 ~ 1, seriallab_mort)
mort_ci2 <- confint(model2, level=0.95)
mort_low2 =  mort_ci2[1]
mort_upr2 = mort_ci2[2]

model3 <- lm(Lab3 ~ 1, seriallab_mort)
mort_ci3 <- confint(model3, level=0.95)
mort_low3 =  mort_ci3[1]
mort_upr3 = mort_ci3[2]

model4 <- lm(Lab4 ~ 1, seriallab_mort)
mort_ci4 <- confint(model4, level=0.95)
mort_low4 =  mort_ci4[1]
mort_upr4 = mort_ci4[2]

model5 <- lm(Lab5 ~ 1, seriallab_mort)
mort_ci5 <- confint(model5, level=0.95)
mort_low5 =  mort_ci5[1]
mort_upr5 = mort_ci5[2]

model6 <- lm(Lab6 ~ 1, seriallab_mort)
mort_ci6 <- confint(model6, level=0.95)
mort_low6=  mort_ci6[1]
mort_upr6 = mort_ci6[2]

################# sotre text of mean (lci, uci) for survvied
txt_survvd_avrg1=as.character(round(survvd_avrg1,2))
txt_survvd_low1= as.character(round(survvd_low1,2))
txt_survvd_upr1 = as.character(round(survvd_upr1,2))
label__survvd1=paste(txt_survvd_avrg1,"\n(",txt_survvd_low1,", ",txt_survvd_upr1,")", sep = "")


txt_survvd_avrg2=as.character(round(survvd_avrg2,2))
txt_survvd_low2= as.character(round(survvd_low2,2))
txt_survvd_upr2 = as.character(round(survvd_upr2,2))
label__survvd2=paste(txt_survvd_avrg2,"\n(",txt_survvd_low2,", ",txt_survvd_upr2,")", sep = "")

txt_survvd_avrg3=as.character(round(survvd_avrg3,2))
txt_survvd_low3= as.character(round(survvd_low3,2))
txt_survvd_upr3 = as.character(round(survvd_upr3,2))
label__survvd3=paste(txt_survvd_avrg3,"\n(",txt_survvd_low3,", ",txt_survvd_upr3,")", sep = "")

txt_survvd_avrg4=as.character(round(survvd_avrg4,2))
txt_survvd_low4= as.character(round(survvd_low4,2))
txt_survvd_upr4 = as.character(round(survvd_upr4,2))
label__survvd4=paste(txt_survvd_avrg4,"\n(",txt_survvd_low4,", ",txt_survvd_upr4,")", sep = "")

txt_survvd_avrg5=as.character(round(survvd_avrg5,2))
txt_survvd_low5= as.character(round(survvd_low5,2))
txt_survvd_upr5 = as.character(round(survvd_upr5,2))
label__survvd5=paste(txt_survvd_avrg5,"\n(",txt_survvd_low5,", ",txt_survvd_upr5,")", sep = "")

txt_survvd_avrg6=as.character(round(survvd_avrg6,2))
txt_survvd_low6= as.character(round(survvd_low6,2))
txt_survvd_upr6 = as.character(round(survvd_upr6,2))
label__survvd6=paste(txt_survvd_avrg6,"\n(",txt_survvd_low6,", ",txt_survvd_upr6,")", sep = "")

################## sotre text of mean (lci, uci) for mortality

txt_mort_avrg1=as.character(round(mort_avrg1,2))
txt_mort_low1= as.character(round(mort_low1,2))
txt_mort_upr1 = as.character(round(mort_upr1,2))
label__mort1=paste(txt_mort_avrg1,"\n(",txt_mort_low1,", ",txt_mort_upr1,")", sep = "")

txt_mort_avrg2=as.character(round(mort_avrg2,2))
txt_mort_low2= as.character(round(mort_low2,2))
txt_mort_upr2 = as.character(round(mort_upr2,2))
label__mort2=paste(txt_mort_avrg2,"\n(",txt_mort_low2,", ",txt_mort_upr2,")", sep = "")

txt_mort_avrg3=as.character(round(mort_avrg3,2))
txt_mort_low3= as.character(round(mort_low3,2))
txt_mort_upr3 = as.character(round(mort_upr3,2))
label__mort3=paste(txt_mort_avrg3,"\n(",txt_mort_low3,", ",txt_mort_upr3,")", sep = "")

txt_mort_avrg4=as.character(round(mort_avrg4,2))
txt_mort_low4= as.character(round(mort_low4,2))
txt_mort_upr4 = as.character(round(mort_upr4,2))
label__mort4=paste(txt_mort_avrg4,"\n(",txt_mort_low4,", ",txt_mort_upr4,")", sep = "")

txt_mort_avrg5=as.character(round(mort_avrg5,2))
txt_mort_low5= as.character(round(mort_low5,2))
txt_mort_upr5 = as.character(round(mort_upr5,2))
label__mort5=paste(txt_mort_avrg5,"\n(",txt_mort_low5,", ",txt_mort_upr5,")", sep = "")

txt_mort_avrg6=as.character(round(mort_avrg6,2))
txt_mort_low6= as.character(round(mort_low6,2))
txt_mort_upr6 = as.character(round(mort_upr6,2))
label__mort6=paste(txt_mort_avrg6,"\n(",txt_mort_low6,", ",txt_mort_upr6,")", sep = "")


########### add these mean and Ci as a label of your table

Serial_plot<- ggplot(long_outremovd_frValid_data_WBCs, aes(x=serial_lab, y=lab_value, group=serial_lab, fill=Outcome))+
  geom_violin(alpha=0.3, color="grey")+
  geom_boxplot(notch=TRUE,color="black", size =1, alpha=1,outlier.alpha = 0.0)+
  facet_grid(. ~ Outcome)+
  stat_compare_means(comparisons = my_comparisons,label = "p.signif", method="wilcox.test")+ # or  method="t.test"
  stat_summary(fun.data=mean_sdl, geom="line", color="White", size=0.5,shape=18)+
  theme(axis.title.y = element_blank())
Serial_plot



dat_text <- data.frame(
  label = c(label__survvd1,label__survvd2,label__survvd3,label__survvd4,label__survvd5,label__survvd6,
            label__mort1,label__mort2,label__mort3,label__mort4,label__mort5,label__mort6),
  Outcome = c("Survived","Survived","Survived","Survived","Survived","Survived",
              "Mortality","Mortality","Mortality","Mortality","Mortality","Mortality"),
  x     = c(1,2,3,4,5,6,
            1,2,3,4,5,6),
  y     = c(-4,-4,-4,-4,-4,-4,
            -4,-4,-4,-4,-4,-4)
)


serial_plot_wconfidence<-Serial_plot+
  annotate("text", x =1, y=-4,
           label= label__survvd1,
           col="dark blue",
           size=3)+
  annotate("text", x =2, y=-4,
           label= label__survvd2,
           col="dark blue",
           size=3)+
  annotate("text", x =3, y=-4,
           label= label__survvd3,
           col="dark blue",
           size=3)+
  annotate("text", x =4, y=-4,
           label= label__survvd4,
           col="dark blue",
           size=3)+
  annotate("text", x =5, y=-4,
           label= label__survvd5,
           col="dark blue",
           size=3)+
  annotate("text", x =6, y=-4,
           label= label__survvd6,
           col="dark blue",
           size=3)+
  annotate("text", x =1, y=-10,
           label= label__mort1,
           col="dark red",
           size=3)+
  annotate("text", x =2, y=-10,
           label= label__mort2,
           col="dark red",
           size=3)+
  annotate("text", x =3, y=-10,
           label= label__mort3,
           col="dark red",
           size=3)+
  annotate("text", x =4, y=-10,
           label= label__mort4,
           col="dark red",
           size=3)+
  annotate("text", x =5, y=-10,
           label= label__mort5,
           col="dark red",
           size=3)+
  annotate("text", x =6, y=-10,
           label= label__mort6,
           col="dark red",
           size=3)
serial_plot_wconfidence

#change X axis labels 
Name_of_lab1=paste(Name_of_lab,"1")
Name_of_lab2=paste(Name_of_lab,"2")
Name_of_lab3=paste(Name_of_lab,"3")
Name_of_lab4=paste(Name_of_lab,"4")
Name_of_lab5=paste(Name_of_lab,"5")
Name_of_lab6=paste(Name_of_lab,"6")

Final_Plot_serial_lab<-serial_plot_wconfidence+
  scale_x_discrete(labels=c("Lab1" = Name_of_lab1,
                            "Lab2" = Name_of_lab2,
                            "Lab3" = Name_of_lab3,
                            "Lab4" = Name_of_lab4,
                            "Lab5" = Name_of_lab5,
                            "Lab6" = Name_of_lab6
  ))+
  annotate("text", x= 6.5, y=-7, label = "mean and 95% CI",angle="90")


#######show serial lab values result cr###############

Final_Plot_serial_cr<- Final_Plot_serial_lab
Final_Plot_serial_cr


Average=c(mort_avrg1,
          mort_avrg2,
          mort_avrg3,
          mort_avrg4,
          mort_avrg5,
          mort_avrg6,
          survvd_avrg1,
          survvd_avrg2,
          survvd_avrg3,
          survvd_avrg4,
          survvd_avrg5,
          survvd_avrg6)

LowerCI=c(mort_low1,
          mort_low2,
          mort_low3,
          mort_low4,
          mort_low5,
          mort_low6,
          survvd_low1,
          survvd_low2,
          survvd_low3,
          survvd_low4,
          survvd_low5,
          survvd_low6)

UpperCI=c(mort_upr1,
          mort_upr2,
          mort_upr3,
          mort_upr4,
          mort_upr5,
          mort_upr6,
          survvd_upr1,
          survvd_upr2,
          survvd_upr3,
          survvd_upr4,
          survvd_upr5,
          survvd_upr6)

OUTCOME=c("Mortality",
          "Mortality",
          "Mortality",
          "Mortality",
          "Mortality",
          "Mortality",
          "Survived",
          "Survived",
          "Survived",
          "Survived",
          "Survived",
          "Survived")

Lab_Names=c(Name_of_lab,
            Name_of_lab,
            Name_of_lab,
            Name_of_lab,
            Name_of_lab,
            Name_of_lab,
            Name_of_lab,
            Name_of_lab,
            Name_of_lab,
            Name_of_lab,
            Name_of_lab,
            Name_of_lab)

Day_of_Lab=c(1,2,3,4,5,6,1,2,3,4,5,6)
For_joinpoint_Cr=data.frame(mortAverage=Average,
                             LowerCI=LowerCI,
                             UpperCI=UpperCI,
                             Lab_Names=Lab_Names,
                             Day_of_Lab=Day_of_Lab,
                             OUTCOME=OUTCOME)


  qqPlot(long_outremovd_frValid_data_WBCs$lab_value) #currently method is wilcoxon, change to t.test if normalized



##  #############serial LAB VLAUES AST#############################

#read dataset
Dataset=read.csv("C:/DOCUMENTS/MY WORK/MaGHALE/Big COVID PROJECT/Article Lab/covid.csv")
attach(Dataset)
Dataset$gender=as.factor(Dataset$gender)
Dataset$Outcome=as.factor(Dataset$Outcome)

# create a dataset with just WBCs
data_Labs <- Dataset[c("Outcome","AST1", "AST2", "AST3","AST4","AST5","AST6")]
Name_of_lab="AST"

data_Labs$Lab1<-data_Labs$AST1
data_Labs$Lab2<-data_Labs$AST2
data_Labs$Lab3<-data_Labs$AST3
data_Labs$Lab4<-data_Labs$AST4
data_Labs$Lab5<-data_Labs$AST5
data_Labs$Lab6<-data_Labs$AST6

# we use it for creating labels in ggplot
#select patient with at least 4 valid points
data_Labs$labs_NA <- rowSums(is.na(data_Labs))
frValid_data_Labs <- filter(data_Labs, between(labs_NA, 0,2))

# remove outliers 
frValid_data_Labs <- frValid_data_Labs
frValid_data_Labs$Outcome=as.factor(frValid_data_Labs$Outcome)
summary(frValid_data_Labs$Outcome)
###########Hidden Serial lab values AST###############

f1 <- ave(frValid_data_Labs$Lab1,frValid_data_Labs$Outcome,FUN=scale)
f2 <- ave(frValid_data_Labs$Lab2,frValid_data_Labs$Outcome,FUN=scale)
f3 <- ave(frValid_data_Labs$Lab3,frValid_data_Labs$Outcome,FUN=scale)
f4 <- ave(frValid_data_Labs$Lab4,frValid_data_Labs$Outcome,FUN=scale)
f5 <- ave(frValid_data_Labs$Lab5,frValid_data_Labs$Outcome,FUN=scale)
f6 <- ave(frValid_data_Labs$Lab6,frValid_data_Labs$Outcome,FUN=scale)

frValid_data_Labs$f1=f1
frValid_data_Labs$f2=f2
frValid_data_Labs$f3=f3
frValid_data_Labs$f4=f4
frValid_data_Labs$f5=f5
frValid_data_Labs$f6=f6

outremovd_frValid_data_Labs <- filter (frValid_data_Labs, 
                                       (between(f6, -3,3)| is.na(f6)) & (between(f5, -3,3)| is.na(f5)) &
                                         (between(f4, -3,3)| is.na(f4)) & (between(f3, -3,3)| is.na(f3)) &
                                         (between(f2, -3,3)| is.na(f2)) & (between(f1, -3,3)| is.na(f1)) )


#tamiz kardan data
seriallab<- outremovd_frValid_data_Labs[c("Outcome", "Lab1","Lab2","Lab3","Lab4","Lab5","Lab6")]
seriallab$Outcome=as.factor(seriallab$Outcome)

#impute non_valids va data sete tamiz FAILED: DATA KHAILI BAD TAR HAM SHOD BAD AZ IMPUTE

#seriallab_bamissing<- outremovd_frValid_data_WBCs[c("Outcome", "Lab1","Lab2","Lab3","Lab4","Lab5","Lab6")]
#seriallab_bamissing$Outcome=as.factor(seriallab_bamissing$Outcome)
#imputed_seriallab <- mice(seriallab_bamissing, m=5, maxit = 50, method = 'pmm', seed = 500)
#summary(imputed_seriallab)
#a=imputed_seriallab$imp$Lab1
#seriallab_imputed <- complete(imputed_seriallab,1)
#seriallab <- seriallab_imputed

#long kardan
long_outremovd_frValid_data_WBCs <- seriallab %>%
  pivot_longer(cols=c("Lab1","Lab2","Lab3","Lab4","Lab5","Lab6"),
               names_to = "serial_lab", values_to = "lab_value")


# mohasebe p value beine moteghaiere serial dar 1 dataye paired
long_outremovd_frValid_data_WBCs$serial_lab = as.factor(long_outremovd_frValid_data_WBCs$serial_lab)
compare_means(lab_value ~ serial_lab,  data = long_outremovd_frValid_data_WBCs)

#keshidan shekle p value ha 
my_comparisons <- list( c("Lab1", "Lab2"),  c("Lab1", "Lab3"), c("Lab1", "Lab4"), c("Lab1", "Lab5"),c("Lab1", "Lab6")
                        #c("Lab2", "Lab3"), c("Lab2", "Lab4"), c("Lab2", "Lab5"),c("Lab2", "Lab6"),
                        #c("Lab3", "Lab4"), c("Lab3", "Lab5"),c("Lab3", "Lab6"),
                        #c("Lab4", "Lab5"),c("Lab4", "Lab6"),
                        #c("Lab5", "Lab6")
)

ggplot(long_outremovd_frValid_data_WBCs, aes(x=serial_lab, y=lab_value, group=serial_lab, fill=Outcome))+
  geom_violin()+
  facet_grid(. ~ Outcome)+
  stat_compare_means(comparisons = my_comparisons, label = "p.signif")+ 
  stat_summary(fun.data=mean_sdl, geom="pointrange", color="White", size=0.7,shape=18)





# average and confidence interval of survived
seriallab_survvd = filter(seriallab, Outcome=="Survived")


survvd_avrg1=mean(seriallab_survvd$Lab1,na.rm = TRUE)
survvd_avrg2=mean(seriallab_survvd$Lab2,na.rm = TRUE)
survvd_avrg3=mean(seriallab_survvd$Lab3,na.rm = TRUE)
survvd_avrg4=mean(seriallab_survvd$Lab4,na.rm = TRUE)
survvd_avrg5=mean(seriallab_survvd$Lab5,na.rm = TRUE)
survvd_avrg6=mean(seriallab_survvd$Lab6,na.rm = TRUE)


model1 <- lm(Lab1 ~ 1, seriallab_survvd)
survvd_ci1 <- confint(model1, level=0.95)
survvd_low1 = survvd_ci1[1]
survvd_upr1 = survvd_ci1[2]

model2 <- lm(Lab2 ~ 1, seriallab_survvd)
survvd_ci2 <- confint(model2, level=0.95)
survvd_low2 =  survvd_ci2[1]
survvd_upr2 = survvd_ci2[2]

model3 <- lm(Lab3 ~ 1, seriallab_survvd)
survvd_ci3 <- confint(model3, level=0.95)
survvd_low3 =  survvd_ci3[1]
survvd_upr3 = survvd_ci3[2]

model4 <- lm(Lab4 ~ 1, seriallab_survvd)
survvd_ci4 <- confint(model4, level=0.95)
survvd_low4 =  survvd_ci4[1]
survvd_upr4 = survvd_ci4[2]

model5 <- lm(Lab5 ~ 1, seriallab_survvd)
survvd_ci5 <- confint(model5, level=0.95)
survvd_low5 =  survvd_ci5[1]
survvd_upr5 = survvd_ci5[2]

model6 <- lm(Lab6 ~ 1, seriallab_survvd)
survvd_ci6 <- confint(model6, level=0.95)
survvd_low6 =  survvd_ci6[1]
survvd_upr6 = survvd_ci6[2]


############ average and confidence interval of mortality
seriallab_mort = filter(seriallab, Outcome=="Mortality")


mort_avrg1=mean(seriallab_mort$Lab1,na.rm = TRUE)
mort_avrg2=mean(seriallab_mort$Lab2,na.rm = TRUE)
mort_avrg3=mean(seriallab_mort$Lab3,na.rm = TRUE)
mort_avrg4=mean(seriallab_mort$Lab4,na.rm = TRUE)
mort_avrg5=mean(seriallab_mort$Lab5,na.rm = TRUE)
mort_avrg6=mean(seriallab_mort$Lab6,na.rm = TRUE)


model1 <- lm(Lab1 ~ 1, seriallab_mort)
mort_ci1 <- confint(model1, level=0.95)
mort_low1 = mort_ci1[1]
mort_upr1 = mort_ci1[2]

model2 <- lm(Lab2 ~ 1, seriallab_mort)
mort_ci2 <- confint(model2, level=0.95)
mort_low2 =  mort_ci2[1]
mort_upr2 = mort_ci2[2]

model3 <- lm(Lab3 ~ 1, seriallab_mort)
mort_ci3 <- confint(model3, level=0.95)
mort_low3 =  mort_ci3[1]
mort_upr3 = mort_ci3[2]

model4 <- lm(Lab4 ~ 1, seriallab_mort)
mort_ci4 <- confint(model4, level=0.95)
mort_low4 =  mort_ci4[1]
mort_upr4 = mort_ci4[2]

model5 <- lm(Lab5 ~ 1, seriallab_mort)
mort_ci5 <- confint(model5, level=0.95)
mort_low5 =  mort_ci5[1]
mort_upr5 = mort_ci5[2]

model6 <- lm(Lab6 ~ 1, seriallab_mort)
mort_ci6 <- confint(model6, level=0.95)
mort_low6=  mort_ci6[1]
mort_upr6 = mort_ci6[2]

################# sotre text of mean (lci, uci) for survvied
txt_survvd_avrg1=as.character(round(survvd_avrg1,2))
txt_survvd_low1= as.character(round(survvd_low1,2))
txt_survvd_upr1 = as.character(round(survvd_upr1,2))
label__survvd1=paste(txt_survvd_avrg1,"\n(",txt_survvd_low1,", ",txt_survvd_upr1,")", sep = "")


txt_survvd_avrg2=as.character(round(survvd_avrg2,2))
txt_survvd_low2= as.character(round(survvd_low2,2))
txt_survvd_upr2 = as.character(round(survvd_upr2,2))
label__survvd2=paste(txt_survvd_avrg2,"\n(",txt_survvd_low2,", ",txt_survvd_upr2,")", sep = "")

txt_survvd_avrg3=as.character(round(survvd_avrg3,2))
txt_survvd_low3= as.character(round(survvd_low3,2))
txt_survvd_upr3 = as.character(round(survvd_upr3,2))
label__survvd3=paste(txt_survvd_avrg3,"\n(",txt_survvd_low3,", ",txt_survvd_upr3,")", sep = "")

txt_survvd_avrg4=as.character(round(survvd_avrg4,2))
txt_survvd_low4= as.character(round(survvd_low4,2))
txt_survvd_upr4 = as.character(round(survvd_upr4,2))
label__survvd4=paste(txt_survvd_avrg4,"\n(",txt_survvd_low4,", ",txt_survvd_upr4,")", sep = "")

txt_survvd_avrg5=as.character(round(survvd_avrg5,2))
txt_survvd_low5= as.character(round(survvd_low5,2))
txt_survvd_upr5 = as.character(round(survvd_upr5,2))
label__survvd5=paste(txt_survvd_avrg5,"\n(",txt_survvd_low5,", ",txt_survvd_upr5,")", sep = "")

txt_survvd_avrg6=as.character(round(survvd_avrg6,2))
txt_survvd_low6= as.character(round(survvd_low6,2))
txt_survvd_upr6 = as.character(round(survvd_upr6,2))
label__survvd6=paste(txt_survvd_avrg6,"\n(",txt_survvd_low6,", ",txt_survvd_upr6,")", sep = "")

################## sotre text of mean (lci, uci) for mortality

txt_mort_avrg1=as.character(round(mort_avrg1,2))
txt_mort_low1= as.character(round(mort_low1,2))
txt_mort_upr1 = as.character(round(mort_upr1,2))
label__mort1=paste(txt_mort_avrg1,"\n(",txt_mort_low1,", ",txt_mort_upr1,")", sep = "")

txt_mort_avrg2=as.character(round(mort_avrg2,2))
txt_mort_low2= as.character(round(mort_low2,2))
txt_mort_upr2 = as.character(round(mort_upr2,2))
label__mort2=paste(txt_mort_avrg2,"\n(",txt_mort_low2,", ",txt_mort_upr2,")", sep = "")

txt_mort_avrg3=as.character(round(mort_avrg3,2))
txt_mort_low3= as.character(round(mort_low3,2))
txt_mort_upr3 = as.character(round(mort_upr3,2))
label__mort3=paste(txt_mort_avrg3,"\n(",txt_mort_low3,", ",txt_mort_upr3,")", sep = "")

txt_mort_avrg4=as.character(round(mort_avrg4,2))
txt_mort_low4= as.character(round(mort_low4,2))
txt_mort_upr4 = as.character(round(mort_upr4,2))
label__mort4=paste(txt_mort_avrg4,"\n(",txt_mort_low4,", ",txt_mort_upr4,")", sep = "")

txt_mort_avrg5=as.character(round(mort_avrg5,2))
txt_mort_low5= as.character(round(mort_low5,2))
txt_mort_upr5 = as.character(round(mort_upr5,2))
label__mort5=paste(txt_mort_avrg5,"\n(",txt_mort_low5,", ",txt_mort_upr5,")", sep = "")

txt_mort_avrg6=as.character(round(mort_avrg6,2))
txt_mort_low6= as.character(round(mort_low6,2))
txt_mort_upr6 = as.character(round(mort_upr6,2))
label__mort6=paste(txt_mort_avrg6,"\n(",txt_mort_low6,", ",txt_mort_upr6,")", sep = "")


########### add these mean and Ci as a label of your table

Serial_plot<- ggplot(long_outremovd_frValid_data_WBCs, aes(x=serial_lab, y=lab_value, group=serial_lab, fill=Outcome))+
  geom_violin(alpha=0.3, color="grey")+
  geom_boxplot(notch=TRUE,color="black", size =1, alpha=1,outlier.alpha = 0.0)+
  facet_grid(. ~ Outcome)+
  stat_compare_means(comparisons = my_comparisons,label = "p.signif", method="wilcox.test")+ # or  method="t.test"
  stat_summary(fun.data=mean_sdl, geom="line", color="White", size=0.5,shape=18)+
  theme(axis.title.y = element_blank())
Serial_plot



dat_text <- data.frame(
  label = c(label__survvd1,label__survvd2,label__survvd3,label__survvd4,label__survvd5,label__survvd6,
            label__mort1,label__mort2,label__mort3,label__mort4,label__mort5,label__mort6),
  Outcome = c("Survived","Survived","Survived","Survived","Survived","Survived",
              "Mortality","Mortality","Mortality","Mortality","Mortality","Mortality"),
  x     = c(1,2,3,4,5,6,
            1,2,3,4,5,6),
  y     = c(-4,-4,-4,-4,-4,-4,
            -4,-4,-4,-4,-4,-4)
)


serial_plot_wconfidence<-Serial_plot+
  annotate("text", x =1, y=-4,
           label= label__survvd1,
           col="dark blue",
           size=3)+
  annotate("text", x =2, y=-4,
           label= label__survvd2,
           col="dark blue",
           size=3)+
  annotate("text", x =3, y=-4,
           label= label__survvd3,
           col="dark blue",
           size=3)+
  annotate("text", x =4, y=-4,
           label= label__survvd4,
           col="dark blue",
           size=3)+
  annotate("text", x =5, y=-4,
           label= label__survvd5,
           col="dark blue",
           size=3)+
  annotate("text", x =6, y=-4,
           label= label__survvd6,
           col="dark blue",
           size=3)+
  annotate("text", x =1, y=-10,
           label= label__mort1,
           col="dark red",
           size=3)+
  annotate("text", x =2, y=-10,
           label= label__mort2,
           col="dark red",
           size=3)+
  annotate("text", x =3, y=-10,
           label= label__mort3,
           col="dark red",
           size=3)+
  annotate("text", x =4, y=-10,
           label= label__mort4,
           col="dark red",
           size=3)+
  annotate("text", x =5, y=-10,
           label= label__mort5,
           col="dark red",
           size=3)+
  annotate("text", x =6, y=-10,
           label= label__mort6,
           col="dark red",
           size=3)
serial_plot_wconfidence

#change X axis labels 
Name_of_lab1=paste(Name_of_lab,"1")
Name_of_lab2=paste(Name_of_lab,"2")
Name_of_lab3=paste(Name_of_lab,"3")
Name_of_lab4=paste(Name_of_lab,"4")
Name_of_lab5=paste(Name_of_lab,"5")
Name_of_lab6=paste(Name_of_lab,"6")

Final_Plot_serial_lab<-serial_plot_wconfidence+
  scale_x_discrete(labels=c("Lab1" = Name_of_lab1,
                            "Lab2" = Name_of_lab2,
                            "Lab3" = Name_of_lab3,
                            "Lab4" = Name_of_lab4,
                            "Lab5" = Name_of_lab5,
                            "Lab6" = Name_of_lab6
  ))+
  annotate("text", x= 6.5, y=-7, label = "mean and 95% CI",angle="90")


#######show serial lab values result AST###############

Final_Plot_serial_AST<- Final_Plot_serial_lab
Final_Plot_serial_AST



Average=c(mort_avrg1,
          mort_avrg2,
          mort_avrg3,
          mort_avrg4,
          mort_avrg5,
          mort_avrg6,
          survvd_avrg1,
          survvd_avrg2,
          survvd_avrg3,
          survvd_avrg4,
          survvd_avrg5,
          survvd_avrg6)

LowerCI=c(mort_low1,
          mort_low2,
          mort_low3,
          mort_low4,
          mort_low5,
          mort_low6,
          survvd_low1,
          survvd_low2,
          survvd_low3,
          survvd_low4,
          survvd_low5,
          survvd_low6)

UpperCI=c(mort_upr1,
          mort_upr2,
          mort_upr3,
          mort_upr4,
          mort_upr5,
          mort_upr6,
          survvd_upr1,
          survvd_upr2,
          survvd_upr3,
          survvd_upr4,
          survvd_upr5,
          survvd_upr6)

OUTCOME=c("Mortality",
          "Mortality",
          "Mortality",
          "Mortality",
          "Mortality",
          "Mortality",
          "Survived",
          "Survived",
          "Survived",
          "Survived",
          "Survived",
          "Survived")

Lab_Names=c(Name_of_lab,
            Name_of_lab,
            Name_of_lab,
            Name_of_lab,
            Name_of_lab,
            Name_of_lab,
            Name_of_lab,
            Name_of_lab,
            Name_of_lab,
            Name_of_lab,
            Name_of_lab,
            Name_of_lab)

Day_of_Lab=c(1,2,3,4,5,6,1,2,3,4,5,6)
For_joinpoint_AST=data.frame(mortAverage=Average,
                             LowerCI=LowerCI,
                             UpperCI=UpperCI,
                             Lab_Names=Lab_Names,
                             Day_of_Lab=Day_of_Lab,
                             OUTCOME=OUTCOME)


qqPlot(long_outremovd_frValid_data_WBCs$lab_value) #currently method is wilcoxon, change to t.test if normalized



##  #############serial LAB VLAUES alt#############################

#read dataset
Dataset=read.csv("C:/DOCUMENTS/MY WORK/MaGHALE/Big COVID PROJECT/Article Lab/covid.csv")
attach(Dataset)
Dataset$gender=as.factor(Dataset$gender)
Dataset$Outcome=as.factor(Dataset$Outcome)

# create a dataset with just WBCs
data_Labs <- Dataset[c("Outcome","ALT1", "ALT2", "ALT3","ALT4","ALT5","ALT6")]
Name_of_lab="ALT"

data_Labs$Lab1<-data_Labs$ALT1
data_Labs$Lab2<-data_Labs$ALT2
data_Labs$Lab3<-data_Labs$ALT3
data_Labs$Lab4<-data_Labs$ALT4
data_Labs$Lab5<-data_Labs$ALT5
data_Labs$Lab6<-data_Labs$ALT6

# we use it for creating labels in ggplot
#select patient with at least 4 valid points
data_Labs$labs_NA <- rowSums(is.na(data_Labs))
frValid_data_Labs <- filter(data_Labs, between(labs_NA, 0,2))

# remove outliers 
frValid_data_Labs <- frValid_data_Labs
frValid_data_Labs$Outcome=as.factor(frValid_data_Labs$Outcome)
summary(frValid_data_Labs$Outcome)
###########Hidden Serial lab values alt###############

f1 <- ave(frValid_data_Labs$Lab1,frValid_data_Labs$Outcome,FUN=scale)
f2 <- ave(frValid_data_Labs$Lab2,frValid_data_Labs$Outcome,FUN=scale)
f3 <- ave(frValid_data_Labs$Lab3,frValid_data_Labs$Outcome,FUN=scale)
f4 <- ave(frValid_data_Labs$Lab4,frValid_data_Labs$Outcome,FUN=scale)
f5 <- ave(frValid_data_Labs$Lab5,frValid_data_Labs$Outcome,FUN=scale)
f6 <- ave(frValid_data_Labs$Lab6,frValid_data_Labs$Outcome,FUN=scale)

frValid_data_Labs$f1=f1
frValid_data_Labs$f2=f2
frValid_data_Labs$f3=f3
frValid_data_Labs$f4=f4
frValid_data_Labs$f5=f5
frValid_data_Labs$f6=f6

outremovd_frValid_data_Labs <- filter (frValid_data_Labs, 
                                       (between(f6, -3,3)| is.na(f6)) & (between(f5, -3,3)| is.na(f5)) &
                                         (between(f4, -3,3)| is.na(f4)) & (between(f3, -3,3)| is.na(f3)) &
                                         (between(f2, -3,3)| is.na(f2)) & (between(f1, -3,3)| is.na(f1)) )


#tamiz kardan data
seriallab<- outremovd_frValid_data_Labs[c("Outcome", "Lab1","Lab2","Lab3","Lab4","Lab5","Lab6")]
seriallab$Outcome=as.factor(seriallab$Outcome)

#impute non_valids va data sete tamiz FAILED: DATA KHAILI BAD TAR HAM SHOD BAD AZ IMPUTE

#seriallab_bamissing<- outremovd_frValid_data_WBCs[c("Outcome", "Lab1","Lab2","Lab3","Lab4","Lab5","Lab6")]
#seriallab_bamissing$Outcome=as.factor(seriallab_bamissing$Outcome)
#imputed_seriallab <- mice(seriallab_bamissing, m=5, maxit = 50, method = 'pmm', seed = 500)
#summary(imputed_seriallab)
#a=imputed_seriallab$imp$Lab1
#seriallab_imputed <- complete(imputed_seriallab,1)
#seriallab <- seriallab_imputed

#long kardan
long_outremovd_frValid_data_WBCs <- seriallab %>%
  pivot_longer(cols=c("Lab1","Lab2","Lab3","Lab4","Lab5","Lab6"),
               names_to = "serial_lab", values_to = "lab_value")


# mohasebe p value beine moteghaiere serial dar 1 dataye paired
long_outremovd_frValid_data_WBCs$serial_lab = as.factor(long_outremovd_frValid_data_WBCs$serial_lab)
compare_means(lab_value ~ serial_lab,  data = long_outremovd_frValid_data_WBCs)

#keshidan shekle p value ha 
my_comparisons <- list( c("Lab1", "Lab2"),  c("Lab1", "Lab3"), c("Lab1", "Lab4"), c("Lab1", "Lab5"),c("Lab1", "Lab6")
                        #c("Lab2", "Lab3"), c("Lab2", "Lab4"), c("Lab2", "Lab5"),c("Lab2", "Lab6"),
                        #c("Lab3", "Lab4"), c("Lab3", "Lab5"),c("Lab3", "Lab6"),
                        #c("Lab4", "Lab5"),c("Lab4", "Lab6"),
                        #c("Lab5", "Lab6")
)

ggplot(long_outremovd_frValid_data_WBCs, aes(x=serial_lab, y=lab_value, group=serial_lab, fill=Outcome))+
  geom_violin()+
  facet_grid(. ~ Outcome)+
  stat_compare_means(comparisons = my_comparisons, label = "p.signif")+ 
  stat_summary(fun.data=mean_sdl, geom="pointrange", color="White", size=0.7,shape=18)





# average and confidence interval of survived
seriallab_survvd = filter(seriallab, Outcome=="Survived")


survvd_avrg1=mean(seriallab_survvd$Lab1,na.rm = TRUE)
survvd_avrg2=mean(seriallab_survvd$Lab2,na.rm = TRUE)
survvd_avrg3=mean(seriallab_survvd$Lab3,na.rm = TRUE)
survvd_avrg4=mean(seriallab_survvd$Lab4,na.rm = TRUE)
survvd_avrg5=mean(seriallab_survvd$Lab5,na.rm = TRUE)
survvd_avrg6=mean(seriallab_survvd$Lab6,na.rm = TRUE)


model1 <- lm(Lab1 ~ 1, seriallab_survvd)
survvd_ci1 <- confint(model1, level=0.95)
survvd_low1 = survvd_ci1[1]
survvd_upr1 = survvd_ci1[2]

model2 <- lm(Lab2 ~ 1, seriallab_survvd)
survvd_ci2 <- confint(model2, level=0.95)
survvd_low2 =  survvd_ci2[1]
survvd_upr2 = survvd_ci2[2]

model3 <- lm(Lab3 ~ 1, seriallab_survvd)
survvd_ci3 <- confint(model3, level=0.95)
survvd_low3 =  survvd_ci3[1]
survvd_upr3 = survvd_ci3[2]

model4 <- lm(Lab4 ~ 1, seriallab_survvd)
survvd_ci4 <- confint(model4, level=0.95)
survvd_low4 =  survvd_ci4[1]
survvd_upr4 = survvd_ci4[2]

model5 <- lm(Lab5 ~ 1, seriallab_survvd)
survvd_ci5 <- confint(model5, level=0.95)
survvd_low5 =  survvd_ci5[1]
survvd_upr5 = survvd_ci5[2]

model6 <- lm(Lab6 ~ 1, seriallab_survvd)
survvd_ci6 <- confint(model6, level=0.95)
survvd_low6 =  survvd_ci6[1]
survvd_upr6 = survvd_ci6[2]


############ average and confidence interval of mortality
seriallab_mort = filter(seriallab, Outcome=="Mortality")


mort_avrg1=mean(seriallab_mort$Lab1,na.rm = TRUE)
mort_avrg2=mean(seriallab_mort$Lab2,na.rm = TRUE)
mort_avrg3=mean(seriallab_mort$Lab3,na.rm = TRUE)
mort_avrg4=mean(seriallab_mort$Lab4,na.rm = TRUE)
mort_avrg5=mean(seriallab_mort$Lab5,na.rm = TRUE)
mort_avrg6=mean(seriallab_mort$Lab6,na.rm = TRUE)


model1 <- lm(Lab1 ~ 1, seriallab_mort)
mort_ci1 <- confint(model1, level=0.95)
mort_low1 = mort_ci1[1]
mort_upr1 = mort_ci1[2]

model2 <- lm(Lab2 ~ 1, seriallab_mort)
mort_ci2 <- confint(model2, level=0.95)
mort_low2 =  mort_ci2[1]
mort_upr2 = mort_ci2[2]

model3 <- lm(Lab3 ~ 1, seriallab_mort)
mort_ci3 <- confint(model3, level=0.95)
mort_low3 =  mort_ci3[1]
mort_upr3 = mort_ci3[2]

model4 <- lm(Lab4 ~ 1, seriallab_mort)
mort_ci4 <- confint(model4, level=0.95)
mort_low4 =  mort_ci4[1]
mort_upr4 = mort_ci4[2]

model5 <- lm(Lab5 ~ 1, seriallab_mort)
mort_ci5 <- confint(model5, level=0.95)
mort_low5 =  mort_ci5[1]
mort_upr5 = mort_ci5[2]

model6 <- lm(Lab6 ~ 1, seriallab_mort)
mort_ci6 <- confint(model6, level=0.95)
mort_low6=  mort_ci6[1]
mort_upr6 = mort_ci6[2]

################# sotre text of mean (lci, uci) for survvied
txt_survvd_avrg1=as.character(round(survvd_avrg1,2))
txt_survvd_low1= as.character(round(survvd_low1,2))
txt_survvd_upr1 = as.character(round(survvd_upr1,2))
label__survvd1=paste(txt_survvd_avrg1,"\n(",txt_survvd_low1,", ",txt_survvd_upr1,")", sep = "")


txt_survvd_avrg2=as.character(round(survvd_avrg2,2))
txt_survvd_low2= as.character(round(survvd_low2,2))
txt_survvd_upr2 = as.character(round(survvd_upr2,2))
label__survvd2=paste(txt_survvd_avrg2,"\n(",txt_survvd_low2,", ",txt_survvd_upr2,")", sep = "")

txt_survvd_avrg3=as.character(round(survvd_avrg3,2))
txt_survvd_low3= as.character(round(survvd_low3,2))
txt_survvd_upr3 = as.character(round(survvd_upr3,2))
label__survvd3=paste(txt_survvd_avrg3,"\n(",txt_survvd_low3,", ",txt_survvd_upr3,")", sep = "")

txt_survvd_avrg4=as.character(round(survvd_avrg4,2))
txt_survvd_low4= as.character(round(survvd_low4,2))
txt_survvd_upr4 = as.character(round(survvd_upr4,2))
label__survvd4=paste(txt_survvd_avrg4,"\n(",txt_survvd_low4,", ",txt_survvd_upr4,")", sep = "")

txt_survvd_avrg5=as.character(round(survvd_avrg5,2))
txt_survvd_low5= as.character(round(survvd_low5,2))
txt_survvd_upr5 = as.character(round(survvd_upr5,2))
label__survvd5=paste(txt_survvd_avrg5,"\n(",txt_survvd_low5,", ",txt_survvd_upr5,")", sep = "")

txt_survvd_avrg6=as.character(round(survvd_avrg6,2))
txt_survvd_low6= as.character(round(survvd_low6,2))
txt_survvd_upr6 = as.character(round(survvd_upr6,2))
label__survvd6=paste(txt_survvd_avrg6,"\n(",txt_survvd_low6,", ",txt_survvd_upr6,")", sep = "")

################## sotre text of mean (lci, uci) for mortality

txt_mort_avrg1=as.character(round(mort_avrg1,2))
txt_mort_low1= as.character(round(mort_low1,2))
txt_mort_upr1 = as.character(round(mort_upr1,2))
label__mort1=paste(txt_mort_avrg1,"\n(",txt_mort_low1,", ",txt_mort_upr1,")", sep = "")

txt_mort_avrg2=as.character(round(mort_avrg2,2))
txt_mort_low2= as.character(round(mort_low2,2))
txt_mort_upr2 = as.character(round(mort_upr2,2))
label__mort2=paste(txt_mort_avrg2,"\n(",txt_mort_low2,", ",txt_mort_upr2,")", sep = "")

txt_mort_avrg3=as.character(round(mort_avrg3,2))
txt_mort_low3= as.character(round(mort_low3,2))
txt_mort_upr3 = as.character(round(mort_upr3,2))
label__mort3=paste(txt_mort_avrg3,"\n(",txt_mort_low3,", ",txt_mort_upr3,")", sep = "")

txt_mort_avrg4=as.character(round(mort_avrg4,2))
txt_mort_low4= as.character(round(mort_low4,2))
txt_mort_upr4 = as.character(round(mort_upr4,2))
label__mort4=paste(txt_mort_avrg4,"\n(",txt_mort_low4,", ",txt_mort_upr4,")", sep = "")

txt_mort_avrg5=as.character(round(mort_avrg5,2))
txt_mort_low5= as.character(round(mort_low5,2))
txt_mort_upr5 = as.character(round(mort_upr5,2))
label__mort5=paste(txt_mort_avrg5,"\n(",txt_mort_low5,", ",txt_mort_upr5,")", sep = "")

txt_mort_avrg6=as.character(round(mort_avrg6,2))
txt_mort_low6= as.character(round(mort_low6,2))
txt_mort_upr6 = as.character(round(mort_upr6,2))
label__mort6=paste(txt_mort_avrg6,"\n(",txt_mort_low6,", ",txt_mort_upr6,")", sep = "")


########### add these mean and Ci as a label of your table

Serial_plot<- ggplot(long_outremovd_frValid_data_WBCs, aes(x=serial_lab, y=lab_value, group=serial_lab, fill=Outcome))+
  geom_violin(alpha=0.3, color="grey")+
  geom_boxplot(notch=TRUE,color="black", size =1, alpha=1,outlier.alpha = 0.0)+
  facet_grid(. ~ Outcome)+
  stat_compare_means(comparisons = my_comparisons,label = "p.signif", method="wilcox.test")+ # or  method="t.test"
  stat_summary(fun.data=mean_sdl, geom="line", color="White", size=0.5,shape=18)+
  theme(axis.title.y = element_blank())
Serial_plot



dat_text <- data.frame(
  label = c(label__survvd1,label__survvd2,label__survvd3,label__survvd4,label__survvd5,label__survvd6,
            label__mort1,label__mort2,label__mort3,label__mort4,label__mort5,label__mort6),
  Outcome = c("Survived","Survived","Survived","Survived","Survived","Survived",
              "Mortality","Mortality","Mortality","Mortality","Mortality","Mortality"),
  x     = c(1,2,3,4,5,6,
            1,2,3,4,5,6),
  y     = c(-4,-4,-4,-4,-4,-4,
            -4,-4,-4,-4,-4,-4)
)


serial_plot_wconfidence<-Serial_plot+
  annotate("text", x =1, y=-4,
           label= label__survvd1,
           col="dark blue",
           size=3)+
  annotate("text", x =2, y=-4,
           label= label__survvd2,
           col="dark blue",
           size=3)+
  annotate("text", x =3, y=-4,
           label= label__survvd3,
           col="dark blue",
           size=3)+
  annotate("text", x =4, y=-4,
           label= label__survvd4,
           col="dark blue",
           size=3)+
  annotate("text", x =5, y=-4,
           label= label__survvd5,
           col="dark blue",
           size=3)+
  annotate("text", x =6, y=-4,
           label= label__survvd6,
           col="dark blue",
           size=3)+
  annotate("text", x =1, y=-10,
           label= label__mort1,
           col="dark red",
           size=3)+
  annotate("text", x =2, y=-10,
           label= label__mort2,
           col="dark red",
           size=3)+
  annotate("text", x =3, y=-10,
           label= label__mort3,
           col="dark red",
           size=3)+
  annotate("text", x =4, y=-10,
           label= label__mort4,
           col="dark red",
           size=3)+
  annotate("text", x =5, y=-10,
           label= label__mort5,
           col="dark red",
           size=3)+
  annotate("text", x =6, y=-10,
           label= label__mort6,
           col="dark red",
           size=3)
serial_plot_wconfidence

#change X axis labels 
Name_of_lab1=paste(Name_of_lab,"1")
Name_of_lab2=paste(Name_of_lab,"2")
Name_of_lab3=paste(Name_of_lab,"3")
Name_of_lab4=paste(Name_of_lab,"4")
Name_of_lab5=paste(Name_of_lab,"5")
Name_of_lab6=paste(Name_of_lab,"6")

Final_Plot_serial_lab<-serial_plot_wconfidence+
  scale_x_discrete(labels=c("Lab1" = Name_of_lab1,
                            "Lab2" = Name_of_lab2,
                            "Lab3" = Name_of_lab3,
                            "Lab4" = Name_of_lab4,
                            "Lab5" = Name_of_lab5,
                            "Lab6" = Name_of_lab6
  ))+
  annotate("text", x= 6.5, y=-7, label = "mean and 95% CI",angle="90")


#######show serial lab values result alt###############

Final_Plot_serial_ALT<- Final_Plot_serial_lab
Final_Plot_serial_ALT
Final_Plot_serial_lab
Average=c(mort_avrg1,
          mort_avrg2,
          mort_avrg3,
          mort_avrg4,
          mort_avrg5,
          mort_avrg6,
          survvd_avrg1,
          survvd_avrg2,
          survvd_avrg3,
          survvd_avrg4,
          survvd_avrg5,
          survvd_avrg6)

LowerCI=c(mort_low1,
          mort_low2,
          mort_low3,
          mort_low4,
          mort_low5,
          mort_low6,
          survvd_low1,
          survvd_low2,
          survvd_low3,
          survvd_low4,
          survvd_low5,
          survvd_low6)

UpperCI=c(mort_upr1,
          mort_upr2,
          mort_upr3,
          mort_upr4,
          mort_upr5,
          mort_upr6,
          survvd_upr1,
          survvd_upr2,
          survvd_upr3,
          survvd_upr4,
          survvd_upr5,
          survvd_upr6)

OUTCOME=c("Mortality",
          "Mortality",
          "Mortality",
          "Mortality",
          "Mortality",
          "Mortality",
          "Survived",
          "Survived",
          "Survived",
          "Survived",
          "Survived",
          "Survived")

Lab_Names=c(Name_of_lab,
            Name_of_lab,
            Name_of_lab,
            Name_of_lab,
            Name_of_lab,
            Name_of_lab,
            Name_of_lab,
            Name_of_lab,
            Name_of_lab,
            Name_of_lab,
            Name_of_lab,
            Name_of_lab)

Day_of_Lab=c(1,2,3,4,5,6,1,2,3,4,5,6)
For_joinpoint_ALT=data.frame(mortAverage=Average,
                             LowerCI=LowerCI,
                             UpperCI=UpperCI,
                             Lab_Names=Lab_Names,
                             Day_of_Lab=Day_of_Lab,
                             OUTCOME=OUTCOME)


qqPlot(long_outremovd_frValid_data_WBCs$lab_value) #currently method is wilcoxon, change to t.test if normalized


##  #############serial LAB VLAUES ldh#############################

#read dataset
Dataset=read.csv("C:/DOCUMENTS/MY WORK/MaGHALE/Big COVID PROJECT/Article Lab/covid.csv")
attach(Dataset)
Dataset$gender=as.factor(Dataset$gender)
Dataset$Outcome=as.factor(Dataset$Outcome)

# create a dataset with just WBCs
data_Labs <- Dataset[c("Outcome","LDH1", "LDH2", "LDH3","LDH4","LDH5","LDH6")]
Name_of_lab="LDH"

data_Labs$Lab1<-data_Labs$LDH1
data_Labs$Lab2<-data_Labs$LDH2
data_Labs$Lab3<-data_Labs$LDH3
data_Labs$Lab4<-data_Labs$LDH4
data_Labs$Lab5<-data_Labs$LDH5
data_Labs$Lab6<-data_Labs$LDH6

# we use it for creating labels in ggplot
#select patient with at least 4 valid points
data_Labs$labs_NA <- rowSums(is.na(data_Labs))
frValid_data_Labs <- filter(data_Labs, between(labs_NA, 0,2))

# remove outliers 
frValid_data_Labs <- frValid_data_Labs
frValid_data_Labs$Outcome=as.factor(frValid_data_Labs$Outcome)
summary(frValid_data_Labs$Outcome)
###########Hidden Serial lab values ldh###############

f1 <- ave(frValid_data_Labs$Lab1,frValid_data_Labs$Outcome,FUN=scale)
f2 <- ave(frValid_data_Labs$Lab2,frValid_data_Labs$Outcome,FUN=scale)
f3 <- ave(frValid_data_Labs$Lab3,frValid_data_Labs$Outcome,FUN=scale)
f4 <- ave(frValid_data_Labs$Lab4,frValid_data_Labs$Outcome,FUN=scale)
f5 <- ave(frValid_data_Labs$Lab5,frValid_data_Labs$Outcome,FUN=scale)
f6 <- ave(frValid_data_Labs$Lab6,frValid_data_Labs$Outcome,FUN=scale)

frValid_data_Labs$f1=f1
frValid_data_Labs$f2=f2
frValid_data_Labs$f3=f3
frValid_data_Labs$f4=f4
frValid_data_Labs$f5=f5
frValid_data_Labs$f6=f6

outremovd_frValid_data_Labs <- filter (frValid_data_Labs, 
                                       (between(f6, -3,3)| is.na(f6)) & (between(f5, -3,3)| is.na(f5)) &
                                         (between(f4, -3,3)| is.na(f4)) & (between(f3, -3,3)| is.na(f3)) &
                                         (between(f2, -3,3)| is.na(f2)) & (between(f1, -3,3)| is.na(f1)) )


#tamiz kardan data
seriallab<- outremovd_frValid_data_Labs[c("Outcome", "Lab1","Lab2","Lab3","Lab4","Lab5","Lab6")]
seriallab$Outcome=as.factor(seriallab$Outcome)

#impute non_valids va data sete tamiz FAILED: DATA KHAILI BAD TAR HAM SHOD BAD AZ IMPUTE

#seriallab_bamissing<- outremovd_frValid_data_WBCs[c("Outcome", "Lab1","Lab2","Lab3","Lab4","Lab5","Lab6")]
#seriallab_bamissing$Outcome=as.factor(seriallab_bamissing$Outcome)
#imputed_seriallab <- mice(seriallab_bamissing, m=5, maxit = 50, method = 'pmm', seed = 500)
#summary(imputed_seriallab)
#a=imputed_seriallab$imp$Lab1
#seriallab_imputed <- complete(imputed_seriallab,1)
#seriallab <- seriallab_imputed

#long kardan
long_outremovd_frValid_data_WBCs <- seriallab %>%
  pivot_longer(cols=c("Lab1","Lab2","Lab3","Lab4","Lab5","Lab6"),
               names_to = "serial_lab", values_to = "lab_value")


# mohasebe p value beine moteghaiere serial dar 1 dataye paired
long_outremovd_frValid_data_WBCs$serial_lab = as.factor(long_outremovd_frValid_data_WBCs$serial_lab)
compare_means(lab_value ~ serial_lab,  data = long_outremovd_frValid_data_WBCs)

#keshidan shekle p value ha 
my_comparisons <- list( c("Lab1", "Lab2"),  c("Lab1", "Lab3"), c("Lab1", "Lab4"), c("Lab1", "Lab5"),c("Lab1", "Lab6")
                        #c("Lab2", "Lab3"), c("Lab2", "Lab4"), c("Lab2", "Lab5"),c("Lab2", "Lab6"),
                        #c("Lab3", "Lab4"), c("Lab3", "Lab5"),c("Lab3", "Lab6"),
                        #c("Lab4", "Lab5"),c("Lab4", "Lab6"),
                        #c("Lab5", "Lab6")
)

ggplot(long_outremovd_frValid_data_WBCs, aes(x=serial_lab, y=lab_value, group=serial_lab, fill=Outcome))+
  geom_violin()+
  facet_grid(. ~ Outcome)+
  stat_compare_means(comparisons = my_comparisons, label = "p.signif")+ 
  stat_summary(fun.data=mean_sdl, geom="pointrange", color="White", size=0.7,shape=18)





# average and confidence interval of survived
seriallab_survvd = filter(seriallab, Outcome=="Survived")


survvd_avrg1=mean(seriallab_survvd$Lab1,na.rm = TRUE)
survvd_avrg2=mean(seriallab_survvd$Lab2,na.rm = TRUE)
survvd_avrg3=mean(seriallab_survvd$Lab3,na.rm = TRUE)
survvd_avrg4=mean(seriallab_survvd$Lab4,na.rm = TRUE)
survvd_avrg5=mean(seriallab_survvd$Lab5,na.rm = TRUE)
survvd_avrg6=mean(seriallab_survvd$Lab6,na.rm = TRUE)


model1 <- lm(Lab1 ~ 1, seriallab_survvd)
survvd_ci1 <- confint(model1, level=0.95)
survvd_low1 = survvd_ci1[1]
survvd_upr1 = survvd_ci1[2]

model2 <- lm(Lab2 ~ 1, seriallab_survvd)
survvd_ci2 <- confint(model2, level=0.95)
survvd_low2 =  survvd_ci2[1]
survvd_upr2 = survvd_ci2[2]

model3 <- lm(Lab3 ~ 1, seriallab_survvd)
survvd_ci3 <- confint(model3, level=0.95)
survvd_low3 =  survvd_ci3[1]
survvd_upr3 = survvd_ci3[2]

model4 <- lm(Lab4 ~ 1, seriallab_survvd)
survvd_ci4 <- confint(model4, level=0.95)
survvd_low4 =  survvd_ci4[1]
survvd_upr4 = survvd_ci4[2]

model5 <- lm(Lab5 ~ 1, seriallab_survvd)
survvd_ci5 <- confint(model5, level=0.95)
survvd_low5 =  survvd_ci5[1]
survvd_upr5 = survvd_ci5[2]

model6 <- lm(Lab6 ~ 1, seriallab_survvd)
survvd_ci6 <- confint(model6, level=0.95)
survvd_low6 =  survvd_ci6[1]
survvd_upr6 = survvd_ci6[2]


############ average and confidence interval of mortality
seriallab_mort = filter(seriallab, Outcome=="Mortality")


mort_avrg1=mean(seriallab_mort$Lab1,na.rm = TRUE)
mort_avrg2=mean(seriallab_mort$Lab2,na.rm = TRUE)
mort_avrg3=mean(seriallab_mort$Lab3,na.rm = TRUE)
mort_avrg4=mean(seriallab_mort$Lab4,na.rm = TRUE)
mort_avrg5=mean(seriallab_mort$Lab5,na.rm = TRUE)
mort_avrg6=mean(seriallab_mort$Lab6,na.rm = TRUE)


model1 <- lm(Lab1 ~ 1, seriallab_mort)
mort_ci1 <- confint(model1, level=0.95)
mort_low1 = mort_ci1[1]
mort_upr1 = mort_ci1[2]

model2 <- lm(Lab2 ~ 1, seriallab_mort)
mort_ci2 <- confint(model2, level=0.95)
mort_low2 =  mort_ci2[1]
mort_upr2 = mort_ci2[2]

model3 <- lm(Lab3 ~ 1, seriallab_mort)
mort_ci3 <- confint(model3, level=0.95)
mort_low3 =  mort_ci3[1]
mort_upr3 = mort_ci3[2]

model4 <- lm(Lab4 ~ 1, seriallab_mort)
mort_ci4 <- confint(model4, level=0.95)
mort_low4 =  mort_ci4[1]
mort_upr4 = mort_ci4[2]

model5 <- lm(Lab5 ~ 1, seriallab_mort)
mort_ci5 <- confint(model5, level=0.95)
mort_low5 =  mort_ci5[1]
mort_upr5 = mort_ci5[2]

model6 <- lm(Lab6 ~ 1, seriallab_mort)
mort_ci6 <- confint(model6, level=0.95)
mort_low6=  mort_ci6[1]
mort_upr6 = mort_ci6[2]

################# sotre text of mean (lci, uci) for survvied
txt_survvd_avrg1=as.character(round(survvd_avrg1,2))
txt_survvd_low1= as.character(round(survvd_low1,2))
txt_survvd_upr1 = as.character(round(survvd_upr1,2))
label__survvd1=paste(txt_survvd_avrg1,"\n(",txt_survvd_low1,", ",txt_survvd_upr1,")", sep = "")


txt_survvd_avrg2=as.character(round(survvd_avrg2,2))
txt_survvd_low2= as.character(round(survvd_low2,2))
txt_survvd_upr2 = as.character(round(survvd_upr2,2))
label__survvd2=paste(txt_survvd_avrg2,"\n(",txt_survvd_low2,", ",txt_survvd_upr2,")", sep = "")

txt_survvd_avrg3=as.character(round(survvd_avrg3,2))
txt_survvd_low3= as.character(round(survvd_low3,2))
txt_survvd_upr3 = as.character(round(survvd_upr3,2))
label__survvd3=paste(txt_survvd_avrg3,"\n(",txt_survvd_low3,", ",txt_survvd_upr3,")", sep = "")

txt_survvd_avrg4=as.character(round(survvd_avrg4,2))
txt_survvd_low4= as.character(round(survvd_low4,2))
txt_survvd_upr4 = as.character(round(survvd_upr4,2))
label__survvd4=paste(txt_survvd_avrg4,"\n(",txt_survvd_low4,", ",txt_survvd_upr4,")", sep = "")

txt_survvd_avrg5=as.character(round(survvd_avrg5,2))
txt_survvd_low5= as.character(round(survvd_low5,2))
txt_survvd_upr5 = as.character(round(survvd_upr5,2))
label__survvd5=paste(txt_survvd_avrg5,"\n(",txt_survvd_low5,", ",txt_survvd_upr5,")", sep = "")

txt_survvd_avrg6=as.character(round(survvd_avrg6,2))
txt_survvd_low6= as.character(round(survvd_low6,2))
txt_survvd_upr6 = as.character(round(survvd_upr6,2))
label__survvd6=paste(txt_survvd_avrg6,"\n(",txt_survvd_low6,", ",txt_survvd_upr6,")", sep = "")

################## sotre text of mean (lci, uci) for mortality

txt_mort_avrg1=as.character(round(mort_avrg1,2))
txt_mort_low1= as.character(round(mort_low1,2))
txt_mort_upr1 = as.character(round(mort_upr1,2))
label__mort1=paste(txt_mort_avrg1,"\n(",txt_mort_low1,", ",txt_mort_upr1,")", sep = "")

txt_mort_avrg2=as.character(round(mort_avrg2,2))
txt_mort_low2= as.character(round(mort_low2,2))
txt_mort_upr2 = as.character(round(mort_upr2,2))
label__mort2=paste(txt_mort_avrg2,"\n(",txt_mort_low2,", ",txt_mort_upr2,")", sep = "")

txt_mort_avrg3=as.character(round(mort_avrg3,2))
txt_mort_low3= as.character(round(mort_low3,2))
txt_mort_upr3 = as.character(round(mort_upr3,2))
label__mort3=paste(txt_mort_avrg3,"\n(",txt_mort_low3,", ",txt_mort_upr3,")", sep = "")

txt_mort_avrg4=as.character(round(mort_avrg4,2))
txt_mort_low4= as.character(round(mort_low4,2))
txt_mort_upr4 = as.character(round(mort_upr4,2))
label__mort4=paste(txt_mort_avrg4,"\n(",txt_mort_low4,", ",txt_mort_upr4,")", sep = "")

txt_mort_avrg5=as.character(round(mort_avrg5,2))
txt_mort_low5= as.character(round(mort_low5,2))
txt_mort_upr5 = as.character(round(mort_upr5,2))
label__mort5=paste(txt_mort_avrg5,"\n(",txt_mort_low5,", ",txt_mort_upr5,")", sep = "")

txt_mort_avrg6=as.character(round(mort_avrg6,2))
txt_mort_low6= as.character(round(mort_low6,2))
txt_mort_upr6 = as.character(round(mort_upr6,2))
label__mort6=paste(txt_mort_avrg6,"\n(",txt_mort_low6,", ",txt_mort_upr6,")", sep = "")


########### add these mean and Ci as a label of your table

Serial_plot<- ggplot(long_outremovd_frValid_data_WBCs, aes(x=serial_lab, y=lab_value, group=serial_lab, fill=Outcome))+
  geom_violin(alpha=0.3, color="grey")+
  geom_boxplot(notch=TRUE,color="black", size =1, alpha=1,outlier.alpha = 0.0)+
  facet_grid(. ~ Outcome)+
  stat_compare_means(comparisons = my_comparisons,label = "p.signif", method="wilcox.test")+ # or  method="t.test"
  stat_summary(fun.data=mean_sdl, geom="line", color="White", size=0.5,shape=18)+
  theme(axis.title.y = element_blank())
Serial_plot



dat_text <- data.frame(
  label = c(label__survvd1,label__survvd2,label__survvd3,label__survvd4,label__survvd5,label__survvd6,
            label__mort1,label__mort2,label__mort3,label__mort4,label__mort5,label__mort6),
  Outcome = c("Survived","Survived","Survived","Survived","Survived","Survived",
              "Mortality","Mortality","Mortality","Mortality","Mortality","Mortality"),
  x     = c(1,2,3,4,5,6,
            1,2,3,4,5,6),
  y     = c(-4,-4,-4,-4,-4,-4,
            -4,-4,-4,-4,-4,-4)
)


serial_plot_wconfidence<-Serial_plot+
  annotate("text", x =1, y=-4,
           label= label__survvd1,
           col="dark blue",
           size=3)+
  annotate("text", x =2, y=-4,
           label= label__survvd2,
           col="dark blue",
           size=3)+
  annotate("text", x =3, y=-4,
           label= label__survvd3,
           col="dark blue",
           size=3)+
  annotate("text", x =4, y=-4,
           label= label__survvd4,
           col="dark blue",
           size=3)+
  annotate("text", x =5, y=-4,
           label= label__survvd5,
           col="dark blue",
           size=3)+
  annotate("text", x =6, y=-4,
           label= label__survvd6,
           col="dark blue",
           size=3)+
  annotate("text", x =1, y=-10,
           label= label__mort1,
           col="dark red",
           size=3)+
  annotate("text", x =2, y=-10,
           label= label__mort2,
           col="dark red",
           size=3)+
  annotate("text", x =3, y=-10,
           label= label__mort3,
           col="dark red",
           size=3)+
  annotate("text", x =4, y=-10,
           label= label__mort4,
           col="dark red",
           size=3)+
  annotate("text", x =5, y=-10,
           label= label__mort5,
           col="dark red",
           size=3)+
  annotate("text", x =6, y=-10,
           label= label__mort6,
           col="dark red",
           size=3)
serial_plot_wconfidence

#change X axis labels 
Name_of_lab1=paste(Name_of_lab,"1")
Name_of_lab2=paste(Name_of_lab,"2")
Name_of_lab3=paste(Name_of_lab,"3")
Name_of_lab4=paste(Name_of_lab,"4")
Name_of_lab5=paste(Name_of_lab,"5")
Name_of_lab6=paste(Name_of_lab,"6")

Final_Plot_serial_lab<-serial_plot_wconfidence+
  scale_x_discrete(labels=c("Lab1" = Name_of_lab1,
                            "Lab2" = Name_of_lab2,
                            "Lab3" = Name_of_lab3,
                            "Lab4" = Name_of_lab4,
                            "Lab5" = Name_of_lab5,
                            "Lab6" = Name_of_lab6
  ))+
  annotate("text", x= 6.5, y=-7, label = "mean and 95% CI",angle="90")


#######show serial lab values result ldh###############

Final_Plot_serial_LDH<- Final_Plot_serial_lab
Final_Plot_serial_LDH


Average=c(mort_avrg1,
          mort_avrg2,
          mort_avrg3,
          mort_avrg4,
          mort_avrg5,
          mort_avrg6,
          survvd_avrg1,
          survvd_avrg2,
          survvd_avrg3,
          survvd_avrg4,
          survvd_avrg5,
          survvd_avrg6)

LowerCI=c(mort_low1,
          mort_low2,
          mort_low3,
          mort_low4,
          mort_low5,
          mort_low6,
          survvd_low1,
          survvd_low2,
          survvd_low3,
          survvd_low4,
          survvd_low5,
          survvd_low6)

UpperCI=c(mort_upr1,
          mort_upr2,
          mort_upr3,
          mort_upr4,
          mort_upr5,
          mort_upr6,
          survvd_upr1,
          survvd_upr2,
          survvd_upr3,
          survvd_upr4,
          survvd_upr5,
          survvd_upr6)

OUTCOME=c("Mortality",
          "Mortality",
          "Mortality",
          "Mortality",
          "Mortality",
          "Mortality",
          "Survived",
          "Survived",
          "Survived",
          "Survived",
          "Survived",
          "Survived")

Lab_Names=c(Name_of_lab,
            Name_of_lab,
            Name_of_lab,
            Name_of_lab,
            Name_of_lab,
            Name_of_lab,
            Name_of_lab,
            Name_of_lab,
            Name_of_lab,
            Name_of_lab,
            Name_of_lab,
            Name_of_lab)

Day_of_Lab=c(1,2,3,4,5,6,1,2,3,4,5,6)
For_joinpoint_LDH=data.frame(mortAverage=Average,
                             LowerCI=LowerCI,
                             UpperCI=UpperCI,
                             Lab_Names=Lab_Names,
                             Day_of_Lab=Day_of_Lab,
                             OUTCOME=OUTCOME)


qqPlot(long_outremovd_frValid_data_WBCs$lab_value) #currently method is wilcoxon, change to t.test if normalized


##  #############serial LAB VLAUES CRP#############################

#read dataset
Dataset=read.csv("C:/DOCUMENTS/MY WORK/MaGHALE/Big COVID PROJECT/Article Lab/covid.csv")
attach(Dataset)
Dataset$gender=as.factor(Dataset$gender)
Dataset$Outcome=as.factor(Dataset$Outcome)

# create a dataset with just WBCs
data_Labs <- Dataset[c("Outcome","CRP1", "CRP2", "CRP3","CRP4","CRP5","CRP6")]
Name_of_lab="CRP"

data_Labs$Lab1<-data_Labs$CRP1
data_Labs$Lab2<-data_Labs$CRP2
data_Labs$Lab3<-data_Labs$CRP3
data_Labs$Lab4<-data_Labs$CRP4
data_Labs$Lab5<-data_Labs$CRP5
data_Labs$Lab6<-data_Labs$CRP6

# we use it for creating labels in ggplot
#select patient with at least 4 valid points
data_Labs$labs_NA <- rowSums(is.na(data_Labs))
frValid_data_Labs <- filter(data_Labs, between(labs_NA, 0,2))

# remove outliers 
frValid_data_Labs <- frValid_data_Labs
frValid_data_Labs$Outcome=as.factor(frValid_data_Labs$Outcome)
summary(frValid_data_Labs$Outcome)
###########Hidden Serial lab values CRP###############

f1 <- ave(frValid_data_Labs$Lab1,frValid_data_Labs$Outcome,FUN=scale)
f2 <- ave(frValid_data_Labs$Lab2,frValid_data_Labs$Outcome,FUN=scale)
f3 <- ave(frValid_data_Labs$Lab3,frValid_data_Labs$Outcome,FUN=scale)
f4 <- ave(frValid_data_Labs$Lab4,frValid_data_Labs$Outcome,FUN=scale)
f5 <- ave(frValid_data_Labs$Lab5,frValid_data_Labs$Outcome,FUN=scale)
f6 <- ave(frValid_data_Labs$Lab6,frValid_data_Labs$Outcome,FUN=scale)

frValid_data_Labs$f1=f1
frValid_data_Labs$f2=f2
frValid_data_Labs$f3=f3
frValid_data_Labs$f4=f4
frValid_data_Labs$f5=f5
frValid_data_Labs$f6=f6

outremovd_frValid_data_Labs <- filter (frValid_data_Labs, 
                                       (between(f6, -3,3)| is.na(f6)) & (between(f5, -3,3)| is.na(f5)) &
                                         (between(f4, -3,3)| is.na(f4)) & (between(f3, -3,3)| is.na(f3)) &
                                         (between(f2, -3,3)| is.na(f2)) & (between(f1, -3,3)| is.na(f1)) )


#tamiz kardan data
seriallab<- outremovd_frValid_data_Labs[c("Outcome", "Lab1","Lab2","Lab3","Lab4","Lab5","Lab6")]
seriallab$Outcome=as.factor(seriallab$Outcome)

#impute non_valids va data sete tamiz FAILED: DATA KHAILI BAD TAR HAM SHOD BAD AZ IMPUTE

#seriallab_bamissing<- outremovd_frValid_data_WBCs[c("Outcome", "Lab1","Lab2","Lab3","Lab4","Lab5","Lab6")]
#seriallab_bamissing$Outcome=as.factor(seriallab_bamissing$Outcome)
#imputed_seriallab <- mice(seriallab_bamissing, m=5, maxit = 50, method = 'pmm', seed = 500)
#summary(imputed_seriallab)
#a=imputed_seriallab$imp$Lab1
#seriallab_imputed <- complete(imputed_seriallab,1)
#seriallab <- seriallab_imputed

#long kardan
long_outremovd_frValid_data_WBCs <- seriallab %>%
  pivot_longer(cols=c("Lab1","Lab2","Lab3","Lab4","Lab5","Lab6"),
               names_to = "serial_lab", values_to = "lab_value")


# mohasebe p value beine moteghaiere serial dar 1 dataye paired
long_outremovd_frValid_data_WBCs$serial_lab = as.factor(long_outremovd_frValid_data_WBCs$serial_lab)
compare_means(lab_value ~ serial_lab,  data = long_outremovd_frValid_data_WBCs)

#keshidan shekle p value ha 
my_comparisons <- list( c("Lab1", "Lab2"),  c("Lab1", "Lab3"), c("Lab1", "Lab4"), c("Lab1", "Lab5"),c("Lab1", "Lab6")
                        #c("Lab2", "Lab3"), c("Lab2", "Lab4"), c("Lab2", "Lab5"),c("Lab2", "Lab6"),
                        #c("Lab3", "Lab4"), c("Lab3", "Lab5"),c("Lab3", "Lab6"),
                        #c("Lab4", "Lab5"),c("Lab4", "Lab6"),
                        #c("Lab5", "Lab6")
)

ggplot(long_outremovd_frValid_data_WBCs, aes(x=serial_lab, y=lab_value, group=serial_lab, fill=Outcome))+
  geom_violin()+
  facet_grid(. ~ Outcome)+
  stat_compare_means(comparisons = my_comparisons, label = "p.signif")+ 
  stat_summary(fun.data=mean_sdl, geom="pointrange", color="White", size=0.7,shape=18)



R. version

# average and confidence interval of survived
seriallab_survvd = filter(seriallab, Outcome=="Survived")


survvd_avrg1=mean(seriallab_survvd$Lab1,na.rm = TRUE)
survvd_avrg2=mean(seriallab_survvd$Lab2,na.rm = TRUE)
survvd_avrg3=mean(seriallab_survvd$Lab3,na.rm = TRUE)
survvd_avrg4=mean(seriallab_survvd$Lab4,na.rm = TRUE)
survvd_avrg5=mean(seriallab_survvd$Lab5,na.rm = TRUE)
survvd_avrg6=mean(seriallab_survvd$Lab6,na.rm = TRUE)


model1 <- lm(Lab1 ~ 1, seriallab_survvd)
survvd_ci1 <- confint(model1, level=0.95)
survvd_low1 = survvd_ci1[1]
survvd_upr1 = survvd_ci1[2]

model2 <- lm(Lab2 ~ 1, seriallab_survvd)
survvd_ci2 <- confint(model2, level=0.95)
survvd_low2 =  survvd_ci2[1]
survvd_upr2 = survvd_ci2[2]

model3 <- lm(Lab3 ~ 1, seriallab_survvd)
survvd_ci3 <- confint(model3, level=0.95)
survvd_low3 =  survvd_ci3[1]
survvd_upr3 = survvd_ci3[2]

model4 <- lm(Lab4 ~ 1, seriallab_survvd)
survvd_ci4 <- confint(model4, level=0.95)
survvd_low4 =  survvd_ci4[1]
survvd_upr4 = survvd_ci4[2]

model5 <- lm(Lab5 ~ 1, seriallab_survvd)
survvd_ci5 <- confint(model5, level=0.95)
survvd_low5 =  survvd_ci5[1]
survvd_upr5 = survvd_ci5[2]

model6 <- lm(Lab6 ~ 1, seriallab_survvd)
survvd_ci6 <- confint(model6, level=0.95)
survvd_low6 =  survvd_ci6[1]
survvd_upr6 = survvd_ci6[2]


############ average and confidence interval of mortality
seriallab_mort = filter(seriallab, Outcome=="Mortality")


mort_avrg1=mean(seriallab_mort$Lab1,na.rm = TRUE)
mort_avrg2=mean(seriallab_mort$Lab2,na.rm = TRUE)
mort_avrg3=mean(seriallab_mort$Lab3,na.rm = TRUE)
mort_avrg4=mean(seriallab_mort$Lab4,na.rm = TRUE)
mort_avrg5=mean(seriallab_mort$Lab5,na.rm = TRUE)
mort_avrg6=mean(seriallab_mort$Lab6,na.rm = TRUE)


model1 <- lm(Lab1 ~ 1, seriallab_mort)
mort_ci1 <- confint(model1, level=0.95)
mort_low1 = mort_ci1[1]
mort_upr1 = mort_ci1[2]

model2 <- lm(Lab2 ~ 1, seriallab_mort)
mort_ci2 <- confint(model2, level=0.95)
mort_low2 =  mort_ci2[1]
mort_upr2 = mort_ci2[2]

model3 <- lm(Lab3 ~ 1, seriallab_mort)
mort_ci3 <- confint(model3, level=0.95)
mort_low3 =  mort_ci3[1]
mort_upr3 = mort_ci3[2]

model4 <- lm(Lab4 ~ 1, seriallab_mort)
mort_ci4 <- confint(model4, level=0.95)
mort_low4 =  mort_ci4[1]
mort_upr4 = mort_ci4[2]

model5 <- lm(Lab5 ~ 1, seriallab_mort)
mort_ci5 <- confint(model5, level=0.95)
mort_low5 =  mort_ci5[1]
mort_upr5 = mort_ci5[2]

model6 <- lm(Lab6 ~ 1, seriallab_mort)
mort_ci6 <- confint(model6, level=0.95)
mort_low6=  mort_ci6[1]
mort_upr6 = mort_ci6[2]

################# sotre text of mean (lci, uci) for survvied
txt_survvd_avrg1=as.character(round(survvd_avrg1,2))
txt_survvd_low1= as.character(round(survvd_low1,2))
txt_survvd_upr1 = as.character(round(survvd_upr1,2))
label__survvd1=paste(txt_survvd_avrg1,"\n(",txt_survvd_low1,", ",txt_survvd_upr1,")", sep = "")


txt_survvd_avrg2=as.character(round(survvd_avrg2,2))
txt_survvd_low2= as.character(round(survvd_low2,2))
txt_survvd_upr2 = as.character(round(survvd_upr2,2))
label__survvd2=paste(txt_survvd_avrg2,"\n(",txt_survvd_low2,", ",txt_survvd_upr2,")", sep = "")

txt_survvd_avrg3=as.character(round(survvd_avrg3,2))
txt_survvd_low3= as.character(round(survvd_low3,2))
txt_survvd_upr3 = as.character(round(survvd_upr3,2))
label__survvd3=paste(txt_survvd_avrg3,"\n(",txt_survvd_low3,", ",txt_survvd_upr3,")", sep = "")

txt_survvd_avrg4=as.character(round(survvd_avrg4,2))
txt_survvd_low4= as.character(round(survvd_low4,2))
txt_survvd_upr4 = as.character(round(survvd_upr4,2))
label__survvd4=paste(txt_survvd_avrg4,"\n(",txt_survvd_low4,", ",txt_survvd_upr4,")", sep = "")

txt_survvd_avrg5=as.character(round(survvd_avrg5,2))
txt_survvd_low5= as.character(round(survvd_low5,2))
txt_survvd_upr5 = as.character(round(survvd_upr5,2))
label__survvd5=paste(txt_survvd_avrg5,"\n(",txt_survvd_low5,", ",txt_survvd_upr5,")", sep = "")

txt_survvd_avrg6=as.character(round(survvd_avrg6,2))
txt_survvd_low6= as.character(round(survvd_low6,2))
txt_survvd_upr6 = as.character(round(survvd_upr6,2))
label__survvd6=paste(txt_survvd_avrg6,"\n(",txt_survvd_low6,", ",txt_survvd_upr6,")", sep = "")

################## sotre text of mean (lci, uci) for mortality

txt_mort_avrg1=as.character(round(mort_avrg1,2))
txt_mort_low1= as.character(round(mort_low1,2))
txt_mort_upr1 = as.character(round(mort_upr1,2))
label__mort1=paste(txt_mort_avrg1,"\n(",txt_mort_low1,", ",txt_mort_upr1,")", sep = "")

txt_mort_avrg2=as.character(round(mort_avrg2,2))
txt_mort_low2= as.character(round(mort_low2,2))
txt_mort_upr2 = as.character(round(mort_upr2,2))
label__mort2=paste(txt_mort_avrg2,"\n(",txt_mort_low2,", ",txt_mort_upr2,")", sep = "")

txt_mort_avrg3=as.character(round(mort_avrg3,2))
txt_mort_low3= as.character(round(mort_low3,2))
txt_mort_upr3 = as.character(round(mort_upr3,2))
label__mort3=paste(txt_mort_avrg3,"\n(",txt_mort_low3,", ",txt_mort_upr3,")", sep = "")

txt_mort_avrg4=as.character(round(mort_avrg4,2))
txt_mort_low4= as.character(round(mort_low4,2))
txt_mort_upr4 = as.character(round(mort_upr4,2))
label__mort4=paste(txt_mort_avrg4,"\n(",txt_mort_low4,", ",txt_mort_upr4,")", sep = "")

txt_mort_avrg5=as.character(round(mort_avrg5,2))
txt_mort_low5= as.character(round(mort_low5,2))
txt_mort_upr5 = as.character(round(mort_upr5,2))
label__mort5=paste(txt_mort_avrg5,"\n(",txt_mort_low5,", ",txt_mort_upr5,")", sep = "")

txt_mort_avrg6=as.character(round(mort_avrg6,2))
txt_mort_low6= as.character(round(mort_low6,2))
txt_mort_upr6 = as.character(round(mort_upr6,2))
label__mort6=paste(txt_mort_avrg6,"\n(",txt_mort_low6,", ",txt_mort_upr6,")", sep = "")


########### add these mean and Ci as a label of your table

Serial_plot<- ggplot(long_outremovd_frValid_data_WBCs, aes(x=serial_lab, y=lab_value, group=serial_lab, fill=Outcome))+
  geom_violin(alpha=0.3, color="grey")+
  geom_boxplot(notch=TRUE,color="black", size =1, alpha=1,outlier.alpha = 0.0)+
  facet_grid(. ~ Outcome)+
  stat_compare_means(comparisons = my_comparisons,label = "p.signif", method="wilcox.test")+ # or  method="t.test"
  stat_summary(fun.data=mean_sdl, geom="line", color="White", size=0.5,shape=18)+
  theme(axis.title.y = element_blank())
Serial_plot



dat_text <- data.frame(
  label = c(label__survvd1,label__survvd2,label__survvd3,label__survvd4,label__survvd5,label__survvd6,
            label__mort1,label__mort2,label__mort3,label__mort4,label__mort5,label__mort6),
  Outcome = c("Survived","Survived","Survived","Survived","Survived","Survived",
              "Mortality","Mortality","Mortality","Mortality","Mortality","Mortality"),
  x     = c(1,2,3,4,5,6,
            1,2,3,4,5,6),
  y     = c(-4,-4,-4,-4,-4,-4,
            -4,-4,-4,-4,-4,-4)
)


serial_plot_wconfidence<-Serial_plot+
  annotate("text", x =1, y=-4,
           label= label__survvd1,
           col="dark blue",
           size=3)+
  annotate("text", x =2, y=-4,
           label= label__survvd2,
           col="dark blue",
           size=3)+
  annotate("text", x =3, y=-4,
           label= label__survvd3,
           col="dark blue",
           size=3)+
  annotate("text", x =4, y=-4,
           label= label__survvd4,
           col="dark blue",
           size=3)+
  annotate("text", x =5, y=-4,
           label= label__survvd5,
           col="dark blue",
           size=3)+
  annotate("text", x =6, y=-4,
           label= label__survvd6,
           col="dark blue",
           size=3)+
  annotate("text", x =1, y=-10,
           label= label__mort1,
           col="dark red",
           size=3)+
  annotate("text", x =2, y=-10,
           label= label__mort2,
           col="dark red",
           size=3)+
  annotate("text", x =3, y=-10,
           label= label__mort3,
           col="dark red",
           size=3)+
  annotate("text", x =4, y=-10,
           label= label__mort4,
           col="dark red",
           size=3)+
  annotate("text", x =5, y=-10,
           label= label__mort5,
           col="dark red",
           size=3)+
  annotate("text", x =6, y=-10,
           label= label__mort6,
           col="dark red",
           size=3)
serial_plot_wconfidence

#change X axis labels 
Name_of_lab1=paste(Name_of_lab,"1")
Name_of_lab2=paste(Name_of_lab,"2")
Name_of_lab3=paste(Name_of_lab,"3")
Name_of_lab4=paste(Name_of_lab,"4")
Name_of_lab5=paste(Name_of_lab,"5")
Name_of_lab6=paste(Name_of_lab,"6")

Final_Plot_serial_lab<-serial_plot_wconfidence+
  scale_x_discrete(labels=c("Lab1" = Name_of_lab1,
                            "Lab2" = Name_of_lab2,
                            "Lab3" = Name_of_lab3,
                            "Lab4" = Name_of_lab4,
                            "Lab5" = Name_of_lab5,
                            "Lab6" = Name_of_lab6
  ))+
  annotate("text", x= 6.5, y=-7, label = "mean and 95% CI",angle="90")


#######show serial lab values result CRP###############

Final_Plot_serial_crp<- Final_Plot_serial_lab
Final_Plot_serial_crp


Average=c(mort_avrg1,
          mort_avrg2,
          mort_avrg3,
          mort_avrg4,
          mort_avrg5,
          mort_avrg6,
          survvd_avrg1,
          survvd_avrg2,
          survvd_avrg3,
          survvd_avrg4,
          survvd_avrg5,
          survvd_avrg6)

LowerCI=c(mort_low1,
          mort_low2,
          mort_low3,
          mort_low4,
          mort_low5,
          mort_low6,
          survvd_low1,
          survvd_low2,
          survvd_low3,
          survvd_low4,
          survvd_low5,
          survvd_low6)

UpperCI=c(mort_upr1,
          mort_upr2,
          mort_upr3,
          mort_upr4,
          mort_upr5,
          mort_upr6,
          survvd_upr1,
          survvd_upr2,
          survvd_upr3,
          survvd_upr4,
          survvd_upr5,
          survvd_upr6)

OUTCOME=c("Mortality",
          "Mortality",
          "Mortality",
          "Mortality",
          "Mortality",
          "Mortality",
          "Survived",
          "Survived",
          "Survived",
          "Survived",
          "Survived",
          "Survived")

Lab_Names=c(Name_of_lab,
            Name_of_lab,
            Name_of_lab,
            Name_of_lab,
            Name_of_lab,
            Name_of_lab,
            Name_of_lab,
            Name_of_lab,
            Name_of_lab,
            Name_of_lab,
            Name_of_lab,
            Name_of_lab)

Day_of_Lab=c(1,2,3,4,5,6,1,2,3,4,5,6)
For_joinpoint_CRP=data.frame(mortAverage=Average,
                             LowerCI=LowerCI,
                             UpperCI=UpperCI,
                             Lab_Names=Lab_Names,
                             Day_of_Lab=Day_of_Lab,
                             OUTCOME=OUTCOME)

qqPlot(long_outremovd_frValid_data_WBCs$lab_value) #currently method is wilcoxon, change to t.test if normalized




#############FInalsss####
Final_Plot_serial_WBC
Final_Plot_serial_lymph 
Final_Plot_serial_neut
Final_Plot_serial_PLT
Final_Plot_serial_Hb
Final_Plot_serial_MCV
Final_Plot_serial_urea
Final_Plot_serial_cr
Final_Plot_serial_AST
Final_Plot_serial_ALT
Final_Plot_serial_LDH
Final_Plot_serial_crp


grid.arrange(Final_Plot_serial_WBC,
             Final_Plot_serial_lymph,
             Final_Plot_serial_neut,
             Final_Plot_serial_PLT,
             Final_Plot_serial_Hb,
             Final_Plot_serial_MCV,
             Final_Plot_serial_urea,
             Final_Plot_serial_cr,
             Final_Plot_serial_AST,
             Final_Plot_serial_ALT,
             Final_Plot_serial_LDH,
             Final_Plot_serial_crp,
             ncol = 2, nrow = 6)

FOR_joinpoint_all <- rbind.data.frame(For_joinpoint_ALT, For_joinpoint_AST, For_joinpoint_Cr,
                                       For_joinpoint_CRP, For_joinpoint_Hb, For_joinpoint_LDH,
                                       For_joinpoint_lymph,For_joinpoint_MCV, For_joinpoint_neut,
                                       For_joinpoint_plt,For_joinpoint_urea, For_joinpoint_WBC)
write_xlsx(FOR_joinpoint_all, "C:/DOCUMENTS/MY WORK/MaGHALE/Big COVID PROJECT/Article Lab/for_joinpoint_all.xlsx")
write_xlsx(For_joinpoint_ALT, "C:/DOCUMENTS/MY WORK/MaGHALE/Big COVID PROJECT/Article Lab/for_joinpoint_ALT.xlsx")
Final_Plot_serial_ALT
Final_Plot_serial_AST

FOR_joinpoint_all <- rbind.data.frame(Final_Plot_serial_ALT,Final_Plot_serial_AST)

Final_Plot_serial_ALT
Final_Plot_serial_AST
