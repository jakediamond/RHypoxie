names<-c("cha031","cha057","coi23","loi068","tor076","charpa_donzy","coi_rieu","ratier_ribes")
colors<-c("blue","red","green","black","grey","sienna","purple","orange")

plot(1:10,1:10,type="n",xlim=c(0,3))
text(names,x=rep(2,8),y=9:2,col=colors)
x11()


dep=18
####DRYING:
colors<-c("blue","red","green","black","grey","sienna","purple","orange")

par(mfrow=c(3,1),mai=c(0.3,0.6,0.1,0.1))

i=3
tab<-read.table(file=paste("0_",names[i],".txt",sep=""),sep="\t",header=T)
plot(tab$time,12-tab$conc,xlim=c(0,500),ylim=c(0,12),col=colors[i],type="l",lwd=2.5,xlab="hours",ylab="O2 mg/L")
tab<-read.table(file=paste("1_",names[i],"_InternalConcentration.txt",sep=""),sep="\t",header=T)
plot(tab$time,12-tab$IT,xlim=c(0,500),ylim=c(0,12),col=colors[i],type="l",lwd=2.5,xlab="hours",ylab="internal O2")
rect(0,0,dep,12,,col="white",border=NA)
tab<-read.table(file=paste("1_",names[i],"_SurvivalRate.txt",sep=""),sep="\t",header=T)
plot(tab$time,tab$IT,xlim=c(0,500),ylim=c(0,1),col=colors[i],type="l",lwd=2.5,xlab="hours",ylab="Survival")
rect(0,0,dep,1,,col="white",border=NA)


par(mfrow=c(3,1),mai=c(0.3,0.6,0.1,0.1))
i=1
tab<-read.table(file=paste("0_",names[i],".txt",sep=""),sep="\t",header=T)
plot(tab$time,12-tab$conc,xlim=c(0,500),ylim=c(0,12),col=colors[i],type="l",lwd=2.5,xlab="hours",ylab="O2 mg/L")
for(i in 2:5){
	tab<-read.table(file=paste("0_",names[i],".txt",sep=""),sep="\t",header=T)
	lines(tab$time,12-tab$conc,xlim=c(0,500),ylim=c(0,12),col=colors[i],type="l",lwd=2.5,xlab="hours",ylab="O2 mg/L")
}
text(names[1:5],x=rep(400,5),y=9:5,col=colors)
text("DRYING",cex=3,x=470,y=10)
i=1
tab<-read.table(file=paste("1_",names[i],"_InternalConcentration.txt",sep=""),sep="\t",header=T)
plot(tab$time,12-tab$IT,xlim=c(0,500),ylim=c(0,12),col=colors[i],type="l",lwd=2.5,xlab="hours",ylab="internal O2")
for(i in 2:5){
	tab<-read.table(file=paste("1_",names[i],"_InternalConcentration.txt",sep=""),sep="\t",header=T)
	lines(tab$time,12-tab$IT,xlim=c(0,500),ylim=c(0,12),col=colors[i],type="l",lwd=2.5,xlab="hours",ylab="internal O2")
}
rect(0,0,dep,12,,col="white",border=NA)
i=1
tab<-read.table(file=paste("1_",names[i],"_SurvivalRate.txt",sep=""),sep="\t",header=T)
plot(tab$time,tab$IT,xlim=c(0,500),ylim=c(0,1),col=colors[i],type="l",lwd=2.5,xlab="hours",ylab="Survival")
for(i in 2:5){
	tab<-read.table(file=paste("1_",names[i],"_SurvivalRate.txt",sep=""),sep="\t",header=T)
	lines(tab$time,tab$IT,xlim=c(0,500),ylim=c(0,1),col=colors[i],type="l",lwd=2.5,xlab="hours",ylab="Survival")
}
rect(0,0,dep,1,,col="white",border=NA)
text("Individual_Tolerance",cex=3,x=400,y=0.8)

####STORM:
colors<-c("blue","red","green","black","grey","sienna","purple","orange")

par(mfrow=c(3,1),mai=c(0.3,0.6,0.1,0.1))
i=6
tab<-read.table(file=paste("0_",names[i],".txt",sep=""),sep="\t",header=T)
plot(tab$time,12-tab$conc,xlim=c(0,500),ylim=c(0,12),col=colors[i],type="l",lwd=2.5,xlab="hours",ylab="O2 mg/L")
tab<-read.table(file=paste("2_",names[i],"_InternalConcentration.txt",sep=""),sep="\t",header=T)
plot(tab$time,12-tab$IT,xlim=c(0,500),ylim=c(0,12),col=colors[i],type="l",lwd=2.5,xlab="hours",ylab="internal O2")
rect(0,0,dep,12,,col="white",border=NA)
tab<-read.table(file=paste("2_",names[i],"_SurvivalRate.txt",sep=""),sep="\t",header=T)
plot(tab$time,tab$IT,xlim=c(0,500),ylim=c(0,1),col=colors[i],type="l",lwd=2.5,xlab="hours",ylab="Survival")
rect(0,0,dep,1,,col="white",border=NA)


par(mfrow=c(3,1),mai=c(0.3,0.6,0.1,0.1))
i=6
tab<-read.table(file=paste("0_",names[i],".txt",sep=""),sep="\t",header=T)
plot(tab$time,12-tab$conc,xlim=c(0,500),ylim=c(0,12),col=colors[i],type="l",lwd=2.5,xlab="hours",ylab="O2 mg/L")
for(i in 7:8){
	tab<-read.table(file=paste("0_",names[i],".txt",sep=""),sep="\t",header=T)
	lines(tab$time,12-tab$conc,xlim=c(0,500),ylim=c(0,12),col=colors[i],type="l",lwd=2.5,xlab="hours",ylab="O2 mg/L")
}
text(names[6:8],x=rep(400,3),y=9:7,col=colors[6:8])
text("STORM",cex=3,x=470,y=10)
i=6
tab<-read.table(file=paste("2_",names[i],"_InternalConcentration.txt",sep=""),sep="\t",header=T)
plot(tab$time,12-tab$IT,xlim=c(0,500),ylim=c(0,12),col=colors[i],type="l",lwd=2.5,xlab="hours",ylab="internal O2")
for(i in 7:8){
	tab<-read.table(file=paste("2_",names[i],"_InternalConcentration.txt",sep=""),sep="\t",header=T)
	lines(tab$time,12-tab$IT,xlim=c(0,500),ylim=c(0,12),col=colors[i],type="l",lwd=2.5,xlab="hours",ylab="internal O2")
}
rect(0,0,dep,12,,col="white",border=NA)
i=6
tab<-read.table(file=paste("2_",names[i],"_SurvivalRate.txt",sep=""),sep="\t",header=T)
plot(tab$time,tab$IT,xlim=c(0,500),ylim=c(0,1),col=colors[i],type="l",lwd=2.5,xlab="hours",ylab="Survival")
for(i in 7:8){
	tab<-read.table(file=paste("2_",names[i],"_SurvivalRate.txt",sep=""),sep="\t",header=T)
	lines(tab$time,tab$IT,xlim=c(0,500),ylim=c(0,1),col=colors[i],type="l",lwd=2.5,xlab="hours",ylab="Survival")
}
rect(0,0,dep,1,,col="white",border=NA)
text("Individual_Tolerance",cex=3,x=400,y=0.8)







####LesDEUX:
colors<-c(rep("blue",5),rep("red",3))

par(mfrow=c(3,1),mai=c(0.3,0.6,0.1,0.1))
i=1
tab<-read.table(file=paste("0_",names[i],".txt",sep=""),sep="\t",header=T)
plot(tab$time,12-tab$conc,xlim=c(0,500),ylim=c(0,12),col=colors[i],type="l",lwd=2.5,xlab="hours",ylab="O2 mg/L")
for(i in 2:8){
	tab<-read.table(file=paste("0_",names[i],".txt",sep=""),sep="\t",header=T)
	lines(tab$time,12-tab$conc,xlim=c(0,500),ylim=c(0,12),col=colors[i],type="l",lwd=2.5,xlab="hours",ylab="O2 mg/L")
}
i=1
tab<-read.table(file=paste("1_",names[i],"_InternalConcentration.txt",sep=""),sep="\t",header=T)
plot(tab$time,12-tab$IT,xlim=c(0,500),ylim=c(0,12),col=colors[i],type="l",lwd=2.5,xlab="hours",ylab="internal O2")
for(i in 2:5){
	tab<-read.table(file=paste("1_",names[i],"_InternalConcentration.txt",sep=""),sep="\t",header=T)
	lines(tab$time,12-tab$IT,xlim=c(0,500),ylim=c(0,12),col=colors[i],type="l",lwd=2.5,xlab="hours",ylab="internal O2")
}
for(i in 6:8){
	tab<-read.table(file=paste("2_",names[i],"_InternalConcentration.txt",sep=""),sep="\t",header=T)
	lines(tab$time,12-tab$IT,xlim=c(0,500),ylim=c(0,12),col=colors[i],type="l",lwd=2.5,xlab="hours",ylab="internal O2")
}
i=1
tab<-read.table(file=paste("1_",names[i],"_SurvivalRate.txt",sep=""),sep="\t",header=T)
plot(tab$time,tab$IT,xlim=c(0,500),ylim=c(0,1),col=colors[i],type="l",lwd=2.5,xlab="hours",ylab="Survival")
for(i in 2:5){
	tab<-read.table(file=paste("1_",names[i],"_SurvivalRate.txt",sep=""),sep="\t",header=T)
	lines(tab$time,tab$IT,xlim=c(0,500),ylim=c(0,1),col=colors[i],type="l",lwd=2.5,xlab="hours",ylab="Survival")
}
for(i in 6:8){
	tab<-read.table(file=paste("2_",names[i],"_SurvivalRate.txt",sep=""),sep="\t",header=T)
	lines(tab$time,tab$IT,xlim=c(0,500),ylim=c(0,1),col=colors[i],type="l",lwd=2.5,xlab="hours",ylab="Survival")
}


##Comparaison:
surv<-c()
seuil=1 ##temps d'atteinte pour la premi?re fois du seuil critique choisi:
i=1
tab<-read.table(file=paste("0_",names[i],".txt",sep=""),sep="\t",header=T)
tim_seuil<-(1:length(tab$time))[(12-tab$conc)<seuil][1]
tab<-read.table(file=paste("1_",names[i],"_SurvivalRate.txt",sep=""),sep="\t",header=T)
surv<-tab[tab$time==tim_seuil,]
for(i in 3:5){
	tab<-read.table(file=paste("0_",names[i],".txt",sep=""),sep="\t",header=T)
	tim_seuil<-(1:length(tab$time))[(12-tab$conc)<seuil][1]
	tab<-read.table(file=paste("1_",names[i],"_SurvivalRate.txt",sep=""),sep="\t",header=T)
	surv<-rbind(surv,tab[tab$time==tim_seuil,])
}
for(i in 6:8){
	tab<-read.table(file=paste("0_",names[i],".txt",sep=""),sep="\t",header=T)
	tim_seuil<-(1:length(tab$time))[(12-tab$conc)<seuil][1]
	tab<-read.table(file=paste("2_",names[i],"_SurvivalRate.txt",sep=""),sep="\t",header=T)
	surv<-rbind(surv,tab[tab$time==tim_seuil,])
}
surv

amoy<-tapply(FUN=mean,X=a$temp,INDEX=a$jour)

library(tidyverse)
library(patchwork)
data_path <- file.path("data", "12_gammarid", "IT")   # path to the data
files_con <- dir(data_path, pattern = "Concentration.txt") # get file names for concentration
files_rate <- dir(data_path, pattern = "Rate.txt") # get file names for survival rates
files_dat <- dir(data_path, pattern = "0_") # get file names for data

df_conc <- data_frame(filename = files_con) %>% # create a data frame holding the file names
  mutate(file_contents = map(filename,          # read files into
                             ~ read_table(file.path(data_path, .))) # a new data column
  ) %>%
  unnest(file_contents) %>%
  mutate(site = word(filename, 2, sep = "_"),
         type = word(filename, 1, sep = "_")) %>%
  mutate(type = if_else(type == 1, "drying", "storm")) %>%
  set_names(names(.) %>% str_replace_all('"', ""))


df_rate <- data_frame(filename = files_rate) %>% # create a data frame holding the file names
  mutate(file_contents = map(filename,          # read files into
                             ~ read_table(file.path(data_path, .))) # a new data column
  ) %>%
  unnest(file_contents) %>%
  mutate(site = word(filename, 2, sep = "_"),
         type = word(filename, 1, sep = "_")) %>%
  mutate(type = if_else(type == 1, "drying", "storm")) %>%
  set_names(names(.) %>% str_replace_all('"', ""))

df_dat <- data_frame(filename = files_dat) %>% # create a data frame holding the file names
  mutate(file_contents = map(filename,          # read files into
                             ~ read_table(file.path(data_path, .))) # a new data column
  ) %>%
  unnest(file_contents) %>%
  mutate(site = str_match(filename, "[:digit:][:punct:]\\s*(.*?)\\s*.txt")[,2]) %>%
  left_join(distinct(df_conc, site, type))

# Time until 50% mortality
df_50 <- df_rate %>%
  group_by(site, type) %>%
  filter(abs(IT - 0.5) == min(abs(IT - 0.5)))


p_mort <- ggplot(data = df_rate,
       aes(x = times / 24,
           y = IT,
           color = site)) +
  geom_line() + 
  scale_color_manual(values = c("black", "#E69F00", "black", "#009E73", 
                                "#E69F00", "#D55E00", "#0072B2", "#0072B2")) +
  scale_fill_manual(values = c("black", "#E69F00", "black", "#009E73", 
                               "#E69F00", "#D55E00", "#0072B2", "#0072B2")) +
  geom_hline(yintercept = 0.5, linetype = "dashed") + 
  # geom_segment(data = df_50,
  #            aes(x = times / 24, xend = times /24,
  #                y = 0, yend = 0.5)) +
  facet_grid(cols = vars(type), scales = "free_x", space = "free_x") + 
  geom_ribbon(aes(ymin = IT_2.5, ymax = IT_97.5, fill = site), alpha = 0.2,
              show.legend = FALSE) + 
  theme_classic() +
  labs(x = "time (days)",
       y = "survival") +
  theme(strip.text = element_blank(),
        strip.background = element_blank(),
        legend.position = "none")


p_conc <- ggplot(data = df_dat,
       aes(x = time / 24,
           y = 12-conc,
           color = site)) +
  geom_line() + 
  # geom_segment(data = df_50,
  #              aes(x = times / 24, xend = times /24,
  #                  y = 0, yend = 3)) +
  scale_color_manual(values = c("black", "#E69F00", "black", "#009E73", 
                                "#E69F00", "#D55E00", "#0072B2", "#0072B2")) +
  facet_grid(cols = vars(type), scales = "free_x", space = "free_x") +
  geom_hline(yintercept = 3, linetype = "dashed") + 
  theme_classic() +
  labs(x = "time (days)",
       y = expression("DO (mg "*L^{-1}*")")) +
  theme(axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        strip.background = element_blank(),
        legend.position  = "none")

p_all <- (p_conc / p_mort) + plot_annotation(tag_levels = "A")
ggsave(plot = p_all,
       filename = file.path("results", "Figures", "gammarid_survival.png"),
       dpi = 1200,
       units = "cm",
       width = 18,
       height = 12)
