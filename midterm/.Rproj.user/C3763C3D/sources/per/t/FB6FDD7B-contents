#downloads csv data into R
aaa3841_OpenData_withNA <- read.csv("~/GitHub(assignments)/midterm/midterm/aaa3841_OpenData_withNA.csv")

#selecting data for analysis
cue_prenap = aaa3841_OpenData_withNA$Cued_Prenap
cue_delay = aaa3841_OpenData_withNA$Cued_Delayed
uncue_prenap = aaa3841_OpenData_withNA$Uncued_Prenap
uncue_delay = aaa3841_OpenData_withNA$Uncued_Delayed

#preparing new data table of the two factors with IAT scores for each subject
cue_factor = rep(c("C","U"), each = 80)
time_factor = rep(rep(c("D","P"), each = 40), 2)
subjects = aaa3841_OpenData_withNA$Ppt.
sub = as.factor(subjects)
subject_dv = c()


subject_dv =  c(cue_delay[1:length(cue_delay)], cue_prenap[1:length(cue_prenap)], 
                uncue_delay[1:length(uncue_delay)], uncue_prenap[1:length(uncue_prenap)])
  

# 2-way between subjects anova 
all_data = data.frame(sub, subject_dv, cue_factor, time_factor)
library(dplyr)
nas <- all_data %>%
  filter(is.na(subject_dv)==TRUE)
nas$sub<-as.factor(as.numeric(nas$sub))
bad_subjects <- levels(nas$sub)

clean_df <- all_data %>%
              filter(sub %in% bad_subjects == FALSE)

aov_out = aov(subject_dv~cue_factor*time_factor+Error(sub/(cue_factor*time_factor)), clean_df)
summary(aov_out)

library(sjstats)
eta_sq(aov_out, partial = TRUE)

# Graph of the means, and paired t-test
print(model.tables(aov_out,"means"), format="markdown")
means = c(mean(cue_prenap), mean(cue_delay, na.rm = TRUE), mean(uncue_prenap), mean(uncue_delay, na.rm = TRUE))
SEM = c(sd(cue_prenap)/38, sd(cue_delay, na.rm=TRUE)/38, sd(uncue_prenap)/38, sd(uncue_delay, na.rm=TRUE)/38)
t.test(uncue_prenap, uncue_delay, paired=TRUE)
group_means = data.frame(all_means=means, cue_f=c("Cue","Cue","Uncue","Uncue"), 
                         time_f=c("Prenap","Delay","Prenap","Delay"), SEM)

library(ggplot2)
ggplot(group_means, aes(x=cue_f,
                          y=all_means, 
                          group=time_f,
                          fill=time_f))+
  geom_bar(stat="identity",position="dodge")+
  theme_classic(base_size=12)+
  xlab("Cue factor") +
  ylab("Mean IAT scores")+
  geom_errorbar(aes(ymin=all_means-SEM,
                    ymax=all_means+SEM),
                position=position_dodge(width=0.9),
                width=.2,
                color="black")+
  coord_cartesian(ylim=c(0,1))

# Power Analysis

# function to run a simulated t-test
sim_power <- function(x){
  A <- rnorm(n=38,mean=0, sd=0.402)
  B <- rnorm(n=38,mean=(0+x), sd=0.451)
  return(t.test(A,B,var.equal=TRUE, paired = TRUE)$p.value)
}

# vector of effect sizes
effect_sizes <- seq(.1,2,.1)
# run simulation for each effect size 1000 times
power <- sapply(effect_sizes, 
                FUN = function(x) {
                  sims <- replicate(1000,sim_power(x))
                  sim_power <- length(sims[sims<.05])/length(sims)
                  return(sim_power)})
# combine into dataframe
plot_df <- data.frame(effect_sizes,power)
which(plot_df$effect_sizes >= .112)


# plot the power curve
ggplot(plot_df, aes(x=effect_sizes,
                    y=power))+
  geom_point()+
  geom_line()
