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
  

# 2-way within subjects anova 
all_data = data.frame(sub, subject_dv, cue_factor, time_factor)
aov_out = aov(subject_dv~cue_factor*time_factor, all_data)
summary(aov_out)

# Graph of the means
means = c(mean(cue_prenap), mean(cue_delay, na.rm = TRUE), mean(uncue_prenap), mean(uncue_delay, na.rm = TRUE))
mean_matrix = matrix(means,2,2)
barplot(mean_matrix, beside= TRUE, ylim= c(0,0.8))
eta_sq(aov_out)
