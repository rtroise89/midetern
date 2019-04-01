aaa3841_OpenData_withNA <- read.csv("~/GitHub(assignments)/midterm/midterm/aaa3841_OpenData_withNA.csv")

cue_prenap = aaa3841_OpenData_withNA$Cued_Prenap
cue_delay = aaa3841_OpenData_withNA$Cued_Delayed
uncue_prenap = aaa3841_OpenData_withNA$Uncued_Prenap
uncue_delay = aaa3841_OpenData_withNA$Uncued_Delayed
cue_factor = rep(rep(c("C","U"), each = 2),10)
time_factor = rep(rep(c("D","P"), 2),10)
subjects = rep(aaa3841_OpenData_withNA$Ppt., each = 4)
sub = as.factor(subjects)
means = c(mean(cue_prenap), mean(cue_delay, na.rm = TRUE), mean(uncue_prenap), mean(uncue_delay, na.rm = TRUE))
mean_matrix = matrix(means,2,2)
barplot(mean_matrix, beside= TRUE, ylim= c(0,0.8))

subject_dv = c()

for(i in 1:40){
    subject_dv = c(subject_dv, c(cue_delay[i], cue_prenap[i], uncue_delay[i], uncue_prenap[i]))
  
}

all_data = data.frame(sub, subject_dv, cue_factor, time_factor)
aov_out = aov(subject_dv~cue_factor*time_factor, all_data)
summary(aov_out)
