setwd("~/Documents/CHILD/ApplStat project/Codes")

x <- t(rmultinom(500, size = 1, prob = c(0.1,0.2,0.8)))
y <- factor(x[,1]*1 + x[,2] *2 + x[,3]*3, labels = c("a", "b", "c"))

x.timeRI <- t(rmultinom(500, size = 1, prob = c(0.080, 0.188, 0.333, 0.274, 0.125)))
x.sevRI <- t(rmultinom(500, size = 1, prob = c(0.8, 0.05, 0.05)))
x.NumQRI <-  t(rmultinom(500, size = 1, prob = c(0.244, 0.205, 0.179, 0.142, 0.078, 0.043, 0.025, 0.034, 0.051)))

ds.sim <- data.frame(FEV075_zscore_5y = rnorm(500, -0.5, 1),
FEV1_zscore_5y = rnorm(500, -0.3, 1.1),
FVC_zscore_5y = rnorm(500, -0.25, 1.3),
FEV075FVC_zscore_5y = rnorm(500, -0.4, 1),
FEV1FVC_zscore_5y = rnorm(500, -0.28, 1),
wheeze1y = rbinom(500, 1, 0.11),
recwheeze3y =rbinom(500, 1, 0.09),
recwheeze5y =rbinom(500, 1, 0.07),
atopy1y =rbinom(500, 1, 0.13),
atopy3y =rbinom(500, 1, 0.13),
atopy5y =rbinom(500, 1, 0.13),
firstRI_infant_RI_groups =ifelse(rbinom(500, 1, 0.27) ==1, "Infant_RI", "No_infant_RI"),
firstRI_time_bin =factor(x.timeRI[,1]*1 + x.timeRI[,2] *2 + x.timeRI[,3]*3 + x.timeRI[,4]*4 + x.timeRI[,5]*5,  
   labels = c("0-06", "06-12", "12-24", "24+", "Never_RI")),
ri_severity =factor(x.sevRI[,1]*1 + x.sevRI[,2] *2 + x.sevRI[,3]*3,  
                           labels = c("Mild", "Moderate", "Severe")),
ri_type =ifelse(rbinom(500, 1, 0.125) ==1, "No", "LRTI"))

library(mice)
miss.ds <- ampute(ds.sim, prop = 0.25)
ds.sim.miss <- miss.ds$amp
#num_RIquestionires_perpartici =factor(x.NumQRI[,1]*1 + x.NumQRI[,2] *2 + x.NumQRI[,3]*3 + x.NumQRI[,4]*4 + x.NumQRI[,5]*5 +
#                                          x.NumQRI[,6]*6 + x.NumQRI[,7] *7 + x.NumQRI[,8]*8 + x.NumQRI[,9]*9,  
 #                                       labels = c("2", "3", "4", "5", "6", "7", "8", "9", "10"))


saveRDS(ds.sim, "Simulated_data.Rds")
write.csv(ds.sim, "Simulated_data.csv")






