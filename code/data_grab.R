library(jsonlite)
transfer24 <- as.data.frame(fromJSON('https://barttorvik.com/2024_transfer_stats.json'))
transfer23 <- as.data.frame(fromJSON('https://barttorvik.com/2023_transfer_stats.json'))
transfer22 <- as.data.frame(fromJSON('https://barttorvik.com/2022_transfer_stats.json'))
transfer21 <- as.data.frame(fromJSON('https://barttorvik.com/2021_transfer_stats.json'))
transfer20 <- as.data.frame(fromJSON('https://barttorvik.com/2020_transfer_stats.json'))
write.csv(transfer24,'/Users/mileskee7/Desktop/College/Senior Year/W/course-paper-or-presentation-mileskee/transfer24.csv')
write.csv(transfer23,'/Users/mileskee7/Desktop/College/Senior Year/W/course-paper-or-presentation-mileskee/transfer23.csv')
write.csv(transfer22,'/Users/mileskee7/Desktop/College/Senior Year/W/course-paper-or-presentation-mileskee/transfer22.csv')
write.csv(transfer21,'/Users/mileskee7/Desktop/College/Senior Year/W/course-paper-or-presentation-mileskee/transfer21.csv')
write.csv(transfer20,'/Users/mileskee7/Desktop/College/Senior Year/W/course-paper-or-presentation-mileskee/transfer20.csv')
stats21 <- as.data.frame(read.csv('https://barttorvik.com/getadvstats.php?year=2021&csv=1',header=F)) #corresponds to transfer21
stats22 <- as.data.frame(read.csv('https://barttorvik.com/getadvstats.php?year=2022&csv=1',header=F)) #corresponds to transfer22
stats23 <- as.data.frame(read.csv('https://barttorvik.com/getadvstats.php?year=2023&csv=1',header=F)) #corresponds to transfer23
stats24 <- as.data.frame(read.csv('https://barttorvik.com/getadvstats.php?year=2024&csv=1',header=F)) #corresponds to transfer24
write.csv(stats21,'/Users/mileskee7/Desktop/College/Senior Year/W/course-paper-or-presentation-mileskee/stats21.csv')
write.csv(stats22,'/Users/mileskee7/Desktop/College/Senior Year/W/course-paper-or-presentation-mileskee/stats22.csv')
write.csv(stats23,'/Users/mileskee7/Desktop/College/Senior Year/W/course-paper-or-presentation-mileskee/stats23.csv')
write.csv(stats24,'/Users/mileskee7/Desktop/College/Senior Year/W/course-paper-or-presentation-mileskee/stats24.csv')
