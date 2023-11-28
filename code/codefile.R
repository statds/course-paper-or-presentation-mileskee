library(dplyr)
transfer24 <- read.csv('/Users/mileskee7/Desktop/College/Senior Year/W/course-paper-or-presentation-mileskee/data/transfer24.csv')
#24 is guys who transferred this year
transfer23 <- read.csv('/Users/mileskee7/Desktop/College/Senior Year/W/course-paper-or-presentation-mileskee/data/transfer23.csv')
transfer22 <- read.csv('/Users/mileskee7/Desktop/College/Senior Year/W/course-paper-or-presentation-mileskee/data/transfer22.csv')
transfer21 <- read.csv('/Users/mileskee7/Desktop/College/Senior Year/W/course-paper-or-presentation-mileskee/data/transfer21.csv')
transfer21 <- transfer21 %>% select('player_name','team','conf','Min_per','ORtg','usg','eFG','ORB_per','DRB_per','AST_per',
                                    'TO_per','blk_per','stl_per','ftr','yr','ht','new.school','dbpm')
transfer22 <- transfer22 %>% select('player_name','team','conf','Min_per','ORtg','usg','eFG','ORB_per','DRB_per','AST_per',
                                    'TO_per','blk_per','stl_per','ftr','yr','ht','new.school','dbpm')
transfer23 <- transfer23 %>% select('player_name','team','conf','Min_per','ORtg','usg','eFG','ORB_per','DRB_per','AST_per',
                                    'TO_per','blk_per','stl_per','ftr','yr','ht','new.school','dbpm')
transfer24 <- transfer24 %>% select('player_name','team','conf','Min_per','ORtg','usg','eFG','ORB_per','DRB_per','AST_per',
                                    'TO_per','blk_per','stl_per','ftr','yr','ht','new.school','dbpm')
stats21 <- read.csv('/Users/mileskee7/Desktop/College/Senior Year/W/course-paper-or-presentation-mileskee/data/stats21.csv')
stats22 <- read.csv('/Users/mileskee7/Desktop/College/Senior Year/W/course-paper-or-presentation-mileskee/data/stats22.csv')
stats23 <- read.csv('/Users/mileskee7/Desktop/College/Senior Year/W/course-paper-or-presentation-mileskee/data/stats23.csv')
stats24 <- read.csv('/Users/mileskee7/Desktop/College/Senior Year/W/course-paper-or-presentation-mileskee/data/stats24.csv')
full21 <- merge(transfer21,stats21,by.x='player_name',by.y='Player')
full22 <- merge(transfer22,stats22,by.x='player_name',by.y='Player')
full23 <- merge(transfer23,stats23,by.x='player_name',by.y='Player')
fulldata <- rbind(full21,full22,full23)
write.csv(fulldata,'/Users/mileskee7/Desktop/College/Senior Year/W/course-paper-or-presentation-mileskee/data/fulldata.csv')
