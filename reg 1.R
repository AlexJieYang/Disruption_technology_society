library(fixest)
library(data.table)
data <- read.csv('P4Reg.csv')

library(dplyr)

data <- data %>%
  mutate(
    P_Patent = as.numeric(Patent_Count > 0),
    P_NCT = as.numeric(NCT_Count > 0),
    P_Newsfeed = as.numeric(Newsfeed_Count > 0),
    P_Tweet = as.numeric(Tweet_Count > 0)
  )

reg1<-feglm(P_Patent~Disruption_p|Year+FieldID, data,family = "binomial", se = "hetero")
reg2<-feglm(P_Patent~Disruption_p|Year+FieldID+JournalID, data,family = "binomial", se = "hetero")
reg3<-feglm(P_Patent~Disruption_p+log(Team_Size)+International+InterdisTeam|Year+FieldID+JournalID, data,family = "binomial", se = "hetero")
reg4<-feglm(P_Patent~Disruption_p+log(Team_Size)+International+InterdisTeam+Funding+log(Reference_Count+1)+RaoStirling_Ref|Year+FieldID+JournalID, data,family = "binomial", se = "hetero")
write.csv(etable(reg1,reg2,reg3,reg4) , " reg11.csv")

reg1<-feglm(P_NCT~Disruption_p|Year+FieldID, data,family = "binomial", se = "hetero")
reg2<-feglm(P_NCT~Disruption_p|Year+FieldID+JournalID, data,family = "binomial", se = "hetero")
reg3<-feglm(P_NCT~Disruption_p+log(Team_Size)+International+InterdisTeam|Year+FieldID+JournalID, data,family = "binomial", se = "hetero")
reg4<-feglm(P_NCT~Disruption_p+log(Team_Size)+International+InterdisTeam+Funding+log(Reference_Count+1)+RaoStirling_Ref|Year+FieldID+JournalID, data,family = "binomial", se = "hetero")
write.csv(etable(reg1,reg2,reg3,reg4) , " reg12.csv")

reg1<-feglm(P_Newsfeed~Disruption_p|Year+FieldID, data,family = "binomial", se = "hetero")
reg2<-feglm(P_Newsfeed~Disruption_p|Year+FieldID+JournalID, data,family = "binomial", se = "hetero")
reg3<-feglm(P_Newsfeed~Disruption_p+log(Team_Size)+International+InterdisTeam|Year+FieldID+JournalID, data,family = "binomial", se = "hetero")
reg4<-feglm(P_Newsfeed~Disruption_p+log(Team_Size)+International+InterdisTeam+Funding+log(Reference_Count+1)+RaoStirling_Ref|Year+FieldID+JournalID, data,family = "binomial", se = "hetero")
write.csv(etable(reg1,reg2,reg3,reg4) , " reg13.csv")

reg1<-feglm(P_Tweet~Disruption_p|Year+FieldID, data,family = "binomial", se = "hetero")
reg2<-feglm(P_Tweet~Disruption_p|Year+FieldID+JournalID, data,family = "binomial", se = "hetero")
reg3<-feglm(P_Tweet~Disruption_p+log(Team_Size)+International+InterdisTeam|Year+FieldID+JournalID, data,family = "binomial", se = "hetero")
reg4<-feglm(P_Tweet~Disruption_p+log(Team_Size)+International+InterdisTeam+Funding+log(Reference_Count+1)+RaoStirling_Ref|Year+FieldID+JournalID, data,family = "binomial", se = "hetero")
write.csv(etable(reg1,reg2,reg3,reg4) , " reg14.csv")


reg1<-feglm(P_Patent~log(NDC+1)|Year+FieldID, data,family = "binomial", se = "hetero")
reg2<-feglm(P_Patent~log(NDC+1)|Year+FieldID+JournalID, data,family = "binomial", se = "hetero")
reg3<-feglm(P_Patent~log(NDC+1)+log(Team_Size)+International+InterdisTeam|Year+FieldID+JournalID, data,family = "binomial", se = "hetero")
reg4<-feglm(P_Patent~log(NDC+1)+log(Team_Size)+International+InterdisTeam+Funding+log(Reference_Count+1)+RaoStirling_Ref|Year+FieldID+JournalID, data,family = "binomial", se = "hetero")
write.csv(etable(reg1,reg2,reg3,reg4) , " reg21.csv")

reg1<-feglm(P_NCT~log(NDC+1)|Year+FieldID, data,family = "binomial", se = "hetero")
reg2<-feglm(P_NCT~log(NDC+1)|Year+FieldID+JournalID, data,family = "binomial", se = "hetero")
reg3<-feglm(P_NCT~log(NDC+1)+log(Team_Size)+International+InterdisTeam|Year+FieldID+JournalID, data,family = "binomial", se = "hetero")
reg4<-feglm(P_NCT~log(NDC+1)+log(Team_Size)+International+InterdisTeam+Funding+log(Reference_Count+1)+RaoStirling_Ref|Year+FieldID+JournalID, data,family = "binomial", se = "hetero")
write.csv(etable(reg1,reg2,reg3,reg4) , " reg22.csv")

reg1<-feglm(P_Newsfeed~log(NDC+1)|Year+FieldID, data,family = "binomial", se = "hetero")
reg2<-feglm(P_Newsfeed~log(NDC+1)|Year+FieldID+JournalID, data,family = "binomial", se = "hetero")
reg3<-feglm(P_Newsfeed~log(NDC+1)+log(Team_Size)+International+InterdisTeam|Year+FieldID+JournalID, data,family = "binomial", se = "hetero")
reg4<-feglm(P_Newsfeed~log(NDC+1)+log(Team_Size)+International+InterdisTeam+Funding+log(Reference_Count+1)+RaoStirling_Ref|Year+FieldID+JournalID, data,family = "binomial", se = "hetero")
write.csv(etable(reg1,reg2,reg3,reg4) , " reg23.csv")

reg1<-feglm(P_Tweet~log(NDC+1)|Year+FieldID, data,family = "binomial", se = "hetero")
reg2<-feglm(P_Tweet~log(NDC+1)|Year+FieldID+JournalID, data,family = "binomial", se = "hetero")
reg3<-feglm(P_Tweet~log(NDC+1)+log(Team_Size)+International+InterdisTeam|Year+FieldID+JournalID, data,family = "binomial", se = "hetero")
reg4<-feglm(P_Tweet~log(NDC+1)+log(Team_Size)+International+InterdisTeam+Funding+log(Reference_Count+1)+RaoStirling_Ref|Year+FieldID+JournalID, data,family = "binomial", se = "hetero")
write.csv(etable(reg1,reg2,reg3,reg4) , " reg24.csv")



reg1<-feglm(P_Patent~Disruption5_p|Year+FieldID, data,family = "binomial", se = "hetero")
reg2<-feglm(P_Patent~Disruption5_p|Year+FieldID+JournalID, data,family = "binomial", se = "hetero")
reg3<-feglm(P_Patent~Disruption5_p+log(Team_Size)+International+InterdisTeam|Year+FieldID+JournalID, data,family = "binomial", se = "hetero")
reg4<-feglm(P_Patent~Disruption5_p+log(Team_Size)+International+InterdisTeam+Funding+log(Reference_Count+1)+RaoStirling_Ref|Year+FieldID+JournalID, data,family = "binomial", se = "hetero")
write.csv(etable(reg1,reg2,reg3,reg4) , " Robust/1/reg11.csv")

reg1<-feglm(P_NCT~Disruption5_p|Year+FieldID, data,family = "binomial", se = "hetero")
reg2<-feglm(P_NCT~Disruption5_p|Year+FieldID+JournalID, data,family = "binomial", se = "hetero")
reg3<-feglm(P_NCT~Disruption5_p+log(Team_Size)+International+InterdisTeam|Year+FieldID+JournalID, data,family = "binomial", se = "hetero")
reg4<-feglm(P_NCT~Disruption5_p+log(Team_Size)+International+InterdisTeam+Funding+log(Reference_Count+1)+RaoStirling_Ref|Year+FieldID+JournalID, data,family = "binomial", se = "hetero")
write.csv(etable(reg1,reg2,reg3,reg4) , " Robust/1/reg12.csv")

reg1<-feglm(P_Newsfeed~Disruption5_p|Year+FieldID, data,family = "binomial", se = "hetero")
reg2<-feglm(P_Newsfeed~Disruption5_p|Year+FieldID+JournalID, data,family = "binomial", se = "hetero")
reg3<-feglm(P_Newsfeed~Disruption5_p+log(Team_Size)+International+InterdisTeam|Year+FieldID+JournalID, data,family = "binomial", se = "hetero")
reg4<-feglm(P_Newsfeed~Disruption5_p+log(Team_Size)+International+InterdisTeam+Funding+log(Reference_Count+1)+RaoStirling_Ref|Year+FieldID+JournalID, data,family = "binomial", se = "hetero")
write.csv(etable(reg1,reg2,reg3,reg4) , " Robust/1/reg13.csv")

reg1<-feglm(P_Tweet~Disruption5_p|Year+FieldID, data,family = "binomial", se = "hetero")
reg2<-feglm(P_Tweet~Disruption5_p|Year+FieldID+JournalID, data,family = "binomial", se = "hetero")
reg3<-feglm(P_Tweet~Disruption5_p+log(Team_Size)+International+InterdisTeam|Year+FieldID+JournalID, data,family = "binomial", se = "hetero")
reg4<-feglm(P_Tweet~Disruption5_p+log(Team_Size)+International+InterdisTeam+Funding+log(Reference_Count+1)+RaoStirling_Ref|Year+FieldID+JournalID, data,family = "binomial", se = "hetero")
write.csv(etable(reg1,reg2,reg3,reg4) , " Robust/1/reg14.csv")


reg1<-feglm(P_Patent~log(D5+1)|Year+FieldID, data,family = "binomial", se = "hetero")
reg2<-feglm(P_Patent~log(D5+1)|Year+FieldID+JournalID, data,family = "binomial", se = "hetero")
reg3<-feglm(P_Patent~log(D5+1)+log(Team_Size)+International+InterdisTeam|Year+FieldID+JournalID, data,family = "binomial", se = "hetero")
reg4<-feglm(P_Patent~log(D5+1)+log(Team_Size)+International+InterdisTeam+Funding+log(Reference_Count+1)+RaoStirling_Ref|Year+FieldID+JournalID, data,family = "binomial", se = "hetero")
write.csv(etable(reg1,reg2,reg3,reg4) , " Robust/1/reg21.csv")

reg1<-feglm(P_NCT~log(D5+1)|Year+FieldID, data,family = "binomial", se = "hetero")
reg2<-feglm(P_NCT~log(D5+1)|Year+FieldID+JournalID, data,family = "binomial", se = "hetero")
reg3<-feglm(P_NCT~log(D5+1)+log(Team_Size)+International+InterdisTeam|Year+FieldID+JournalID, data,family = "binomial", se = "hetero")
reg4<-feglm(P_NCT~log(D5+1)+log(Team_Size)+International+InterdisTeam+Funding+log(Reference_Count+1)+RaoStirling_Ref|Year+FieldID+JournalID, data,family = "binomial", se = "hetero")
write.csv(etable(reg1,reg2,reg3,reg4) , " Robust/1/reg22.csv")

reg1<-feglm(P_Newsfeed~log(D5+1)|Year+FieldID, data,family = "binomial", se = "hetero")
reg2<-feglm(P_Newsfeed~log(D5+1)|Year+FieldID+JournalID, data,family = "binomial", se = "hetero")
reg3<-feglm(P_Newsfeed~log(D5+1)+log(Team_Size)+International+InterdisTeam|Year+FieldID+JournalID, data,family = "binomial", se = "hetero")
reg4<-feglm(P_Newsfeed~log(D5+1)+log(Team_Size)+International+InterdisTeam+Funding+log(Reference_Count+1)+RaoStirling_Ref|Year+FieldID+JournalID, data,family = "binomial", se = "hetero")
write.csv(etable(reg1,reg2,reg3,reg4) , " Robust/1/reg23.csv")

reg1<-feglm(P_Tweet~log(D5+1)|Year+FieldID, data,family = "binomial", se = "hetero")
reg2<-feglm(P_Tweet~log(D5+1)|Year+FieldID+JournalID, data,family = "binomial", se = "hetero")
reg3<-feglm(P_Tweet~log(D5+1)+log(Team_Size)+International+InterdisTeam|Year+FieldID+JournalID, data,family = "binomial", se = "hetero")
reg4<-feglm(P_Tweet~log(D5+1)+log(Team_Size)+International+InterdisTeam+Funding+log(Reference_Count+1)+RaoStirling_Ref|Year+FieldID+JournalID, data,family = "binomial", se = "hetero")
write.csv(etable(reg1,reg2,reg3,reg4) , " Robust/1/reg24.csv")

reg1<-feglm(P_Patent~Disruption10_p|Year+FieldID, data,family = "binomial", se = "hetero")
reg2<-feglm(P_Patent~Disruption10_p|Year+FieldID+JournalID, data,family = "binomial", se = "hetero")
reg3<-feglm(P_Patent~Disruption10_p+log(Team_Size)+International+InterdisTeam|Year+FieldID+JournalID, data,family = "binomial", se = "hetero")
reg4<-feglm(P_Patent~Disruption10_p+log(Team_Size)+International+InterdisTeam+Funding+log(Reference_Count+1)+RaoStirling_Ref|Year+FieldID+JournalID, data,family = "binomial", se = "hetero")
write.csv(etable(reg1,reg2,reg3,reg4) , " Robust/2/reg11.csv")

reg1<-feglm(P_NCT~Disruption10_p|Year+FieldID, data,family = "binomial", se = "hetero")
reg2<-feglm(P_NCT~Disruption10_p|Year+FieldID+JournalID, data,family = "binomial", se = "hetero")
reg3<-feglm(P_NCT~Disruption10_p+log(Team_Size)+International+InterdisTeam|Year+FieldID+JournalID, data,family = "binomial", se = "hetero")
reg4<-feglm(P_NCT~Disruption10_p+log(Team_Size)+International+InterdisTeam+Funding+log(Reference_Count+1)+RaoStirling_Ref|Year+FieldID+JournalID, data,family = "binomial", se = "hetero")
write.csv(etable(reg1,reg2,reg3,reg4) , " Robust/2/reg12.csv")

reg1<-feglm(P_Newsfeed~Disruption10_p|Year+FieldID, data,family = "binomial", se = "hetero")
reg2<-feglm(P_Newsfeed~Disruption10_p|Year+FieldID+JournalID, data,family = "binomial", se = "hetero")
reg3<-feglm(P_Newsfeed~Disruption10_p+log(Team_Size)+International+InterdisTeam|Year+FieldID+JournalID, data,family = "binomial", se = "hetero")
reg4<-feglm(P_Newsfeed~Disruption10_p+log(Team_Size)+International+InterdisTeam+Funding+log(Reference_Count+1)+RaoStirling_Ref|Year+FieldID+JournalID, data,family = "binomial", se = "hetero")
write.csv(etable(reg1,reg2,reg3,reg4) , " Robust/2/reg13.csv")

reg1<-feglm(P_Tweet~Disruption10_p|Year+FieldID, data,family = "binomial", se = "hetero")
reg2<-feglm(P_Tweet~Disruption10_p|Year+FieldID+JournalID, data,family = "binomial", se = "hetero")
reg3<-feglm(P_Tweet~Disruption10_p+log(Team_Size)+International+InterdisTeam|Year+FieldID+JournalID, data,family = "binomial", se = "hetero")
reg4<-feglm(P_Tweet~Disruption10_p+log(Team_Size)+International+InterdisTeam+Funding+log(Reference_Count+1)+RaoStirling_Ref|Year+FieldID+JournalID, data,family = "binomial", se = "hetero")
write.csv(etable(reg1,reg2,reg3,reg4) , " Robust/2/reg14.csv")


reg1<-feglm(P_Patent~log(D10+1)|Year+FieldID, data,family = "binomial", se = "hetero")
reg2<-feglm(P_Patent~log(D10+1)|Year+FieldID+JournalID, data,family = "binomial", se = "hetero")
reg3<-feglm(P_Patent~log(D10+1)+log(Team_Size)+International+InterdisTeam|Year+FieldID+JournalID, data,family = "binomial", se = "hetero")
reg4<-feglm(P_Patent~log(D10+1)+log(Team_Size)+International+InterdisTeam+Funding+log(Reference_Count+1)+RaoStirling_Ref|Year+FieldID+JournalID, data,family = "binomial", se = "hetero")
write.csv(etable(reg1,reg2,reg3,reg4) , " Robust/2/reg21.csv")

reg1<-feglm(P_NCT~log(D10+1)|Year+FieldID, data,family = "binomial", se = "hetero")
reg2<-feglm(P_NCT~log(D10+1)|Year+FieldID+JournalID, data,family = "binomial", se = "hetero")
reg3<-feglm(P_NCT~log(D10+1)+log(Team_Size)+International+InterdisTeam|Year+FieldID+JournalID, data,family = "binomial", se = "hetero")
reg4<-feglm(P_NCT~log(D10+1)+log(Team_Size)+International+InterdisTeam+Funding+log(Reference_Count+1)+RaoStirling_Ref|Year+FieldID+JournalID, data,family = "binomial", se = "hetero")
write.csv(etable(reg1,reg2,reg3,reg4) , " Robust/2/reg22.csv")

reg1<-feglm(P_Newsfeed~log(D10+1)|Year+FieldID, data,family = "binomial", se = "hetero")
reg2<-feglm(P_Newsfeed~log(D10+1)|Year+FieldID+JournalID, data,family = "binomial", se = "hetero")
reg3<-feglm(P_Newsfeed~log(D10+1)+log(Team_Size)+International+InterdisTeam|Year+FieldID+JournalID, data,family = "binomial", se = "hetero")
reg4<-feglm(P_Newsfeed~log(D10+1)+log(Team_Size)+International+InterdisTeam+Funding+log(Reference_Count+1)+RaoStirling_Ref|Year+FieldID+JournalID, data,family = "binomial", se = "hetero")
write.csv(etable(reg1,reg2,reg3,reg4) , " Robust/2/reg23.csv")

reg1<-feglm(P_Tweet~log(D10+1)|Year+FieldID, data,family = "binomial", se = "hetero")
reg2<-feglm(P_Tweet~log(D10+1)|Year+FieldID+JournalID, data,family = "binomial", se = "hetero")
reg3<-feglm(P_Tweet~log(D10+1)+log(Team_Size)+International+InterdisTeam|Year+FieldID+JournalID, data,family = "binomial", se = "hetero")
reg4<-feglm(P_Tweet~log(D10+1)+log(Team_Size)+International+InterdisTeam+Funding+log(Reference_Count+1)+RaoStirling_Ref|Year+FieldID+JournalID, data,family = "binomial", se = "hetero")
write.csv(etable(reg1,reg2,reg3,reg4) , " Robust/2/reg24.csv")