require (ggplot2)

data(ToothGrowth)

ToothGrowth$dose <- as.factor(ToothGrowth$dose)
names(ToothGrowth) <- c("Len", "Supp", "Dose")
supplements <- c("Orange Juice", "Vitamin C")

byDose <- as.data.frame(t(
  sapply(unique(as.character(ToothGrowth$Dose)), 
         function(Dose) data.frame(Mean=round(mean(ToothGrowth$Len[ToothGrowth$Dose==Dose]), 2), 
                                   StdDev=round(sd(ToothGrowth$Len[ToothGrowth$Dose==Dose]), 2),
                                   N=as.numeric(sum(ToothGrowth$Dose==Dose)),
                                   LowerConfidenceInterval=round(mean(ToothGrowth$Len[ToothGrowth$Dose==Dose]) - qt(0.975, df=sum(ToothGrowth$Dose==Dose)-1) * sd(ToothGrowth$Len[ToothGrowth$Dose==Dose]) / sqrt(sum(ToothGrowth$Dose==Dose)), 2), 
                                   UpperConfidenceInterval=round(mean(ToothGrowth$Len[ToothGrowth$Dose==Dose]) + qt(0.975, df=sum(ToothGrowth$Dose==Dose)-1) * sd(ToothGrowth$Len[ToothGrowth$Dose==Dose]) / sqrt(sum(ToothGrowth$Dose==Dose)), 2)
         ))
))

bySupplement <- as.data.frame(t(
  sapply(unique(as.character(ToothGrowth$Supp)), 
         function(Supplement) data.frame(Mean=round(mean(ToothGrowth$Len[ToothGrowth$Supp==Supplement]), 2), 
                                         StdDev=round(sd(ToothGrowth$Len[ToothGrowth$Supp==Supplement]), 2),
                                         N=as.numeric(sum(ToothGrowth$Supp==Supplement)),
                                         LowerConfidenceInterval=round(mean(ToothGrowth$Len[ToothGrowth$Supp==Supplement]) - qt(0.975, df=sum(ToothGrowth$Supp==Supplement)-1) * sd(ToothGrowth$Len[ToothGrowth$Supp==Supplement]) / sqrt(sum(ToothGrowth$Supp==Supplement)), 2), 
                                         UpperConfidenceInterval=round(mean(ToothGrowth$Len[ToothGrowth$Supp==Supplement]) + qt(0.975, df=sum(ToothGrowth$Supp==Supplement)-1) * sd(ToothGrowth$Len[ToothGrowth$Supp==Supplement]) / sqrt(sum(ToothGrowth$Supp==Supplement)), 2)
         ))
))

g <- ggplot(ToothGrowth, aes(rep(1:(length(Len)/2), 2), Len, colour=Dose)) + 
     geom_point(size = 3) + 
     facet_grid(. ~ Supp, labeller=function(var, value) supplements[value]) + 
     scale_x_discrete(name="", labels="") +
     scale_y_discrete(name="Tooth Length")

rownames(bySupplement) <- c("Vitamin C", "Orange Juice")


print(g)
print(byDose)
print(bySupplement)

VitaminC <- bySupplement["Vitamin C",]
OrangeJuice <- bySupplement["Orange Juice",]

sp <- sqrt(((VitaminC$N[[1]]-1)*VitaminC$StdDev[[1]]^2+(OrangeJuice$N[[1]]-1)*OrangeJuice$StdDev[[1]]^2) / (VitaminC$N[[1]] + OrangeJuice$N[[1]] - 2))
ci <- OrangeJuice$Mean[[1]] - VitaminC$Mean[[1]] + c(-1, 1) * qt(0.975, VitaminC$N[[1]] + OrangeJuice$N[[1]] - 2) * sp * sqrt(1 / VitaminC$N[[1]] + 1 / OrangeJuice$N[[1]])
print(ci)

LowestDose <- byDose[1,]
MediumDose <- byDose[2,]
HighestDose <- byDose[3,]

sp <- sqrt(((HighestDose$N[[1]]-1)*HighestDose$StdDev[[1]]^2+(LowestDose$N[[1]]-1)*LowestDose$StdDev[[1]]^2) / (HighestDose$N[[1]] + LowestDose$N[[1]] - 2))
ci <- HighestDose$Mean[[1]] - LowestDose$Mean[[1]] + c(-1, 1) * qt(0.975, HighestDose$N[[1]] + LowestDose$N[[1]] - 2) * sp * sqrt(1 / HighestDose$N[[1]] + 1 / LowestDose$N[[1]])
print(ci)

sp <- sqrt(((HighestDose$N[[1]]-1)*HighestDose$StdDev[[1]]^2+(MediumDose$N[[1]]-1)*MediumDose$StdDev[[1]]^2) / (HighestDose$N[[1]] + MediumDose$N[[1]] - 2))
ci <- HighestDose$Mean[[1]] - MediumDose$Mean[[1]] + c(-1, 1) * qt(0.975, HighestDose$N[[1]] + MediumDose$N[[1]] - 2) * sp * sqrt(1 / HighestDose$N[[1]] + 1 / MediumDose$N[[1]])
print(ci)

