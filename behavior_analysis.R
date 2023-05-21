library(SpadeR)
library(iNEXT)
library(EMT)
library(stats)
library(vegan)
library(ggplot2)
library(fossil)
library(dplyr)
library(viridis)
library(ggpattern)

#organizing data
data <- matrix(c(2, 31, 40, 1, 0, 11, 26, 11, 0, 0, 9, 40, 8, 0, 0, 0, 12,2,0, 0
), 5, 4,
dimnames = list(Category = c("clus1", "clus2", "clus3", "clus4", "clus5"),
                Behavior = c("Leaf harvesting", "Hygiene and prophylaxis", "Fungal gardening", "Brood care")))

#expected values come from summing total observations for each clusters for the focal behaviors and dividing by the total observations for the clusters 
#leaf cutting
multinomial.test(c(2, 31, 40, 1, 0) , c(0.113989637, 0.564766839, 0.316062176, 0.005181347, 0))

#social contact
multinomial.test(c(5, 28, 34, 1, 0) , c(0.113989637, 0.564766839, 0.316062176, 0.005181347, 0))

#hygiene
multinomial.test(c(11, 26, 11, 0, 0) , c(0.113989637, 0.564766839, 0.316062176, 0.005181347, 0))

#fungus
multinomial.test(c(9, 40, 8, 0, 0) , c(0.113989637, 0.564766839, 0.316062176, 0.005181347, 0))

#brood care
multinomial.test(c(0, 12, 2, 0, 0) , c(0.113989637, 0.564766839, 0.316062176, 0.005181347, 0))

#adjusting multinomial test p values for multiple comparisons
p.adjust(c(0.0001,  0.0988, 0.0225, 0.1715), method = "fdr")

#pairwise comparisons

#leaf cutting clus 5
binom.test(0, 74, 0, alternative="two.sided")

#leaf cutting clus 4
binom.test(2, 74, 0.005181347, alternative="two.sided")

#leaf cutting clus 3
binom.test(40, 74, 0.316062176, alternative="two.sided")

#leaf cutting clus 2
binom.test(31, 74, 0.564766839, alternative="two.sided")

#leaf cutting clus 1
binom.test(2, 74, 0.113989637, alternative="two.sided")

#######

#social contact clus 5
binom.test(0, 68, 0, alternative="two.sided")

#social contact clus 4
binom.test(2, 68, 0.005181347, alternative="two.sided")

#social contact clus 3
binom.test(40, 68, 0.316062176, alternative="two.sided")

#social contact clus 2
binom.test(31, 68, 0.564766839, alternative="two.sided")

#social contact clus 1
binom.test(2, 68, 0.113989637, alternative="two.sided")

#######

#hygiene clus 5
binom.test(0, 48, 0, alternative="two.sided")

#hygiene clus 4
binom.test(0, 48, 0.005181347, alternative="two.sided")

#hygiene clus 3
binom.test(11, 48, 0.316062176, alternative="two.sided")

#hygiene clus 2
binom.test(26, 48, 0.564766839, alternative="two.sided")

#hygiene clus 1
binom.test(11, 48, 0.113989637, alternative="two.sided")

############

#fungus clus 4
binom.test(0, 57, 0.005181347, alternative="two.sided")

#fungus clus 3
binom.test(8, 57, 0.316062176, alternative="two.sided")

#fungus clus 2
binom.test(40, 57, 0.564766839, alternative="two.sided")

#fungus clus 1
binom.test(9, 57, 0.113989637, alternative="two.sided")

############

#brood clus 4
binom.test(0, 14, 0.005181347, alternative="two.sided")

#brood clus 3
binom.test(2, 14, 0.316062176, alternative="two.sided")

#brood clus 2
binom.test(12, 14, 0.564766839, alternative="two.sided")

#brood clus 1
binom.test(0, 14, 0.113989637, alternative="two.sided")

#adjusting for multiple comparisons, no social contact
library(stats)
binom_adjusted_ps <- c(#leaf cutting clus 5
  binom.test(0, 74, 0, alternative="two.sided")$p.value,
  
  #leaf cutting clus 4
  binom.test(2, 74, 0.005181347, alternative="two.sided")$p.value,
  
  #leaf cutting clus 3
  binom.test(40, 74, 0.316062176, alternative="two.sided")$p.value,
  
  #leaf cutting clus 2
  binom.test(31, 74, 0.564766839, alternative="two.sided")$p.value,
  
  #leaf cutting clus 1
  binom.test(2, 74, 0.113989637, alternative="two.sided")$p.value,
  
  
  #######
  
  #hygiene clus 5
  binom.test(0, 48, 0, alternative="two.sided")$p.value,
  
  #hygiene clus 4
  binom.test(0, 48, 0.005181347, alternative="two.sided")$p.value,
  
  #hygiene clus 3
  binom.test(11, 48, 0.316062176, alternative="two.sided")$p.value,
  
  #hygiene clus 2
  binom.test(26, 48, 0.564766839, alternative="two.sided")$p.value,
  
  #hygiene clus 1
  binom.test(11, 48, 0.113989637, alternative="two.sided")$p.value,
  
  ############
  
  #fungus clus 5
  binom.test(0, 57, 0, alternative="two.sided")$p.value,
  
  #fungus clus 4
  binom.test(0, 57, 0.005181347, alternative="two.sided")$p.value,
  
  #fungus clus 3
  binom.test(8, 57, 0.316062176, alternative="two.sided")$p.value,
  
  #fungus clus 2
  binom.test(40, 57, 0.564766839, alternative="two.sided")$p.value,
  
  #fungus clus 1
  binom.test(9, 57, 0.113989637, alternative="two.sided")$p.value,
  
  ############
  
  #brood clus 5
  binom.test(0, 14, 0, alternative="two.sided")$p.value,
  
  #brood clus 4
  binom.test(0, 14, 0.005181347, alternative="two.sided")$p.value,
  
  #brood clus 3
  binom.test(2, 14, 0.316062176, alternative="two.sided")$p.value,
  
  #brood clus 2
  binom.test(12, 14, 0.564766839, alternative="two.sided")$p.value,
  
  #brood clus 1
  binom.test(0, 14, 0.113989637, alternative="two.sided")$p.value )

#adjusting binomial test p values for multiple comparisons
p.adjust(binom_adjusted_ps, method = "fdr")

#fossil M/H similarity index

#0.81 1.57
morisita.horn(c(2, 11, 9, 0), c(31, 26, 40, 12))

#0.81 2.45
morisita.horn(c(2, 11, 9, 0), c(40, 11, 8, 2))

#0.81 3.51
morisita.horn(c(2, 11, 9, 0), c(1, 0, 0, 0))

#0.81 >3.51
morisita.horn(c(2, 11, 9, 0), c(1, 0, 0, 0))

#1.57 2.45
morisita.horn(c(31, 26, 40, 12), c(40, 11, 8, 2))

#1.57 3.51
morisita.horn(c(31, 26, 40, 12), c(1, 0, 0, 0))

#1.57 >3.51
morisita.horn(c(31, 26, 40, 12), c(0, 0, 0, 0))

#2.45 3.51
morisita.horn(c(40, 11, 8, 2), c(1, 0, 0, 0))

#2.45 >3.51
morisita.horn(c(40, 11, 8, 2), c(0, 0, 0, 0))

#3.51 >3.51
morisita.horn(c(1, 0, 0, 0), c(0, 0, 0, 0))

#vegan simpson diversity
diversity(data, index = "shannon", MARGIN = 1, base = exp(1))


####
#RPTP graph
RPTP <- read.csv("C:/Users/imura/Documents/grad_5/behavior_morphology_paper/supplementary material/Supplementary Table 4 RPTP values for all worker groups and task categories.csv")
RPTP$Behavior.Category <- factor(RPTP$Behavior.Category, levels = c('Leaf harvesting',	'Hygiene and prophylaxis',	'Fungal gardening',	'Brood care'))
RPTP$Category <- factor(RPTP$Category, levels = c('clus1', 'clus2', 'clus3','clus4', 'clus5'))
ggplot(RPTP, aes(factor(Behavior.Category), rij, fill = Category)) + 
  geom_bar(stat="identity", position = "dodge") + 
  scale_fill_brewer(palette = "Set1") + 
  theme(text = element_text(size=15)) +
  theme_bw()

#plotting histograms of behavior
df <- read.csv("C:/Users/imura/Documents/grad_5/behavior_morphology_paper/IULIAN_OPTIMAL_SIZE_RECODED5_behavior_prelim3_raw.csv", sep = ",", header = TRUE, stringsAsFactors = FALSE)
a_brood_care <- data_frame(df$Head.Width[which(df$Behavior.Category=="Brood care", )])
colnames(a_brood_care) <- c("length")
a_brood_care$cat <- "a_brood_care"
d_fungus <- data_frame(df$Head.Width[which(df$Behavior.Category=="Fungal alteration" | df$Behavior.Category=="Leaf processing")])
colnames(d_fungus) <- c("length")
d_fungus$cat <- "d_fungus"
c_hygiene <- data_frame(df$Head.Width[which(df$Behavior.Category=="Hygiene and prophylaxis", )])
colnames(c_hygiene) <- c("length")
c_hygiene$cat <- "c_hygiene"
b_process_leaf <- data_frame(df$Head.Width[which(df$Behavior.Category=="Leaf cutting" | df$Behavior.Category=="Leaf transport", )])
colnames(b_process_leaf) <- c("length")
b_process_leaf$cat <- "b_process_leaf"

hists <- rbind(a_brood_care, b_process_leaf, c_hygiene, d_fungus)
ggplot(hists, aes(hists$length, fill = hists$cat)) + geom_density(alpha = 0.4, color = NA) + theme_bw()+ scale_fill_viridis(discrete=TRUE, option="plasma")



