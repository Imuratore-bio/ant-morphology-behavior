library(SpadeR)
library(iNEXT)
library(EMT)
library(stats)
library(vegan)
library(ggplot2)
library(fossil)

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
binom.test(0, 74, 0, alternative="greater")

#leaf cutting clus 4
binom.test(2, 74, 0.005181347, alternative="greater")

#leaf cutting clus 3
binom.test(40, 74, 0.316062176, alternative="greater")

#leaf cutting clus 2
binom.test(31, 74, 0.564766839, alternative="greater")

#leaf cutting clus 1
binom.test(2, 74, 0.113989637, alternative="greater")

#######

#social contact clus 5
binom.test(0, 68, 0, alternative="greater")

#social contact clus 4
binom.test(2, 68, 0.005181347, alternative="greater")

#social contact clus 3
binom.test(40, 68, 0.316062176, alternative="greater")

#social contact clus 2
binom.test(31, 68, 0.564766839, alternative="greater")

#social contact clus 1
binom.test(2, 68, 0.113989637, alternative="greater")

#######

#hygiene clus 5
binom.test(0, 48, 0, alternative="greater")

#hygiene clus 4
binom.test(0, 48, 0.005181347, alternative="greater")

#hygiene clus 3
binom.test(11, 48, 0.316062176, alternative="greater")

#hygiene clus 2
binom.test(26, 48, 0.564766839, alternative="greater")

#hygiene clus 1
binom.test(11, 48, 0.113989637, alternative="greater")

############

#fungus clus 4
binom.test(0, 57, 0.005181347, alternative="greater")

#fungus clus 3
binom.test(8, 57, 0.316062176, alternative="greater")

#fungus clus 2
binom.test(40, 57, 0.564766839, alternative="greater")

#fungus clus 1
binom.test(9, 57, 0.113989637, alternative="greater")

############

#brood clus 4
binom.test(0, 14, 0.005181347, alternative="greater")

#brood clus 3
binom.test(2, 14, 0.316062176, alternative="greater")

#brood clus 2
binom.test(12, 14, 0.564766839, alternative="greater")

#brood clus 1
binom.test(0, 14, 0.113989637, alternative="greater")

#ADJUSTED FOR MULTIPLE COMPARISON, no social contact
library(stats)
binom_adjusted_ps <- c(#leaf cutting clus 5
  binom.test(0, 74, 0, alternative="greater")$p.value,
  
  #leaf cutting clus 4
  binom.test(2, 74, 0.005181347, alternative="greater")$p.value,
  
  #leaf cutting clus 3
  binom.test(40, 74, 0.316062176, alternative="greater")$p.value,
  
  #leaf cutting clus 2
  binom.test(31, 74, 0.564766839, alternative="greater")$p.value,
  
  #leaf cutting clus 1
  binom.test(2, 74, 0.113989637, alternative="greater")$p.value,
  
  
  #######
  
  #hygiene clus 5
  binom.test(0, 48, 0, alternative="greater")$p.value,
  
  #hygiene clus 4
  binom.test(0, 48, 0.005181347, alternative="greater")$p.value,
  
  #hygiene clus 3
  binom.test(11, 48, 0.316062176, alternative="greater")$p.value,
  
  #hygiene clus 2
  binom.test(26, 48, 0.564766839, alternative="greater")$p.value,
  
  #hygiene clus 1
  binom.test(11, 48, 0.113989637, alternative="greater")$p.value,
  
  ############
  
  #fungus clus 5
  binom.test(0, 57, 0, alternative="greater")$p.value,
  
  #fungus clus 4
  binom.test(0, 57, 0.005181347, alternative="greater")$p.value,
  
  #fungus clus 3
  binom.test(8, 57, 0.316062176, alternative="greater")$p.value,
  
  #fungus clus 2
  binom.test(40, 57, 0.564766839, alternative="greater")$p.value,
  
  #fungus clus 1
  binom.test(9, 57, 0.113989637, alternative="greater")$p.value,
  
  ############
  
  #brood clus 5
  binom.test(0, 14, 0, alternative="greater")$p.value,
  
  #brood clus 4
  binom.test(0, 14, 0.005181347, alternative="greater")$p.value,
  
  #brood clus 3
  binom.test(2, 14, 0.316062176, alternative="greater")$p.value,
  
  #brood clus 2
  binom.test(12, 14, 0.564766839, alternative="greater")$p.value,
  
  #brood clus 1
  binom.test(0, 14, 0.113989637, alternative="greater")$p.value )



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
RPTP <- read.csv("C:/Users/imura/Documents/grad_5/behavior_morphology_paper/supplementary_material/Supplementary Table 4 RPTP values for all worker groups and task categories.csv")
RPTP$Behavior.Category <- factor(RPTP$Behavior.Category, levels = c('Leaf harvesting',	'Hygiene and prophylaxis',	'Fungal gardening',	'Brood care'))
RPTP$Category <- factor(RPTP$Category, levels = c('clus1', 'clus2', 'clus3','clus4', 'clus5'))
ggplot(RPTP, aes(factor(Behavior.Category), rij, fill = Category)) + 
  geom_bar(stat="identity", position = "dodge") + 
  scale_fill_brewer(palette = "Set1") + 
  theme(text = element_text(size=15)) +
  theme_bw()
