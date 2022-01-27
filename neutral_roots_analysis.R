library(ggrepel)
library(gridExtra)
library(lme4)
library(lmerTest)
library(tidyverse)

#########################
# Load and process data #
#########################

# The 'IsSuffixBack' column is calculated based on suffixed tokens, but is
# duplicated across both unsuffixed and suffixed tokens. We use this value
# to look for a relationship between speakers' suffix choices and their
# acoustic realization of neutral root vowels in both suffixed and unsuffixed
# tokens
#setwd('/your/directory/goes/here/')
setwd('C:/Users/conno/Dropbox/ling/Dissertation/code/ch_5/tidy4amp')
neutral_roots <- read_csv('neutral_roots.csv')

unsuffixed_roots <- neutral_roots %>%
  filter(!HasSuffix)

suffixed_roots <- neutral_roots %>%
  filter(HasSuffix) %>%
  mutate(IsSuffixBackFactor = as_factor(IsSuffixBack))

# Get mean F2 and proportion of back responses for each root for plotting
agg_unsuffixed_roots <- unsuffixed_roots %>%
  group_by(Word) %>% 
  summarize(
    mean_f2 = mean(F2),
    prop_back = mean(IsSuffixBack, na.rm=TRUE))

####################
# Generate figures #
####################

# Generate summary plot of relationship between mean F2 and overall proportion
# of back suffixes
ggplot(agg_unsuffixed_roots, aes(x=mean_f2, y=prop_back, label=Word)) +
  geom_point(size=5) +
  geom_label_repel(size=6, box.padding=1) +
  geom_smooth(method="lm", se=FALSE, size=2) +
  xlab("Mean F2") +
  ylab("Mean Percent Back") +
  theme(text=element_text(size=20))
ggsave("correlation_plot.png")

# Plot F2 based on place of articulation of following consonant
p1 <- ggplot(unsuffixed_roots, aes(x=reorder(PrecedingPlace, F2, median), y=F2, 
                                   fill=PrecedingPlace)) +
  geom_violin() +
  xlab("Preceding consonant place of articulation") +
  theme(legend.position = "none",
        text=element_text(size=20)) +
  geom_boxplot(width=0.1, outlier.shape = NA)

p2 <- ggplot(unsuffixed_roots, aes(x=reorder(FollowingPlace, F2, median), y=F2, 
                                   fill=FollowingPlace)) +
  geom_violin() +
  xlab("Following consonant place of articulation") +
  theme(legend.position = "none",
        text=element_text(size=20)) +
  geom_boxplot(width=0.1, outlier.shape = NA)

grid.arrange(p1, p2, ncol=2)

ggplot(suffixed_roots, aes(x=IsSuffixBackFactor, y=F2, fill=IsSuffixBack)) +
  geom_violin() +
  geom_boxplot(width=0.1, outlier.shape = NA) +
  xlab("Suffix backness") +
  scale_x_discrete(name="Suffix backness", labels=c("Front", "Back")) +
  theme(legend.position = "none",
        text=element_text(size=20)) +
  ylab("F2 of final root vowel")
ggsave('front_back_suffix_acoustic_plot.png')

##############
# Fit models #
##############

# Unsuffixed roots
m_f2_unsuffixed <- lmer(
  F2 ~ IsSuffixBack + Gender + FollowingPlace + PrecedingPlace + (1|Speaker) + (1|Word), 
  data=unsuffixed_roots
)
summary(m_f2_unsuffixed)

# Suffixed roots
m_f2_suffixed <- lmer(
  F2 ~ IsSuffixBack + Gender + FollowingPlace + PrecedingPlace + (1|Speaker) + (1|Word), 
  data=suffixed_roots
)
summary(m_f2_suffixed)

