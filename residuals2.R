library(tidyverse)

group1 <- rnorm(n = 10)
group2 <- rnorm(n = 10, mean = 5)
dat <- tibble(group1, group2) %>%
  gather(key = group, value = score, group1:group2) %>%
  mutate(group = factor(group)) %>%
  group_by(group) %>% 
  mutate(group_jittered = seq(as.numeric(unique(group)) - 0.2,
                              as.numeric(unique(group)) + 0.2,
                              length.out = n())) %>% 
  mutate(group_mean = mean(score))

groups = dat %>%
  group_by(group, group_mean) %>% 
  summarise(x = min(group_jittered),
            xend = max(group_jittered))

ggplot(dat) +
  aes(x = group_jittered, y = score, colour = group) +
  geom_point() +
  geom_segment(aes(
    x = group_jittered, xend = group_jittered,
    y = score, yend = group_mean), lty = 3) +
  scale_x_continuous(breaks = c(1, 2), labels = levels(dat$group)) +
  geom_segment(data = groups,
               aes(x = x, xend = xend,
                   y = group_mean, yend = group_mean)) +
  theme(legend.position = "none") +
  labs(x = "Group")

ggsave("resduals2.pdf", height = 6, width = 6)

