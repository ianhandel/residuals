library(tidyverse)

set.seed(1)
N <- 5 # samples per group


# generate groups, means and values around means
# also jitter the x axis (group_jittered)
dat <- tibble(
  group = rep(c("A", "B"), each = N),
  group_jittered = if_else(group == "A", 1, 2) +
    rep(seq(-0.2, 0.2, length.out = N), times = 2),
  group_mean = if_else(group == "A", 8, 4),
  value = group_mean + rnorm(N * 2, 0, 1))

# df to store details of group mean lines
groups = dat %>%
  group_by(group, group_mean) %>% 
  summarise(x = min(group_jittered),
            xend = max(group_jittered))

ggplot(dat) +
  aes(x = group_jittered, y = value, colour = group) +
  geom_point() +
  geom_segment(aes(
    x = group_jittered, xend = group_jittered,
    y = value, yend = group_mean), lty = 3) +
  scale_x_continuous(breaks = c(1, 2), labels = c("A", "B")) +
  geom_segment(data = groups,
               aes(x = x, xend = xend,
                   y = group_mean, yend = group_mean)) +
  theme(legend.position = "none") +
  labs(x = "Group")

ggsave("resduals.pdf", height = 6, width = 6)
