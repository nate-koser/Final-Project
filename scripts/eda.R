##load libraries##
library(here)
source(here("scripts", "libs.R"))

##load tidy data##
data <- read_csv(here("data_clean", "data_clean.csv"))

#isolate the relevant data for [u] and [i] vowels

udata <- data %>%
  filter(., TextGridLabel %in% c("u2", "U2", "u5", "U5",
                                 "u20", "U20", "u50", "U50"))%>%
  filter(., Filename != "M1A74", Filename != "M1B25")      #two files for which f0 data did not show up

idata <- data %>%
  filter(., TextGridLabel %in% c("i2", "I2", "i5", "I5",
                                 "i20", "I20", "i50", "I50"))

##basic statistical information##

u_stats <- udata %>%
  group_by(., Stress) %>%
  summarize(., dur = mean(NormalizedDuration), dur_sd = sd(NormalizedDuration),
            int = mean(Intensity), int_sd = sd(Intensity),
            F1 = mean(F1_midpoint), F1_sd = sd(F1_midpoint),
            F2 = mean(F2_midpoint), F2_sd = sd(F2_midpoint),
            F0 = mean(f0_midpoint), F0_sd = sd(f0_midpoint))


i_stats <- idata %>%
  group_by(., Stress) %>%
  summarize(., dur = mean(NormalizedDuration), dur_sd = sd(NormalizedDuration),
            int = mean(Intensity), int_sd = sd(Intensity),
            F1 = mean(F1_midpoint), F1_sd = sd(F1_midpoint),
            F2 = mean(F2_midpoint), F2_sd = sd(F2_midpoint),
            F0 = mean(f0_midpoint), F0_sd = sd(f0_midpoint))


##plots and tables##

#duration for stressed and unstressed [i] and [u]
durplot <- data %>%
  filter(., TextGridLabel %in% c("i2", "I2", "i20", "I20", "i5", "I5", "i50", "I50",
                                 "u2", "U2", "u20", "U20", "u5", "U5", "u50", "U50"
  )) %>%
  mutate(., newnorm = NormalizedDuration * 1000) %>%
  ggplot(., aes(Stress, newnorm)) +
  facet_grid(.~Vowel)+
  stat_summary(fun.y = mean, geom = "point", size = 4)+
  stat_summary(fun.data = mean_se, geom = "errorbar", width = .2)+
  theme(text = element_text(size = 14)) +
  theme(axis.text = element_text(size = 14)) +
  ylim(30,90)+
  labs(x = "Predicted Stress (N=unstressed, Y=stressed)", y = "Mean Duration (ms)")

udur <- udata %>%
  group_by(., Stress, Frame, Syllnum) %>%
  summarize(., Mean = mean(NormalizedDuration)*1000, SD = sd(NormalizedDuration)*1000)


idur <- idata %>%
  group_by(., Stress, Frame, Syllnum) %>%
  summarize(., Mean = mean(NormalizedDuration)*1000, SD = sd(NormalizedDuration)*1000)

#intensity for stressed and unstressed [i] and [u]
intplot <- data %>%
  filter(., TextGridLabel %in% c("i2", "I2", "i20", "I20", "i5", "I5", "i50", "I50",
                                 "u2", "U2", "u20", "U20", "u5", "U5", "u50", "U50"
  )) %>%
  ggplot(., aes(Stress, Intensity)) +
  facet_grid(.~Vowel)+
  stat_summary(fun.y = mean, geom = "point", size = 4)+
  stat_summary(fun.data = mean_se, geom = "errorbar", width = .2)+
  theme(text = element_text(size = 14)) +
  theme(axis.text = element_text(size = 14)) +
  ylim(60,80)+
  labs(x = "Predicted Stress (N=unstressed, Y=stressed)", y = "Mean Intensity (dB)")


uint <- udata %>%
  group_by(., Stress, Frame, Syllnum) %>%
  summarize(., Mean = mean(Intensity), SD = sd(Intensity))

iint <- idata %>%
  group_by(., Stress, Frame, Syllnum) %>%
  summarize(., Mean = mean(Intensity), SD = sd(Intensity))

#F1/F2 for [u]
vowelplot_u_3 <- udata %>%
  filter(., TextGridLabel %in% c("u2", "u5", "U2", "U5")) %>%
  ggplot(., aes(F2_midpoint, F1_midpoint, color = TextGridLabel))+
  geom_point()+
  stat_ellipse(inherit.aes = TRUE)+
  labs(color = "Vowels by Position", x = "F2 (Hz)", y = "F1 (Hz)")+
  xlim(2800,0)+
  ylim(1000,0)

vowelplot_u_4 <- udata %>%
  filter(., TextGridLabel %in% c("u20", "u50", "U20", "U50")) %>%
  ggplot(., aes(F2_midpoint, F1_midpoint, color = TextGridLabel))+
  geom_point()+
  stat_ellipse(inherit.aes = TRUE)+
  labs(color = "Vowels by Position", title = "F2/F1 Plot, [u] Vowels (Tetrasyllabic)")+
  xlim(2800,0)+
  ylim(1000,0)


uvow <- udata %>%
  group_by(., Stress, Frame, Syllnum) %>%
  summarize(., "F1 Mean" = mean(F1_midpoint), "F1 SD" = sd(F1_midpoint),
               "F2 Mean" = mean(F2_midpoint), "F2 SD" = sd(F2_midpoint))

#F1/F2 for [i]
vowelplot_i_3 <- idata %>%
  filter(., TextGridLabel %in% c("i2", "i5", "I2", "I5")) %>%
  ggplot(., aes(F2_midpoint, F1_midpoint, color = TextGridLabel))+
  geom_point()+
  stat_ellipse(inherit.aes = TRUE)+
  labs(color = "Vowels by Position", x = "F2 (Hz)", y = "F1 (Hz)")+
  xlim(2800,0)+
  ylim(1000,0)


vowelplot_i_4 <- idata %>%
  filter(., TextGridLabel %in% c("i20", "i50", "I20", "I50")) %>%
  ggplot(., aes(F2_midpoint, F1_midpoint, color = TextGridLabel))+
  geom_point()+
  stat_ellipse(inherit.aes = TRUE)+
  labs(color = "Vowels by Position", title = "F2/F1 Plot, [i] Vowels (Tetrasyllabic)")+
  xlim(2800,0)+
  ylim(1000,0)

ivow <- idata %>%
  group_by(., Stress, Frame, Syllnum) %>%
  summarize(., "F1 Mean" = mean(F1_midpoint), "F1 SD" = sd(F1_midpoint),
            "F2 Mean" = mean(F2_midpoint), "F2 SD" = sd(F2_midpoint))


#F0 for stressed and unstressed [i] and [u]
f0plot <- data %>%
  filter(., TextGridLabel %in% c("i2", "I2", "i20", "I20", "i5", "I5", "i50", "I50",
                                 "u2", "U2", "u20", "U20", "u5", "U5", "u50", "U50"
  )) %>%
  filter(., Filename != "M1A74", Filename != "M1B25") %>%
  ggplot(., aes(Stress, f0_midpoint)) +
  facet_grid(.~Vowel)+
  stat_summary(fun.y = mean, geom = "point", size = 4)+
  stat_summary(fun.data = mean_se, geom = "errorbar", width = .2)+
  theme(text = element_text(size = 14)) +
  theme(axis.text = element_text(size = 14)) +
  ylim(80,160)+
  labs(x = "Predicted Stress (N=unstressed, Y=stressed)", y = "Mean F0 (Hz)")


uF0 <- udata %>%
  group_by(., Stress, Frame, Syllnum) %>%
  summarize(., Mean = mean(f0_midpoint), SD = sd(f0_midpoint))


iF0 <- idata %>%
  group_by(., Stress, Frame, Syllnum) %>%
  summarize(., Mean = mean(f0_midpoint), SD = sd(f0_midpoint))

#Distribution of F0 by stress, [u] and [i] vowels

uf0_dist <- ggplot(udata) +
  geom_histogram(aes(x = f0_midpoint, fill = Stress), bins = 18)+
  labs(x = "F0 in Hz")

if0_dist <- ggplot(idata) +
  geom_histogram(aes(x = f0_midpoint, fill = Stress), bins = 18)+
  labs(x = "F0 in Hz")

##save all plots to figs folder##

#ggsave(filename = "./figs/durplot.png", plot = durplot,
#       width = 6, height = 4, unit = "in")

#ggsave(filename = "./figs/intplot.png", plot = intplot,
#       width = 6, height = 4, unit = "in")

#ggsave(filename = "./figs/vowelplot_u_3.png", plot = vowelplot_u_3,
#       width = 6, height = 4, unit = "in")

#ggsave(filename = "./figs/vowelplot_u_4.png", plot = vowelplot_u_4,
#       width = 6, height = 4, unit = "in")

#ggsave(filename = "./figs/vowelplot_i_3.png", plot = vowelplot_i_3,
#       width = 6, height = 4, unit = "in")

#ggsave(filename = "./figs/vowelplot_i_4.png", plot = vowelplot_i_4,
#       width = 6, height = 4, unit = "in")

#ggsave(filename = "./figs/f0plot.png", plot = f0plot,
#       width = 6, height = 4, unit = "in")

#ggsave(filename = "./figs/uf0_dist.png", plot = uf0_dist,
       #width = 6, height = 4, unit = "in")

#ggsave(filename = "./figs/if0_dist.png", plot = if0_dist,
       #width = 6, height = 4, unit = "in")



