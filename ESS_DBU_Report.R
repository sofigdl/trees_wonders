# Convert to long format
compareddf_long <- compareddf %>%
  pivot_longer(cols = c(Bti_rs, Bti), names_to = "Source", values_to = "BTI")

ggplot(compareddf_long, aes(x = Source, y = BTI, fill = Source)) +
  geom_boxplot(
    width = .5, 
    outlier.shape = NA,
    alpha=.9
  ) +
  scale_fill_manual(values=c("#A3D977", "#2E7D32")) + 
  labs(x = "Datenquelle", y = "Biomasse (Kg C)", fill = "Source") +
  scale_x_discrete(labels = c("Baumkataster", "Fernerkundung")) +
  theme_minimal(base_size = 20)+
  theme(legend.position="none")+stat_summary(
    fun = median,
    geom = "point",
    shape = 16,
    size = 2,
    color = "black",
    show.legend = FALSE
  )+ylim(0,1500)
#..............................
# Convert to long format
compareddf_long <- compareddf %>%
  pivot_longer(cols = c(IncBio_rs, IncBio), names_to = "Source", values_to = "Biomass")

ggplot(compareddf_long, aes(x = Source, y = Biomass, fill = Source)) +
  geom_boxplot(
    width = .5, 
    outlier.shape = NA,
    alpha=.9
  ) +
  scale_fill_manual(values=c("#8AA76C", "#415D43")) + 
  labs(x = "Datenquelle", y = "Biomass Increment (Kg C /Jahr)", fill = "Source") +
  scale_x_discrete(labels = c("Baumkataster", "Fernerkundung")) +
  theme_minimal(base_size = 20)+
  theme(legend.position="none")+stat_summary(
    fun = median,
    geom = "point",
    shape = 16,
    size = 2,
    color = "black",
    show.legend = FALSE
  )+ylim(0,100)
#.....................
# Convert to long format
compareddf_long <- compareddf %>%
  pivot_longer(cols = c(CO2stor_rs, CO2stor), names_to = "Source", values_to = "CO2")



ggplot(compareddf_long, aes(x = Source, y = CO2, fill = Source)) +
  geom_boxplot(
    width = .5, 
    outlier.shape = NA,
    alpha=.9
  ) +
  scale_fill_manual(values=c("#C2C287", "#6D6D3D")) + 
  labs(x = "Datenquelle", y = "CO2-Speicherung (Kg CO2/Jahr)", fill = "Source") +
  scale_x_discrete(labels = c("Baumkataster", "Fernerkundung")) +
  theme_minimal(base_size = 20)+
  theme(legend.position="none")+stat_summary(
    fun = median,
    geom = "point",
    shape = 16,
    size = 2,
    color = "black",
    show.legend = FALSE
  )+ylim(0,200)

#....................
# Convert to long format
compareddf_long <- compareddf %>%
  pivot_longer(cols = c(transp_rs, transp), names_to = "Source", values_to = "Transpiration")

ggplot(compareddf_long, aes(x = Source, y = Transpiration, fill = Source)) +
  geom_boxplot(
    width = .5, 
    outlier.shape = NA,
    alpha=.9
  ) +
  scale_fill_manual(values=c("#7EC8E3", "#357ABD")) +
  labs(x = "Datenquelle", y = "Transpiration (mm/Jahr)", color = "Source") +
  scale_x_discrete(labels = c("Baumkataster", "Fernerkundung")) +
  theme_minimal(base_size = 20)+
  theme(legend.position="none")+stat_summary(
    fun = median,
    geom = "point",
    shape = 16,
    size = 2,
    color = "black",
    show.legend = FALSE
  )#+ylim(0,200)


#........................
# Convert to long format
compareddf_long <- compareddf %>%
  pivot_longer(cols = c(ro_rs, ro), names_to = "Source", values_to = "Runoff")


ggplot(compareddf_long, aes(x = Source, y = Runoff, fill = Source)) +
  geom_boxplot(
    width = .5, 
    outlier.shape = NA,
    alpha=.9
  ) +
  scale_fill_manual(values=c("#A4C2F4", "#375E97")) +
  labs(x = "Datenquelle", y = "Abfluss (mm/Jahr)", color = "Source") +
  scale_x_discrete(labels = c("Baumkataster", "Fernerkundung")) +
  theme_minimal(base_size = 20)+
  theme(legend.position="none")+stat_summary(
    fun = median,
    geom = "point",
    shape = 16,
    size = 2,
    color = "black",
    show.legend = FALSE
  )#+ylim(0,200)

#.......................
# Convert to long format
compareddf_long <- compareddf %>%
  pivot_longer(cols = c(wa_com_rs, wa_com), names_to = "Source", values_to = "Water")



ggplot(compareddf_long, aes(x = Source, y = Water, fill = Source)) +
  geom_boxplot(
    width = .5, 
    outlier.shape = NA,
    alpha=.9
  ) +
  scale_fill_manual(values=c("#5BA4C4", "#00658B")) +
  labs(x = "Datenquelle", y = "Water consumption (m3/Jahr)", color = "Source") +
  scale_x_discrete(labels = c("Baumkataster", "Fernerkundung")) +
  theme_minimal(base_size = 20)+
  theme(legend.position="none")+stat_summary(
    fun = median,
    geom = "point",
    shape = 16,
    size = 2,
    color = "black",
    show.legend = FALSE
  )+ylim(0,150)

#..........................
# Convert to long format
compareddf_long <- compareddf %>%
  pivot_longer(cols = c(cooltr_rs, cooltr), names_to = "Source", values_to = "CoolTr")


ggplot(compareddf_long, aes(x = Source, y = CoolTr, fill = Source)) +
  geom_boxplot(
    width = .5, 
    outlier.shape = NA,
    alpha=.9
  ) +
  scale_fill_manual(values=c("#B0B8E6", "#4933C4")) +
  labs(x = "Datenquelle", y = "Kühlung durch Transpiration im Sommer  (kWh)", color = "Source") +
  scale_x_discrete(labels = c("Baumkataster", "Fernerkundung")) +
  theme_minimal(base_size = 20)+
  theme(legend.position="none")+stat_summary(
    fun = median,
    geom = "point",
    shape = 16,
    size = 2,
    color = "black",
    show.legend = FALSE
  )+ylim(0,45000)

#.......................
# Convert to long format
compareddf_long <- compareddf %>%
  pivot_longer(cols = c(coolsh_rs, coolsh), names_to = "Source", values_to = "CoolSh")


ggplot(compareddf_long, aes(x = Source, y = CoolSh, fill = Source)) +
  geom_boxplot(
    width = .5, 
    outlier.shape = NA,
    alpha=.9
  ) +
  scale_fill_manual(values=c("#D6C7E6", "#6A4C93")) +
  labs(x = "Datenquelle", y = "Kühlung durch Beschattung im Sommer (kWh)", color = "Source") +
  scale_x_discrete(labels = c("Baumkataster", "Fernerkundung")) +
  theme_minimal(base_size = 20)+
  theme(legend.position="none")+stat_summary(
    fun = median,
    geom = "point",
    shape = 16,
    size = 2,
    color = "black",
    show.legend = FALSE
  )+ylim(0,75000)
