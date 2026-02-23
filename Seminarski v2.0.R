# Ukljucivanje svih potrebnih biblioteka

library(tidyverse)
library(ggplot2)
library(scales)
library(dplyr)
library(corrplot)
library(ggcorrplot)
library(stringr)
library(regclass)
calc_r2 <- function(pred, true) {
  1 - sum((pred - true)^2) / sum((true - mean(true))^2)
}
# logging funkcija (za W&B kasnije)
log_model <- function(model_name, rmse, mae, r2 = NA) {
  log_results <- data.frame(
    model = model_name,
    RMSE = rmse,
    MAE = mae,
    R2 = r2,
    time = Sys.time()
  )
  
  write.table(
    log_results,
    "results.csv",
    sep = ",",
    row.names = FALSE,
    append = TRUE,
    col.names = !file.exists("results.csv")
  )
}
data=read.csv("EAFC26.csv")
data
names(data)
str(data)
summary(data$OVR)

#dodato polje za tip kartice
data$CardType <- cut(
  data$OVR,
  breaks = c(-Inf, 64, 74, Inf),
  labels = c("Bronze", "Silver", "Gold"),
  right = TRUE
)
table(data$CardType)

#histogram za overall 

ggplot(data, aes(x = OVR)) +
  geom_histogram(bins = 50, fill = "lightblue", color = "black", alpha = 0.7) +
  labs(
    title = "Raspodela OVR rejtinga",
    x = "Overall igraca (OVR)",
    y = "Broj igraca"
  ) +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))


# Boxplot za overall

ggplot(data, aes(y = OVR)) +
  geom_boxplot(fill = "lightblue", alpha = 0.7) +
  labs(
    title = "Boxplot OVR rejtinga",
    y = "Overall igraca (OVR)"
  ) +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))


cor(data[sapply(data, is.numeric)], data$ovr)
cors <- cor(data[sapply(data, is.numeric)], data$ovr)
cors_sorted <- sort(cors[,1], decreasing = TRUE)
print(cors_sorted)


#TABELA NAPADACA

# Definisemo trazene pozicije
wanted_positions_attack <- c("ST", "RW", "LW")

# Provera za glavnu poziciju
cond_main_attack <- data$Position %in% wanted_positions_attack

# Filtriranje igraca
attackers_data <- data[cond_main_attack, ]

# Izbacivanje kolona url i card (ako postoje)
cols_to_remove <- c("url", "card")
attackers_data <- attackers_data[, !names(attackers_data) %in% cols_to_remove]

# Provera rezultata
dim(attackers_data)
colnames(attackers_data)
head(attackers_data)

#TABELA VEZNJAKA 
# Definisemo trazene pozicije
wanted_positions_midfielders <- c("CDM", "CAM", "CM", "RM", "LM")

# Provera za glavnu poziciju
cond_main_midfielders <- data$Position %in% wanted_positions_midfielders

# Filtriranje igraca
midfielders_data <- data[cond_main_midfielders , ]

# Izbacivanje kolona url i card (ako postoje)
midfielders_data <- midfielders_data[, !names(midfielders_data) %in% cols_to_remove]

# Provera rezultata
dim(midfielders_data)
colnames(midfielders_data)
head(midfielders_data)

summary(attackers_data$OVR)
summary(midfielders_data$OVR)

cor(attackers_data[sapply(attackers_data, is.numeric)], attackers_data$OVR)
cor(midfielders_data[sapply(midfielders_data, is.numeric)], midfielders_data$OVR)

cors <- cor(attackers_data[sapply(attackers_data, is.numeric)], attackers_data$OVR)
cors_sorted <- sort(cors[,1], decreasing = TRUE)
cors_sorted

cors2 <- cor(midfielders_data[sapply(midfielders_data, is.numeric)], midfielders_data$OVR)
cors_sorted2 <- sort(cors2[,1], decreasing = TRUE)
cors_sorted2


#UNIVARIJANTNA ANALIZA

ggplot(attackers_data, aes(CardType)) +
  geom_bar(fill = "skyblue", color = "black") +
  labs(
    title = "Distribucija tipova kartica kod napadača",
    x = "Tip kartice igrača",
    y = "Broj igrača"
  ) +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))

ggplot(midfielders_data, aes(CardType)) +
  geom_bar(fill = "skyblue", color = "black") +
  labs(
    title = "Distribucija tipova kartica kod veznjaka",
    x = "Tip kartice igrača",
    y = "Broj igrača"
  ) +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))


# Histogram za Volleys

ggplot(attackers_data, aes(x = Volleys)) +
  geom_histogram(bins = 40, fill = "lightgreen", color = "black", alpha = 0.8) +
  labs(
    title = "Raspodela atributa Volleys",
    x = "Volleys",
    y = "Broj igrača"
  ) +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))

#Hisogram za Finishing

ggplot(attackers_data, aes(x = Finishing)) +
  geom_histogram(bins = 40, fill = "orange", color = "black", alpha = 0.8) +
  labs(
    title = "Raspodela atributa Finishing",
    x = "Finishing",
    y = "Broj igrača"
  ) +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))

#UNIVARIJANTNA ANALIZA - Veznjaci

# Histogram za Vision

ggplot(midfielders_data, aes(x = Vision)) +
  geom_histogram(bins = 40, fill = "red", color = "black", alpha = 0.8) +
  labs(
    title = "Raspodela atributa Vision",
    x = "Vision",
    y = "Broj igrača"
  ) +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))

#Hisogram za Crossing

ggplot(midfielders_data, aes(x = Crossing)) +
  geom_histogram(bins = 40, fill = "purple", color = "black", alpha = 0.8) +
  labs(
    title = "Raspodela atributa Crossing",
    x = "Crossing",
    y = "Broj igrača"
  ) +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))

#UNIVARIJANTNA ANALIZA - Napadači i Veznjaci

# Histogram za Ball Control - napadači

ggplot(attackers_data, aes(x = Ball.Control)) +
  geom_histogram(bins = 40, fill = "yellow", color = "black", alpha = 0.8) +
  labs(
    title = "Raspodela atributa Ball Control kod napadača",
    x = "Ball Control",
    y = "Broj igrača"
  ) +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))

#Hisogram za Ball Control - veznjaci

ggplot(midfielders_data, aes(x = Ball.Control)) +
  geom_histogram(bins = 40, fill = "green", color = "black", alpha = 0.8) +
  labs(
    title = "Raspodela atributa Ball Control kod veznjaka",
    x = "Ball Control",
    y = "Broj igrača"
  ) +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))


#BIVARIJANTNA ANALIZA - Napadači i Veznjaci


# Boxplot za CardType vs OVR (Napadači)
ggplot(attackers_data, aes(x = CardType, y = OVR)) +
  geom_boxplot(fill = "skyblue", color = "black") +
  labs(
    title = "Distribucija OVR za različite tipove kartica (Napadači)",
    x = "Tip kartice",
    y = "Overall (OVR)"
  ) +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))

# Boxplot za CardType vs OVR (Veznjaci)
ggplot(midfielders_data, aes(x = CardType, y = OVR)) +
  geom_boxplot(fill = "purple", color = "black") +
  labs(
    title = "Distribucija OVR za različite tipove kartica (Veznjaci)",
    x = "Tip kartice",
    y = "Overall (OVR)"
  ) +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))


# Scatter plot za Finishing vs OVR (Napadači)
ggplot(attackers_data, aes(x = Finishing, y = OVR)) +
  geom_point(color = "orange") +
  geom_smooth(method = "lm", color = "black", linetype = "dashed") +
  labs(
    title = "Korelacija između Finishing i OVR (Napadači)",
    x = "Finishing",
    y = "Overall (OVR)"
  ) +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))

# Scatter plot za Volleys vs OVR (Napadači)
ggplot(attackers_data, aes(x = Volleys, y = OVR)) +
  geom_point(color = "lightgreen") +
  geom_smooth(method = "lm", color = "black", linetype = "dashed") +
  labs(
    title = "Korelacija između Volleys i OVR (Napadači)",
    x = "Volleys",
    y = "Overall (OVR)"
  ) +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))

# Scatter plot za Ball Control - napadači vs OVR
ggplot(attackers_data, aes(x = Ball.Control, y = OVR)) +
  geom_point(color = "yellow") +
  geom_smooth(method = "lm", color = "black", linetype = "dashed") +
  labs(
    title = "Korelacija između Ball Control i OVR (Napadači)",
    x = "Ball Control",
    y = "Overall (OVR)"
  ) +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))

# Scatter plot za Ball Control - veznjaci vs OVR
ggplot(midfielders_data, aes(x = Ball.Control, y = OVR)) +
  geom_point(color = "green") +
  geom_smooth(method = "lm", color = "black", linetype = "dashed") +
  labs(
    title = "Korelacija između Ball Control i OVR (Veznjaci)",
    x = "Ball Control",
    y = "Overall (OVR)"
  ) +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))


# Scatter plot za Vision vs OVR (Veznjaci)
ggplot(midfielders_data, aes(x = Vision, y = OVR)) +
  geom_point(color = "red") +
  geom_smooth(method = "lm", color = "black", linetype = "dashed") +
  labs(
    title = "Korelacija između Vision i OVR (Veznjaci)",
    x = "Vision",
    y = "Overall (OVR)"
  ) +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))

# Scatter plot za Crossing vs OVR (Veznjaci)
ggplot(midfielders_data, aes(x = Crossing, y = OVR)) +
  geom_point(color = "purple") +
  geom_smooth(method = "lm", color = "black", linetype = "dashed") +
  labs(
    title = "Korelacija između Crossing i OVR (Veznjaci)",
    x = "Crossing",
    y = "Overall (OVR)"
  ) +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))

# Point plot za Age vs OVR (Napadači)
ggplot(attackers_data, aes(x = Age, y = OVR)) +
  geom_point(color = "red") +
  labs(
    title = "Korelacija između Age i OVR (Napadači)",
    x = "Age",
    y = "Overall (OVR)"
  ) +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))

# Point plot za Age vs OVR (Veznjaci)
ggplot(midfielders_data, aes(x = Age, y = OVR)) +
  geom_point(color = "orange") +
  labs(
    title = "Korelacija između Age i OVR (Veznjaci)",
    x = "Age",
    y = "Overall (OVR)"
  ) +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))

# Point plot za Stamina vs OVR (Veznjaci)
ggplot(midfielders_data, aes(x = Stamina, y = OVR)) +
  geom_point(color = "orange") +
  labs(
    title = "Korelacija između Stamina i OVR (Veznjaci)",
    x = "Stamina",
    y = "Overall (OVR)"
  ) +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))

#MULTIVARIJANTNA ANALIZA


ggplot(attackers_data, aes(x = Age, y = OVR, color = Finishing)) +
  geom_jitter(alpha = 0.6) +
  scale_color_viridis_c() +
  labs(
    title = "Uticaj godina i Finishing atributa na OVR (Napadači)",
    x = "Godine",
    y = "Overall (OVR)",
    color = "Finishing"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    panel.grid.minor = element_blank()
  )


ggplot(attackers_data, aes(x = Finishing, y = OVR, color = Age)) +
  geom_point(alpha = 0.6) +
  scale_color_viridis_c() +
  facet_wrap(~ CardType) +
  labs(
    title = "Uticaj Finishing i godina na OVR (Napadači)",
    x = "Finishing",
    y = "Overall (OVR)",
    color = "Age"
  ) +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))


ggplot(attackers_data, aes(x = Reactions, y = OVR, color = Dribbling)) +
  geom_point(alpha = 0.6) +
  scale_color_viridis_c() +
  facet_wrap(~ CardType) +
  labs(
    title = "Uticaj Reactions i Dribbling atributa na OVR (Napadači)",
    x = "Reactions",
    y = "Overall (OVR)",
    color = "Dribbling"
  ) +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))

grid <- expand.grid(
  Composure = seq(min(midfielders_data$Composure), max(midfielders_data$Composure), length.out = 160),
  Short.Passing = seq(min(midfielders_data$Short.Passing), max(midfielders_data$Short.Passing), length.out = 160)
)
fit_lm <- lm(OVR ~ Composure + Short.Passing, data = midfielders_data)
grid$OVR_hat <- predict(fit_lm, newdata = grid)
ggplot() +
  geom_contour_filled(data = grid, aes(Composure, Short.Passing, z = OVR_hat), bins = 12) +
  geom_point(data = midfielders_data, aes(Composure, Short.Passing), alpha = 0.25) +
  scale_fill_viridis_d() +
  labs(
    title = "Contour : Uticaj Composure i Short Passing na OVR (Veznjaci)",
    x = "Composure",
    y = "Short Passing",
    fill = "OVR (procena)"
  ) +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))


ggplot(midfielders_data, aes(x = Composure, y = OVR, color = Short.Passing)) +
  geom_point(alpha = 0.6) +
  scale_color_viridis_c() +
  facet_wrap(~ CardType) +
  labs(
    title = "Uticaj Composure i Short Passing atributa na OVR (Veznjaci)",
    x = "Composure",
    y = "Overall (OVR)",
    color = "Short Passing"
  ) +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))

grid <- expand.grid(
  Composure = seq(min(midfielders_data$Composure), max(midfielders_data$Composure), length.out = 160),
  Short.Passing = seq(min(midfielders_data$Short.Passing), max(midfielders_data$Short.Passing), length.out = 160)
)
fit_lm <- lm(OVR ~ Composure + Short.Passing, data = midfielders_data)
grid$OVR_hat <- predict(fit_lm, newdata = grid)
ggplot() +
  geom_contour_filled(data = grid, aes(Composure, Short.Passing, z = OVR_hat), bins = 12) +
  geom_point(data = midfielders_data, aes(Composure, Short.Passing), alpha = 0.25) +
  scale_fill_viridis_d() +
  labs(
    title = "Contour (LM): Uticaj Composure i Short Passing na OVR (Veznjaci)",
    x = "Composure",
    y = "Short Passing",
    fill = "OVR (procena)"
  ) +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))



ggplot(midfielders_data, aes(x = Vision, y = OVR, color = Crossing)) +
  geom_point(alpha = 0.6) +
  scale_color_viridis_c() +
  facet_wrap(~ CardType) +
  labs(
    title = "Uticaj Vision i Crossing atributa na OVR (Veznjaci)",
    x = "Vision",
    y = "Overall (OVR)",
    color = "Crossing"
  ) +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))


#ČIŠĆENJE I OBRADA PODATAKA

#NEDOSTAJUCE VREDNOSTI
colSums(is.na(attackers_data))
colSums(is.na(midfielders_data))
attackers_data


# Izbacivanje kolona url i card (ako postoje)
cols_to_remove <- c("url", "card")
attackers_data <- attackers_data[, !names(attackers_data) %in% cols_to_remove]
midfielders_data <- midfielders_data[, !names(midfielders_data) %in% cols_to_remove]


# Spisak GK kolona
gk_columns <- c(
  "GK.Diving",
  "GK.Handling",
  "GK.Kicking",
  "GK.Positioning",
  "GK.Reflexes"
)

# Izbacivanje GK kolona iz oba skupa
attackers_data <- attackers_data[, !names(attackers_data) %in% gk_columns]
midfielders_data <- midfielders_data[, !names(midfielders_data) %in% gk_columns]


#PRAZNA POLJA
colSums(attackers_data == "", na.rm = TRUE)
colSums(midfielders_data == "", na.rm = TRUE)

#NEPRAVILNE I NELOGICNE VREDNOSTI
# Height: uzima samo broj ispred "cm"
attackers_data$Height <- as.numeric(sub("cm.*", "", attackers_data$Height))
midfielders_data$Height <- as.numeric(sub("cm.*", "", midfielders_data$Height))

# Weight: uzima samo broj ispred "kg"
attackers_data$Weight <- as.numeric(sub("kg.*", "", attackers_data$Weight))
midfielders_data$Weight <- as.numeric(sub("kg.*", "", midfielders_data$Weight))

#Analiza i potencijalno izbacivanje outlier i high leverage tačaka

los_finishing_visok_ovr <- attackers_data %>%
  filter(Finishing < 60 & OVR > 80)
nrow(los_finishing_visok_ovr)

los_control_visok_ovr <- attackers_data %>%
  filter(Ball.Control < 60 & OVR > 80)
nrow(los_control_visok_ovr)

prestari_top <- attackers_data %>%
  filter(Age > 36 & OVR > 85)
nrow(prestari_top)

los_vision_visok_ovr <- midfielders_data %>%
  filter(Vision < 60 & OVR > 80)
nrow(los_vision_visok_ovr)
#ovde se javlja 8 igraca pa cemo to proveriti

midfielders_data %>%
  filter(Vision < 60 & OVR > 80)


los_pas_visok_ovr <- midfielders_data %>%
  filter(Short.Passing < 60 & OVR > 80)
nrow(los_pas_visok_ovr)

premlad_top <- midfielders_data %>%
  filter(Age < 18 & OVR > 80)
nrow(premlad_top)


#EDA

numericke_kolone_att <- attackers_data %>% select_if(is.numeric)
names(numericke_kolone_att)

matrica_korelacije_att <- cor(numericke_kolone_att, use = "complete.obs")
matrica_korelacije_att

grep("OVR", colnames(numericke_kolone_att), ignore.case = TRUE, value = TRUE)

sort(matrica_korelacije_att[, "OVR"], decreasing = TRUE)

ggcorrplot(
  matrica_korelacije_att,
  hc.order = TRUE,
  type = "full",
  lab = TRUE,
  lab_size = 2.0,
  colors = c("red", "white", "blue"),
  outline.col = "gray",
  ggtheme = ggplot2::theme_minimal()
) +
  labs(
    title = "Korelaciona matrica numeričkih promenljivih (Napadači)",
    subtitle = "Prikaz svih parova promenljivih"
  ) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 14),
    plot.subtitle = element_text(hjust = 0.5, size = 12),
    axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)
  )

attackers_data$GENDER <- as.factor(attackers_data$GENDER)
attackers_data$Position <- as.factor(attackers_data$Position)
attackers_data$Preferred.foot <- as.factor(attackers_data$Preferred.foot)
attackers_data$CardType <- as.factor(attackers_data$CardType)

midfielders_data$GENDER <- as.factor(midfielders_data$GENDER)
midfielders_data$Position <- as.factor(midfielders_data$Position)
midfielders_data$Preferred.foot <- as.factor(midfielders_data$Preferred.foot)
midfielders_data$CardType <- as.factor(midfielders_data$CardType)

#FEATURE ENGINEERING

# Kreiranje kategorijske promenljive AgeGroup za napadače
attackers_data$AgeGroup <- cut(
  attackers_data$Age,
  breaks = c(-Inf, 21, 27, 33, Inf),
  labels = c("Young", "Prime", "Experienced", "Veteran")
)

# Pretvaranje u factor tip
attackers_data$AgeGroup <- as.factor(attackers_data$AgeGroup)

# Provera
table(attackers_data$AgeGroup)

ggplot(attackers_data, aes(x = AgeGroup, y = OVR)) +
  geom_boxplot(fill = "skyblue", color = "black") +
  labs(
    title = "OVR po starosnim grupama kod napadača",
    x = "Starosna grupa",
    y = "OVR"
  ) +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))

ggplot(attackers_data, aes(x = AgeGroup)) +
  geom_bar(fill = "steelblue") +
  labs(
    title = "Distribucija napadača po starosnim grupama",
    x = "Starosna grupa",
    y = "Broj igrača"
  ) +
  theme_minimal()


# Kreiranje kategorijske promenljive AgeGroup za veznjake
midfielders_data$AgeGroup <- cut(
  midfielders_data$Age,
  breaks = c(-Inf, 21, 27, 33, Inf),
  labels = c("Young", "Prime", "Experienced", "Veteran")
)

# Pretvaranje u factor tip
midfielders_data$AgeGroup <- as.factor(midfielders_data$AgeGroup)

# Provera
table(midfielders_data$AgeGroup)

ggplot(midfielders_data, aes(x = AgeGroup, y = OVR)) +
  geom_boxplot(fill = "lightgreen", color = "black") +
  labs(
    title = "OVR po starosnim grupama kod veznjaka",
    x = "Starosna grupa",
    y = "OVR"
  ) +
  theme_minimal()

ggplot(midfielders_data, aes(x = AgeGroup)) +
  geom_bar(fill = "steelblue") +
  labs(
    title = "Distribucija veznjaka po starosnim grupama",
    x = "Starosna grupa",
    y = "Broj igrača"
  ) +
  theme_minimal()


attackers_data
midfielders_data


#PRIPREMA I PODELA NA TRAIN I TEST

library(dplyr)

attackers_model <- attackers_data %>%
  select(
    OVR,
    Finishing,
    Ball.Control,
    Volleys,
    Reactions,
    Dribbling,
    AgeGroup,
    Stamina,
    Shot.Power
  )

midfielders_model <- midfielders_data %>%
  select(
    OVR,
    Vision,
    Short.Passing,
    Dribbling,
    Stamina,
    Balance,
    Long.Passing,
    Crossing,
    Ball.Control,
    Composure,
    Reactions,
    AgeGroup,
    CardType
  )

set.seed(123)
n <- nrow(attackers_model)
train_index <- sample(seq_len(n), size = 0.8 * n)
train_atc <- attackers_model[train_index, ]
test_atc <- attackers_model[-train_index, ]
nrow(train_atc)
nrow(test_atc)

set.seed(123)
n <- nrow(midfielders_model)
train_index <- sample(seq_len(n), size = 0.8 * n)
train_mid <- midfielders_model[train_index, ]
test_mid <- midfielders_model[-train_index, ]
nrow(train_mid)
nrow(test_mid)

true_ovr_atc <- test_atc$OVR
true_ovr_mid <- test_mid$OVR

#MODELI ZA NAPADACE
#1
model_atc_1 <- lm(OVR ~ Finishing, data = train_atc)
pred_atc_1 <- predict(model_atc_1, test_atc)
rmse_atc_1 <- sqrt(mean((pred_atc_1 - true_ovr_atc)^2))
mae_atc_1 <- mean(abs(pred_atc_1 - true_ovr_atc))
rmse_atc_1; mae_atc_1
summary(model_atc_1)

pred_atc_1 <- predict(model_atc_1, test_atc)
rmse_atc_1 <- sqrt(mean((pred_atc_1 - true_ovr_atc)^2))
mae_atc_1 <- mean(abs(pred_atc_1 - true_ovr_atc))
rmse_atc_1
mae_atc_1
summary(model_atc_1)
r2_atc_1 <- calc_r2(pred_atc_1, true_ovr_atc)
summary(model_atc_1)
log_model("Linear_ATC_1", rmse_atc_1, mae_atc_1, r2_atc_1)

#2
model_atc_2 <- lm(OVR ~ Finishing + Ball.Control, data = train_atc)
pred_atc_2 <- predict(model_atc_2, test_atc)
rmse_atc_2 <- sqrt(mean((pred_atc_2 - true_ovr_atc)^2))
mae_atc_2 <- mean(abs(pred_atc_2 - true_ovr_atc))
rmse_atc_2; mae_atc_2
summary(model_atc_2)

pred_atc_2 <- predict(model_atc_2, test_atc)
rmse_atc_2 <- sqrt(mean((pred_atc_2 - true_ovr_atc)^2))
mae_atc_2 <- mean(abs(pred_atc_2 - true_ovr_atc))
rmse_atc_2
mae_atc_2
summary(model_atc_2)
r2_atc_2 <- calc_r2(pred_atc_2, true_ovr_atc)
summary(model_atc_2)
log_model("Linear_ATC_2", rmse_atc_2, mae_atc_2, r2_atc_2)
#3
model_atc_3 <- lm(OVR ~ Finishing + Ball.Control + Volleys, data = train_atc)

pred_atc_3 <- predict(model_atc_3, test_atc)
rmse_atc_3 <- sqrt(mean((pred_atc_3 - true_ovr_atc)^2))
mae_atc_3 <- mean(abs(pred_atc_3 - true_ovr_atc))
rmse_atc_3
mae_atc_3
r2_atc_3 <- calc_r2(pred_atc_3, true_ovr_atc)
summary(model_atc_3)
log_model("Linear_ATC_3", rmse_atc_3, mae_atc_3, r2_atc_3)
#4
model_atc_4 <- lm(
  OVR ~ Finishing + Ball.Control + Volleys +
    Reactions + Dribbling + AgeGroup,
  data = train_atc
)

summary(model_atc_4)
VIF(model_atc_4)

pred_atc_4 <- predict(model_atc_4, test_atc)
rmse_atc_4 <- sqrt(mean((pred_atc_4 - true_ovr_atc)^2))
mae_atc_4 <- mean(abs(pred_atc_4 - true_ovr_atc))
rmse_atc_4
mae_atc_4
r2_atc_4 <- calc_r2(pred_atc_4, true_ovr_atc)
summary(model_atc_4)
VIF(model_atc_4)
log_model("Linear_ATC_4", rmse_atc_4, mae_atc_4, r2_atc_4)
#5
model_atc_5 <- lm(
  OVR ~ Finishing + Ball.Control + Volleys +
    Reactions + Dribbling + AgeGroup + Stamina + Shot.Power,
  data = train_atc
)

summary(model_atc_5)
VIF(model_atc_5)

pred_atc_5 <- predict(model_atc_5, test_atc)
rmse_atc_5 <- sqrt(mean((pred_atc_5 - true_ovr_atc)^2))
mae_atc_5 <- mean(abs(pred_atc_5 - true_ovr_atc))
rmse_atc_5
mae_atc_5
r2_atc_5 <- calc_r2(pred_atc_5, true_ovr_atc)
summary(model_atc_5)
VIF(model_atc_5)
log_model("Linear_ATC_5", rmse_atc_5, mae_atc_5, r2_atc_5)
saveRDS(model_atc_5, "attack_best_model.rds")
#MODELI ZA VEZNJAKE

#1
model_mid_1 <- lm(OVR ~ Vision, data = train_mid)
pred_mid_1 <- predict(model_mid_1, test_mid)
m1_rmse_mid <- sqrt(mean((pred_mid_1 - true_ovr_mid)^2))
m1_mae_mid <- mean(abs(pred_mid_1 - true_ovr_mid))
m1_rmse_mid; m1_mae_mid
summary(model_mid_1)

pred_mid_1 <- predict(model_mid_1, test_mid)
rmse_mid_1 <- sqrt(mean((pred_mid_1 - true_ovr_mid)^2))
mae_mid_1 <- mean(abs(pred_mid_1 - true_ovr_mid))
rmse_mid_1
mae_mid_1
summary(model_mid_1)
r2_mid_1 <- calc_r2(pred_mid_1, true_ovr_mid)
summary(model_mid_1)
log_model("Linear_MID_1", rmse_mid_1, mae_mid_1, r2_mid_1)
#2
model_mid_2 <- lm(OVR ~ Vision + Short.Passing, data = train_mid)

pred_mid_2 <- predict(model_mid_2, test_mid)
rmse_mid_2 <- sqrt(mean((pred_mid_2 - true_ovr_mid)^2))
mae_mid_2 <- mean(abs(pred_mid_2 - true_ovr_mid))
rmse_mid_2
mae_mid_2
summary(model_mid_2)
r2_mid_2 <- calc_r2(pred_mid_2, true_ovr_mid)
summary(model_mid_2)
log_model("Linear_MID_2", rmse_mid_2, mae_mid_2, r2_mid_2)
#3
model_mid_3 <- lm(
  OVR ~ Vision + Short.Passing + Crossing + Ball.Control,
  data = train_mid
)

pred_mid_3 <- predict(model_mid_3, test_mid)
rmse_mid_3 <- sqrt(mean((pred_mid_3 - true_ovr_mid)^2))
mae_mid_3 <- mean(abs(pred_mid_3 - true_ovr_mid))
rmse_mid_3
mae_mid_3
summary(model_mid_3)
r2_mid_3 <- calc_r2(pred_mid_3, true_ovr_mid)
summary(model_mid_3)
log_model("Linear_MID_3", rmse_mid_3, mae_mid_3, r2_mid_3)
#4
model_mid_4 <- lm(
  OVR ~ Vision + Short.Passing + Crossing +
    Ball.Control + Composure + Reactions + AgeGroup,
  data = train_mid
)

summary(model_mid_4)
VIF(model_mid_4)

pred_mid_4 <- predict(model_mid_4, test_mid)
rmse_mid_4 <- sqrt(mean((pred_mid_4 - true_ovr_mid)^2))
mae_mid_4 <- mean(abs(pred_mid_4 - true_ovr_mid))
rmse_mid_4
mae_mid_4
r2_mid_4 <- calc_r2(pred_mid_4, true_ovr_mid)
summary(model_mid_4)
VIF(model_mid_4)
log_model("Linear_MID_4", rmse_mid_4, mae_mid_4, r2_mid_4)

#5
model_mid_5 <- lm(
  OVR ~ Vision + Short.Passing +
    Ball.Control + Composure + Reactions +
    AgeGroup +
    Long.Passing + Dribbling + Stamina,
  data = train_mid
)

summary(model_mid_5)
VIF(model_mid_5)

pred_mid_5 <- predict(model_mid_5, test_mid)
rmse_mid_5 <- sqrt(mean((pred_mid_5 - true_ovr_mid)^2))
mae_mid_5 <- mean(abs(pred_mid_5 - true_ovr_mid))
rmse_mid_5
mae_mid_5
r2_mid_5 <- calc_r2(pred_mid_5, true_ovr_mid)
summary(pred_mid_5)
VIF(model_mid_5)
log_model("Linear_MID_5", rmse_mid_5, mae_mid_5, r2_mid_5)
#RANDOM FOREST

install.packages("ranger")
library(ranger)
library(ranger)
library(xgboost)
library(glmnet)
library(ggplot2)
library(rpart)
library(rpart.plot)

calc_r2 <- function(pred, true) {
  1 - sum((pred - true)^2) / sum((true - mean(true))^2)
}

log_model <- function(model_name, rmse, mae, r2 = NA) {
  log_results <- data.frame(
    model = model_name,
    RMSE = rmse,
    MAE = mae,
    R2 = r2,
    time = Sys.time()
  )
  
  write.table(
    log_results,
    "results.csv",
    sep = ",",
    row.names = FALSE,
    append = TRUE,
    col.names = !file.exists("results.csv")
  )
}

# RANDOM FOREST — NAPADACI

rf_atc <- ranger(
  OVR ~ Finishing + Ball.Control + Volleys +
    Reactions + Dribbling + AgeGroup,
  data = train_atc,
  num.trees = 500,
  importance = "impurity",
  seed = 123
)

rf_pred_atc <- predict(rf_atc, test_atc)$predictions
rf_rmse_atc <- sqrt(mean((rf_pred_atc - true_ovr_atc)^2))
rf_mae_atc <- mean(abs(rf_pred_atc - true_ovr_atc))
rf_r2_atc <- calc_r2(rf_pred_atc, true_ovr_atc)

rf_rmse_atc; rf_mae_atc; rf_r2_atc
log_model("RF_ATC", rf_rmse_atc, rf_mae_atc, rf_r2_atc)

# FEATURE IMPORTANCE — RF NAPADACI
rf_imp_atc <- data.frame(
  feature = names(rf_atc$variable.importance),
  importance = rf_atc$variable.importance
)
rf_imp_atc <- rf_imp_atc[order(-rf_imp_atc$importance), ]

ggplot(rf_imp_atc, aes(x = reorder(feature, importance), y = importance)) +
  geom_col(fill = "steelblue") +
  coord_flip() +
  labs(title = "RF Feature Importance — Napadači")

# RANDOM FOREST — VEZNJACI

rf_mid <- ranger(
  OVR ~ Vision + Short.Passing + Ball.Control +
    Composure + Reactions + AgeGroup +
    Long.Passing + Dribbling + Stamina,
  data = train_mid,
  num.trees = 500,
  importance = "impurity",
  seed = 123
)

rf_pred_mid <- predict(rf_mid, test_mid)$predictions
rf_rmse_mid <- sqrt(mean((rf_pred_mid - true_ovr_mid)^2))
rf_mae_mid <- mean(abs(rf_pred_mid - true_ovr_mid))
rf_r2_mid <- calc_r2(rf_pred_mid, true_ovr_mid)

rf_rmse_mid; rf_mae_mid; rf_r2_mid
log_model("RF_MID", rf_rmse_mid, rf_mae_mid, rf_r2_mid)

rf_imp_mid <- data.frame(
  feature = names(rf_mid$variable.importance),
  importance = rf_mid$variable.importance
)
rf_imp_mid <- rf_imp_mid[order(-rf_imp_mid$importance), ]

ggplot(rf_imp_mid, aes(x = reorder(feature, importance), y = importance)) +
  geom_col(fill = "steelblue") +
  coord_flip() +
  labs(title = "RF Feature Importance — Veznjaci")

saveRDS(rf_mid, "rf_mid_model.rds")


# XGBOOST — NAPADACI

num_cols_atc <- setdiff(names(train_atc)[sapply(train_atc, is.numeric)], "OVR")

train_matrix_atc <- as.matrix(train_atc[, num_cols_atc])
test_matrix_atc <- as.matrix(test_atc[, num_cols_atc])

xgb_atc <- xgboost(
  data = train_matrix_atc,
  label = train_atc$OVR,
  nrounds = 100,
  objective = "reg:squarederror",
  verbose = 0
)

xgb_pred_atc <- predict(xgb_atc, test_matrix_atc)

xgb_rmse_atc <- sqrt(mean((xgb_pred_atc - true_ovr_atc)^2))
xgb_mae_atc <- mean(abs(xgb_pred_atc - true_ovr_atc))
xgb_r2_atc <- calc_r2(xgb_pred_atc, true_ovr_atc)

xgb_rmse_atc; xgb_mae_atc; xgb_r2_atc
log_model("XGB_ATC", xgb_rmse_atc, xgb_mae_atc, xgb_r2_atc)

# XGBOOST — VEZNJACI

num_cols_mid <- setdiff(names(train_mid)[sapply(train_mid, is.numeric)], "OVR")

train_matrix_mid <- as.matrix(train_mid[, num_cols_mid])
test_matrix_mid <- as.matrix(test_mid[, num_cols_mid])

xgb_mid <- xgboost(
  data = train_matrix_mid,
  label = train_mid$OVR,
  nrounds = 100,
  objective = "reg:squarederror",
  verbose = 0
)

xgb_pred_mid <- predict(xgb_mid, test_matrix_mid)

xgb_rmse_mid <- sqrt(mean((xgb_pred_mid - true_ovr_mid)^2))
xgb_mae_mid <- mean(abs(xgb_pred_mid - true_ovr_mid))
xgb_r2_mid <- calc_r2(xgb_pred_mid, true_ovr_mid)

xgb_rmse_mid; xgb_mae_mid; xgb_r2_mid
log_model("XGB_MID", xgb_rmse_mid, xgb_mae_mid, xgb_r2_mid)

# LASSO — NAPADACI

train_atc$AgeGroup <- as.factor(train_atc$AgeGroup)
test_atc$AgeGroup <- as.factor(test_atc$AgeGroup)

x_atc <- model.matrix(
  OVR ~ Finishing + Ball.Control + Volleys +
    Reactions + Dribbling + AgeGroup,
  train_atc
)[, -1]

lasso_atc <- cv.glmnet(x_atc, train_atc$OVR, alpha = 1)

x_test_atc <- model.matrix(
  OVR ~ Finishing + Ball.Control + Volleys +
    Reactions + Dribbling + AgeGroup,
  test_atc
)[, -1]

lasso_pred_atc <- predict(lasso_atc, x_test_atc, s = "lambda.min")

lasso_rmse_atc <- sqrt(mean((lasso_pred_atc - true_ovr_atc)^2))
lasso_mae_atc <- mean(abs(lasso_pred_atc - true_ovr_atc))
lasso_r2_atc <- calc_r2(lasso_pred_atc, true_ovr_atc)

lasso_rmse_atc; lasso_mae_atc; lasso_r2_atc
log_model("LASSO_ATC", lasso_rmse_atc, lasso_mae_atc, lasso_r2_atc)


# LASSO — VEZNJACI

x_mid <- model.matrix(
  OVR ~ Vision + Short.Passing + Ball.Control +
    Composure + Reactions + AgeGroup +
    Long.Passing + Dribbling + Stamina,
  train_mid
)[, -1]

lasso_mid <- cv.glmnet(x_mid, train_mid$OVR, alpha = 1)

x_test_mid <- model.matrix(
  OVR ~ Vision + Short.Passing + Ball.Control +
    Composure + Reactions + AgeGroup +
    Long.Passing + Dribbling + Stamina,
  test_mid
)[, -1]

lasso_pred_mid <- predict(lasso_mid, x_test_mid, s = "lambda.min")

lasso_rmse_mid <- sqrt(mean((lasso_pred_mid - true_ovr_mid)^2))
lasso_mae_mid <- mean(abs(lasso_pred_mid - true_ovr_mid))
lasso_r2_mid <- calc_r2(lasso_pred_mid, true_ovr_mid)

lasso_rmse_mid; lasso_mae_mid; lasso_r2_mid
log_model("LASSO_MID", lasso_rmse_mid, lasso_mae_mid, lasso_r2_mid)




#DECISION TREES

att_tree_df <- attackers_data[, c("OVR","Finishing","Ball.Control","Volleys","Reactions","Dribbling","Age")]

set.seed(123)
tree_att <- rpart(
  OVR ~ .,
  data = att_tree_df,
  method = "anova",
  control = rpart.control(cp = 0.01, maxdepth = 4, minsplit = 30)
)

# PRIKAZ
rpart.plot(
  tree_att,
  type = 2, extra = 101, fallen.leaves = TRUE, cex = 0.8,
  main = "Decision Tree (Regresija): OVR — Napadači"
)


mid_tree_df <- midfielders_data[, c(
  "OVR",
  "Vision","Short.Passing","Dribbling","Stamina","Balance","Long.Passing",
  "Crossing","Ball.Control","Composure","Reactions","Age"
)]

set.seed(123)
tree_mid <- rpart(
  OVR ~ .,
  data = mid_tree_df,
  method = "anova",
  control = rpart.control(cp = 0.01, maxdepth = 4, minsplit = 30)
)

# PRIKAZ
rpart.plot(
  tree_mid,
  type = 2, extra = 101, fallen.leaves = TRUE, cex = 0.7,
  main = "Decision Tree (Regresija): OVR — Veznjaci"
)



results <- read.csv("results.csv")

# sortiraj po R2 opadajuće

results_sorted <- results[order(-results$R2), ]

print(results_sorted)