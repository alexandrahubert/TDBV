rm(list = ls())
load(file = "processed_data/ref.RData")
load(file = "output/modeles.RData")

data <- ref %>% 
 # filter(jeu_donnees %in% c("tbv_ref", "galineau_2020")) %>% 
 filter(Surface_BV_km2 < 20)



# sans terme quadratique

mod <- lm(formula = Lpb ~ log10(1+Surface_BV_km2),
          data = data)

summary(mod)

mod$coefficients
a <- mod$coefficients[2]
b <- mod$coefficients[1]

sbv <- 0.010
x <- logsbvplus1 <- log10(1 + sbv)
lpb_pred_man <- a * x + b

new_data = data.frame(Surface_BV_km2 = sbv)
lpb_pred_model <- predict.lm(mod, newdata = new_data)



# avec terme quadratique

mod <- lm(formula = log10(1 + Lpb) ~ poly(log10(1 + Surface_BV_km2), 2),
          data = data)

summary(mod)

mod$coefficients
a <- mod$coefficients[2]
b <- mod$coefficients[1]

x <- logsbvplus1 <- log10(1 + sbv)
y <- a * x + b

lpb_est_man <- 10^(y) - 1

new_data = data.frame(Surface_BV_km2 = sbv)
pred <- predict.lm(mod, newdata = new_data)
lpb_pred_model <- 10^(pred) - 1

































ggplot(data = data, aes(x = Surface_BV_km2, y = Lpb)) +
  geom_point() + 
  scale_x_log10() +
  scale_y_log10() +
  geom_smooth(method = "gam")

summary(mod)
plot(mod)


sbv <- 0.1

sbvplus1 <- 1+sbv

log10sbvplus1 <- log(sbvplus1, base = 10)

new_data <- data.frame(Surface_BV_km2 = log10sbvplus1)

predit <- predict(modele_lpb, newdata = new_data)

unpluslpb <- 10^predit

lpb <- unpluslpb - 1

