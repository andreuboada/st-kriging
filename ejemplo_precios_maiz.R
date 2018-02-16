# Ejemplo de interpolación de precios del maíz
# Andreu Boada de Atela
# SEDESOL
# Laboratorio de Ciencia de Datos
# Febrero 2018

# Cargamos paquetes
library(tidyverse)
library(gstat)
library(sp)
library(spacetime)
library(raster)
library(rgdal)
library(rgeos)
library(stringr)
library(GISTools)
library(ggmap)
library(lubridate)
library(gridExtra)

# Los datos corresponden a precios del maíz a nivel pie de parcela para todos los
# municipios de México (a nivel anual y datos de precios en central de abastos
# del maíz blanco en grano por kg vendido en toneladas. Los datos fueron obtenidos
# de la SAGARPA y la Secretaría de Economía (SE).

panel_temporal <- read_csv("datos/precios_maiz_mexico_2015_2017.csv")


# Veamos una gráfica para los dos tipos de observaciones
aux <- tibble(grupo="A pie de parcela (municipal)", precio=municipal_3$precio) %>%
  bind_rows(tibble(grupo="En central de abastos (semanales)", precio=semanales_3$precio))
ggplot(aux, aes(x=grupo, y=precio)) +
  geom_boxplot() +
  scale_y_log10() + 
  scale_x_discrete(name="") +
  scale_y_continuous(name="Precio por kilogramo")

# El problema con este resultado es la diferencia en las medias para 
# precios semanales en central de abstos y precios mensuales a nivel 
# de parcela

panel_temporal %>%
  group_by(tipo) %>%
  summarise(media=mean(precio), sd=sd(precio), n_obs=n())

# Una primera aproximación será normalizar el precio por separado
# en cada grupo.

# Veamos una gráfica de cuantiles empíricos vs cuantiles normales
# para analizar qué tanto sentido tiene hacer pooling ajustando
# media y desviación estándar
panel_ord <- panel_temporal %>%
  group_by(tipo) %>%
  arrange(fecha)
panel_ajuste <- mutate(panel_ord, 
                       media = mean(precio),
                       sd = sd(precio),
                       residual = (precio - media)/sd
)

todos_residual <- sort(panel_ajuste$residual)
panel_total <- panel_ajuste %>%
  arrange(residual) %>%
  mutate(
    n_obs = n(),
    cuantil_total = approx(x = 1:length(todos_residual), y = todos_residual,
                           n = n_obs[1])$y
  )

ggplot(panel_total, aes(x = cuantil_total, y = residual)) +
  geom_point() +
  facet_wrap(~ tipo, nrow = 1) +
  stat_smooth(method = "lm")

# Agrupamos los datos y vemos el ajuste con una distribución normal
n <- nrow(panel_total)
panel_normal <- panel_total %>%
  ungroup() %>%
  arrange(residual) %>%
  mutate(
    valor.f.tot = (seq(1, n) - 0.5) / n, 
    q.norm.tot = qnorm(valor.f.tot)
  )

ggplot(panel_normal, aes(x = q.norm.tot, y = residual)) +
  geom_point() +
  stat_smooth(method = "lm")


####################### ST dataframe 🙈 ################################

# La fecha de la observación debe estar en formato POSIXlt
# Vamos a suponer que los precios observados anualmente corresponden
# al 1 de diciembre, ya que estos datos se registran a final de año.

panel_ajuste$lon <- panel_ajuste$lon + runif(nrow(panel_ajuste), min = 1e-6, max = 9e-6)
panel_ajuste$lat <- panel_ajuste$lat + runif(nrow(panel_ajuste), min = 1e-6, max = 9e-6)
maizSP2 <- SpatialPoints(panel_ajuste[,c('lon','lat')],crs(sids))
maizTM2 <- as.POSIXlt(panel_ajuste$fecha)
maizDF2 <- panel_ajuste %>% ungroup() %>% dplyr::select(residual)
timeDF2 <- STIDF(sp=maizSP2, time=maizTM2, data=maizDF2)

# Calculamos el semivariograma empírico
vv2 <- variogram(residual~1, timeDF2, cutoff = 1100, tunit="weeks", twindow = 1000, tlags=0:4)
plot(vv2)
plot(vv2, map=FALSE)
plot(vv2,wireframe=T)

# Ajustamos el semivariograma empírico
sumMetric <- vgmST("sumMetric", 
                   space = vgm(psill=0.5,"Gau", range=200, nugget=0.2),
                   time = vgm(psill=0.5,"Gau", range=200, nugget=0.2), 
                   joint = vgm(psill=0.5,"Gau", range=200, nugget=0.2),
                   nugget = 0.01,
                   stAni=200)

# Adaptamos parámetros iniciales
pars.l2 <- c(sill.s = 0, range.s = 10, nugget.s = 0,
             sill.t = 0, range.t = 1, nugget.t = 0,
             sill.st = 0, range.st = 10, nugget.st = 0,
             anis = 0)

pars.u2 <- c(sill.s = 2, range.s = 100, nugget.s = 1,
             sill.t = 100, range.t = 100, nugget.t = 1,
             sill.st = 2, range.st = 100, nugget.st = 1,
             anis = 700)

vgm2 <- fit.StVariogram(vv2,sumMetric,method="L-BFGS-B",lower=pars.l2,upper=pars.u2)
# Semivariograma ajustado
plot(vv2,vgm2, map = FALSE)
plot(vv2, map=FALSE)
extractPar(vgm2)

# Gráfica del semivariograma empírico
fit <- list(vst=vv2, vstModel=vgm2)
toPlot = data.frame(fit$vst)
toPlot2 <- toPlot %>% mutate(timelag = ordered(timelag, levels=unique(toPlot$timelag)))
ggplot(toPlot2, aes(x=dist, y=gamma, color=timelag, group=timelag)) + 
  geom_point() +
  geom_line() +
  scale_x_continuous(name = "distancia", limits = c(0,1100)) +
  scale_y_continuous(name = expression(gamma), limits = c(0.2,1.42))


# Gráfica del semivariograma ajustado
dist_grid <- expand.grid(timelag = unique(toPlot$timelag), 
                         spacelag = seq(min(toPlot$spacelag, na.rm=T),
                                        max(toPlot$spacelag, na.rm=T), 
                                        length.out=500))
model <- fit$vstModel
vs = variogramLine(model$space, dist_vector=dist_grid$spacelag)[,2]
vt = variogramLine(model$time,  dist_vector=dist_grid$timelag)[,2]
h = sqrt(dist_grid$spacelag^2 + (model$stAni * as.numeric(dist_grid$timelag))^2)
vst = variogramLine(model$joint, dist_vector=h)[,2]
aux <- data.frame(spacelag=dist_grid$spacelag, timelag=dist_grid$timelag, model=(vs + vt + vst))
aux$timelag <- ordered(aux$timelag, levels = unique(aux$timelag))
ggplot(aux, aes(x=spacelag, y = model, group = timelag, color = timelag)) + 
  geom_line(position = position_dodge(30)) +
  scale_x_continuous(name = "distancia", limits = c(0,1100)) +
  scale_y_continuous(name = expression(gamma), limits = c(0.2,1.42))


# Ajuste del modelo y predicción
grid_sp <- spsample(sids, n = 700, type = "regular")
grid_sp@coords[,1] + runif(350, min = 1e-6, max = 9e-6)
grid_sp@coords[,2] + runif(350, min = 1e-6, max = 9e-6)
grid_tm <- seq(ymd('2015-1-1'),ymd('2017-1-1'), by = '3 months')
grid_tm <- as.POSIXlt(grid_tm)
grid_ST <- STF(grid_sp, grid_tm)
pred2 <- krigeST(residual~1, data=timeDF2, modelList=vgm2, newdata=grid_ST)
stplot(pred2, main = "Precio del maíz en el espacio y tiempo")
