# st-kriging
Notas de kriging espacio temporal basadas en Gräler et al. (2012)

## Requisitos
- `R`

Paquetes:
- `tidyverse`
- `lubridate`
- `broom`
- `gridExtra`
- `ggmap`
- `sp`
- `maptools`
- `rgdal`
- `rgeos`
- `gstat`
- `spacetime`
- `raster`
- `GISTools`

## Datos

### PRECIOS NACIONALES
- Información obtenida del Sistema Nacional de Información e Integración de Mercados (SNIIM)
- Se utiliza precio mínimo por kilogramo (vendido por tonelada) de la Secretaría de Economía
- Fuente: http://www.economia-sniim.gob.mx/

### Precios municipales
- Información obtenida del Servicio de Información Agroalimentaria y Pesquera (SIAP)
- de la SAGARPA
- Fuente: http://infosiap.siap.gob.mx/gobmx/datosAbiertos.php


## Referencias
- Gräler, B., Rehr, M., Gerharz, L., & Pebesma, E. (2012). Spatio-temporal analysis and interpolation of PM10 measurements in Europe for 2009. ETC/ACM Technical Paper, 8, 1-29.
- Kilibarda, M., Hengl, T., Heuvelink, G., Gräler, B., Pebesma, E., Perčec Tadić, M., & Bajat, B. (2014). Spatio‐temporal interpolation of daily temperatures for global land areas at 1 km resolution. Journal of Geophysical Research: Atmospheres, 119(5), 2294-2313.
- Pebesma, E., & Gräler, B. (2013). Spatio-temporal geostatistics using gstat. Institute for Geoinformatics, University of Münster Rep.
