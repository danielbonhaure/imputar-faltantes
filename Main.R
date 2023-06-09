
# -----------------------------------------------------------------------------#
# ---- PASO 1. Inicializar entorno ----

# i. Cambiar carpeta de trabajo actual a la de este script
getScriptPath <- function(){
  cmd.args <- commandArgs()
  m <- regexpr("(?<=^--file=).+", cmd.args, perl=TRUE)
  script.dir <- dirname(regmatches(cmd.args, m))
  if(length(script.dir) == 0)
    stop("Can't determine script dir: please call the script with Rscript")
  if(length(script.dir) > 1)
    stop("Can't determine script dir: more than one '--file' argument detected")
  return(script.dir)
}
setwd( getScriptPath() )


# ii. Borrar objetos existentes en el ambiente
rm(list = ls()); gc()

# iii. Configurar huso horario en UTC
Sys.setenv(TZ = "UTC")

# iv. Cargar paquetes a utilizar
list.of.packages <- c("dplyr")
for (pack in list.of.packages) {
  if (! require(pack, character.only = TRUE)) {
    stop(paste0("Paquete no encontrado: ", pack))
  }
}

# v. Verificar si están instalados los paquetes necesarios
list.of.packages <- c("here", "doMC", "foreach", "iterators", 
                      "parallel", "randomForest", "xts", "zoo",
                      "lazyeval", "sirad", "missForest", "rgeos",
                      "gstat", "geosphere", "rgdal", "optparse")
for (pack in list.of.packages) {
  if(pack %in% rownames(installed.packages()) == FALSE) {
    stop(paste0("Paquete no encontrado: ", pack))
  }
}

rm(pack, list.of.packages); gc()
# ------------------------------------------------------------------------------

# -----------------------------------------------------------------------------#
# ---- PASO 2. Cargar librerias propias e iniciar script ----

# Cargar librerias
source(here::here("Rad.R"), echo = FALSE, chdir = TRUE)
source(here::here("Utils.R"), echo = FALSE, chdir = TRUE)
source(here::here("Impute.R"), echo = FALSE, chdir = TRUE)

# ------------------------------------------------------------------------------

# -----------------------------------------------------------------------------#
# ---- PASO 3. Leer parámetros de entrada ----

errors <- c()

option_list = list(
    optparse::make_option(c("-s", "--stations"), action="store", default="86285", type='character', 
                          help="Comma separated list of weather stations ids that should be imputed."),
    optparse::make_option(c("-c", "--country"), action="store", default="PY", type='character', 
                          help="Country of stations."),
    optparse::make_option(c("-m", "--parallelism"), action="store", default=1, type='integer', 
                          help="Max parallel tasks."),
    optparse::make_option(c("-o", "--output"), action="store", default="estaciones_sin_faltantes", type='character', 
                          help="Output filename (without file extension).")
)
opt = optparse::parse_args(optparse::OptionParser(option_list=option_list))

# ------------------------------------------------------------------------------

# -----------------------------------------------------------------------------#
# ---- PASO 4. Definir variables globales y verificar inputs ----

# Limpiar estaciones
stations <- as.integer(unlist(strsplit(opt$stations, ',')))
stations <- stations[!is.na(stations)]

# Verificar que queden estaciones luego de la limpieza
stopifnot(length(stations) > 0)

# Creamos un array con los OMM ID's de las estaciones sobre las que queremos estimar.
estacionesID <- stations

# Definir parámetros por defecto para el cálculo de radiación
srad_parameters <- list(
    'PY' = list(
        'bc' = c(A = 0.714, B = 0.007, C = 2.26),
        'svk' = c(A = 0.095, B = 0.345, C = -1.04),
        'ap' = c(A = 0.523, B = 0.232)
    ),
    'AR' = list(
        'ap' = c(A = 0.58, B = 0.2),
        'bc' = c(A = 0.69, B = 0.02, C = 2.12),
        'svk' = c(A = 0.06, B = 0.47, C = 0.8)
    ),
    'UY' = list(
      'ap' = c(A = 0.58, B = 0.2),
      'bc' = c(A = 0.69, B = 0.02, C = 2.12),
      'svk' = c(A = 0.06, B = 0.47, C = 0.8)
    )
)

# Definir credenciales de acceso a la API del CRC-SAS 
base.url        <- 'https://api.crc-sas.org/ws-api'
usuario.default <- '' 
clave.default   <- ''

# ------------------------------------------------------------------------------

# -----------------------------------------------------------------------------#
# ---- PASO 5. Imputar faltantes y calcular radicación ----
    
# Obtenemos los datos de las estaciones.
estaciones <- ConsumirServicioJSON(url = paste0(base.url, "/estaciones/", opt$country),
                                   usuario = usuario.default, clave = clave.default)

# Renombrar columnas
estaciones <- estaciones %>%
  dplyr::rename(lon_dec = longitud, lat_dec = latitud, elev = elevacion) %>%
  dplyr::filter(omm_id %in% estacionesID) %>%
  dplyr::mutate(pais_id = opt$country)

# Verificar que queden estaciones
stopifnot(nrow(estaciones) > 0)

# Calculamos las coordenadas en Gauss Kruger de cada estación.
GK.coords <- Gauss.Kruger.coordinates(estaciones)
# Agregamos las coordenadas x e y (GK) de cada estación.
estaciones <- data.frame(sp::coordinates(GK.coords), estaciones)

# Obtenemos los registros diarios de las estaciones con las que vamos a trabajar.
registrosDiarios <- purrr::map_dfr(
  .x = estaciones$omm_id,
  .f = function(omm_id) {
    fecha.desde           <- ConvertirFechaISO8601(as.Date("1947-01-01", tz = UTC))
    fecha.hasta           <- ConvertirFechaISO8601(as.Date(Sys.Date(), tz = UTC))
    url.registros.diarios <- glue::glue("{base.url}/registros_diarios/{omm_id}/{fecha.desde}/{fecha.hasta}")
    registros.largo       <- ConsumirServicioJSON(url = url.registros.diarios,
                                                  usuario = usuario.default, clave = clave.default)
    if ( "estado" %in% names(registros.largo) )
      registros.largo     <- dplyr::select(registros.largo, -estado)
    registros.ancho       <- tidyr::spread(registros.largo, key = variable_id, value = valor)
    return (registros.ancho)
  }
)

# Convertimos las fechas de la query en variables Date.
registrosDiarios$fecha <- as.Date(registrosDiarios$fecha)

# Check invalid temperatures to avoid DSSAT errors.
wrong_temps <- which(registrosDiarios$tmax <= registrosDiarios$tmin)
# Make invalid temperatures NA so the impute methods recalculate them.
registrosDiarios[wrong_temps, c('tmax', 'tmin')] <- NA

registrosImputados <- purrr::map_dfr(
  .x = estaciones$omm_id,
  .f = function(omm_id) {
    datosEstacion <- registrosDiarios[registrosDiarios$omm_id == omm_id,]
    indexesToWrite <- c()
    
    # Obtenemos los ID's y las coordenadas de cada estación vecina.max.distancia <- 150
    max.diff.elev <- 200
    max.vecinas   <- 7
    url.vecinas   <- glue::glue("{base.url}/estaciones_vecinas/{omm_id}")
    query.vecinas <- glue::glue("max_diferencia_elevacion={max.diff.elev}&max_vecinas={max.vecinas}")
    vecinos.data  <- ConsumirServicioJSON(url = glue::glue("{url.vecinas}?{query.vecinas}"),
                                          usuario = usuario.default, clave = clave.default)
    
    # renombrar columnas
    vecinos.data <- vecinos.data %>%
      dplyr::rename(lon_dec = longitud, lat_dec = latitud, elev = elevacion) %>%
      dplyr::select(omm_id, lat_dec, lon_dec)
    
    # Calculamos las coordenadas en GK.
    GK.coords <- Gauss.Kruger.coordinates(vecinos.data)
    
    # Agregamos las coordenadas x e y al data frame de vecinos.
    vecinos.data <- data.frame(sp::coordinates(GK.coords), omm_id=GK.coords@data$omm_id)
    
    # Traer registros de vecinos (solo cuando sea necesario).
    registrosVecinos <- NULL
    
    #
    # Imputar tmax, tmin y prcp
    #
    
    for (variable in c('tmax', 'tmin', 'prcp')) {
      missingIndexes <- which(is.na(datosEstacion[, variable]))
      
      writeLines(paste0("> Station: ", omm_id, ". Variable: ", variable, ". Missing: ", length(missingIndexes)))
      
      # Check if there are missing values for this station and variable, otherwise, skip it.
      if(length(missingIndexes) == 0) next;
      
      indexesToWrite <- c(indexesToWrite, missingIndexes)
      
      # Query the for neighbor's data. Done only if there are missing values.
      if(is.null(registrosVecinos)) {
        registrosVecinos <- purrr::map_dfr(
          .x = vecinos.data$omm_id,
          .f = function(omm_id) {
            writeLines(paste0(">> Station: ", omm_id, '. Downloading neighboring stations data.'))
            fecha.desde           <- ConvertirFechaISO8601(as.Date("1947-01-01", tz = UTC))
            fecha.hasta           <- ConvertirFechaISO8601(as.Date(Sys.Date(), tz = UTC))
            url.registros.diarios <- glue::glue("{base.url}/registros_diarios/{omm_id}/{fecha.desde}/{fecha.hasta}")
            registros.largo       <- ConsumirServicioJSON(url = url.registros.diarios,
                                                          usuario = usuario.default, clave = clave.default)
            if ( "estado" %in% names(registros.largo) )
              registros.largo     <- dplyr::select(registros.largo, -estado)
            registros.ancho       <- tidyr::spread(registros.largo, key = variable_id, value = valor)
            return (registros.ancho)
          }
        )
        registrosVecinos$fecha <- as.Date(registrosVecinos$fecha)
        registrosVecinos <- tibble::as_tibble(registrosVecinos) %>% dplyr::left_join(vecinos.data, by='omm_id')
      }
      
      if (variable == 'prcp') {
        datosEstacion <- impute_mf(omm_id, datosEstacion, variable, estaciones, missingIndexes, registrosVecinos, vecinos.data, opt$parallelism)
      } else {
        datosEstacion <- impute_idw(omm_id, datosEstacion, variable, estaciones, missingIndexes, registrosVecinos, vecinos.data, opt$parallelism)
      }
    }
    
    #
    # Imputar radiación
    #
    
    # Update radiation only.
    missing_dates <- as.Date(datosEstacion$fecha)
    
    writeLines(paste0("> Station: ", omm_id, ". Variable: rad. Missing: ", length(missing_dates)))
    
    codigo_pais <- toupper(estaciones[estaciones$omm_id == omm_id, 'pais_id'])
    
    if(!codigo_pais %in% names(srad_parameters)) codigo_pais <- 'AR'
    
    records <- datosEstacion %>% filter(fecha %in% missing_dates)
    
    estimado <- estimarRadiacion(estaciones=estaciones[estaciones$omm_id == omm_id, ],
                                 registrosDiarios=records,
                                 ap.cal = srad_parameters[[codigo_pais]]$ap,
                                 bc.cal = srad_parameters[[codigo_pais]]$bc,
                                 svk.cal = srad_parameters[[codigo_pais]]$svk)
    
    if(estimado$not_estimated > 0) {
      error_details <- paste0("Failed to estimate ", estimado$not_estimated, " radiation values for station ", omm_id)
      errors <<- c(errors, error_details)
      writeLines(error_details)
      return (NULL)
    } else {
      return (estimado$results %>% dplyr::select(omm_id, fecha, tmax, tmin, prcp, rad))
    }
  }
)

# Unir datos imputados y datos de la estación descargados desde la API
registrosCompletos <- registrosDiarios %>% dplyr::select(-tmax, -tmin, -prcp) %>%
  dplyr::right_join(registrosImputados, by = c("omm_id", "fecha"))

# Guardar resultados en un archivo RDS y en un archivo CSV
base_file_name <-
  tools::file_path_sans_ext(opt$output)
saveRDS(
  registrosCompletos, file=paste0(base_file_name, ".rds"))
write.csv(
  registrosCompletos, file=paste0(base_file_name, ".csv"), row.names=FALSE)

# ------------------------------------------------------------------------------

