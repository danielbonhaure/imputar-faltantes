
# 
Gauss.Kruger.coordinates <- function(df) {
    sp::spTransform(sp::SpatialPointsDataFrame(cbind(x = df[,"lon_dec"], y = df[,"lat_dec"]), data=df,
                                       proj4string = sp::CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"), bbox = NULL),
                sp::CRS("+proj=tmerc +lat_0=-90 +lon_0=-60 +k=1 +x_0=5500000 +y_0=0 +ellps=intl +twogs84=-148,136,90,0,0,0,0 +units=m +no_defs"))
}


# El parámetro ssl_verifypeer=FALSE implica que no se va a verificar la validez del certificado 
# utilizado en la conexión SSL establecida con la API del CRC-SAS. Esto es útil cuando la máquina 
# cliente no puede validar el certificado emitido por la CA del CRC-SAS.
httr::set_config( httr::config(ssl_verifypeer = FALSE) )


# Función para acceder a un servicio web definido por una URL utilizando el método GET.
# Devuelve la respuesta como texto plano.
ConsumirServicioGET <- function(url, usuario, clave) {
  req  <- httr::GET(url = url, 
                    config = httr::authenticate(user = usuario, 
                                                password = clave))
  return (httr::content(req, as = "text", encoding = "UTF-8"))
}


# Función para acceder a un servicio web definido por una URL utilizando un usuario y clave.
# Asumiendo que la respuesta es un string JSON, hace la conversión del mismo a Data Frame.
ConsumirServicioJSON <- function(url, usuario, clave) {
  respuesta <- ConsumirServicioGET(url, usuario, clave)
  return (jsonlite::fromJSON(respuesta))
}


# Convierte una fecha a formato IS0 8601 (YYYY-MM-DDTHH:mm:ss) utilizando el huso horario GMT-0.
# Este es formato un estándar para representar fechas como una cadena de caracteres [7].
ConvertirFechaISO8601 <- function(fecha) {
  return (strftime(fecha, "%Y-%m-%dT%H:%M:%S", tz = "UTC"))
}

