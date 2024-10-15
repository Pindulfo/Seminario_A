library(pxR)
library(dplyr)

#obtiene los nombres de archivos
archivos <- list.files('INPUT/DATA/Diabetes/Ingresos/', pattern = '*.px')
#añade la ruta del archivo
archivos_pc <- sapply(archivos, function(x) paste0('INPUT/DATA/Diabetes/Ingresos/',x))

#crea un dataframe con el primer archivo que sevirá de molde para añadir los siguientes años
df <- data.frame(as.data.frame(read.px(archivos_pc[1])))
#añade el atributo del año de los datos
df['Año'] = 1997

for (i in 2:length(archivos_pc)){
  #genera un df con los datos del año
  df_provisional <- data.frame(as.data.frame(read.px(archivos_pc[i])))
  
  #obtiene el año a partir del nombre del archivo
  año <- unlist(strsplit(archivos[i], "\\."))[1]
  año <- substr(año,nchar(año)-3,nchar(año))
  
  #genera una columna con el año de los datos
  df_provisional['Año'] = año
  
  #hace que los nombres de las columnas sean iguales al dataframe finak
  colnames(df_provisional) <- colnames(df)
  
  #une el dataframe con los datos del año al df final
  df <- rbind(df,df_provisional)
}

#dataframe con todos los datos de todos los años
df




