library(data.table)

# #####################################################################################################
# @Author: Alfonso Guauna - alfonsogauna@hotmail.com
#
# Functions to replicate the methodology of share and wage curves of Autor 2015
#
# Comentary:
#   - We need that the occupations are order by increasing wage and the shares
#     must be in percentajes [5% = 5, not 0.05]
#   - We hace two types of tables, all of these must have only two columns
#     with the following names;
#       1) table_share (calculating shares): occ & share
#       2) table_share (calculating wages): occ & wage
#     table like 1 are used in calc_percentiles() and weight_percentiles() instead 
#     tables like 2 are used in wage_calc_percentiles() & wage_weight_percentiles()
#  - The procedure is as follows;
#    1. First we start with the share table and we use the calc_percentiles (wage_calc_percentiles) to
#       create the table of percentiles of the ocupations shares (wages)
#    2. Then with the weight_percentiles (wage_weight_percentiles) function we can weight the others 
#      others distributions of the shares (wages)
# #####################################################################################################

share_function <- function(occ, tabla_share){
  # function that returns the share given an occupation in table_share
  if (occ == '' | is.na(occ)){
    return(0)
  } else {
    tabla_share <- as.data.table(tabla_share)
    tabla_share[cno == occ]$share
  }
}


wage_function <- function(occ, tabla_share){
  # function that returns the wage given an occupation in table_share
  if (occ == '' | is.na(occ)){
    return(0)
  } else {
    tabla_share <- as.data.table(tabla_share)
    tabla_share[cno == occ]$wage
  }
}


calc_percentiles <- function(tabla_share){
  tabla_share <- as.data.table(tabla_share)
  tabla_share <- tabla_share[, `:=` (share_start = cumsum(share) - share,
                                     share_end = cumsum(share))]
  percen <- c(1:99)
  cortes <- tabla_share$share_end
  linespace <- c(percen, cortes)
  linespace <- sort(linespace)
  percentiles_tabla <- as.data.table(linespace)
  setnames(percentiles_tabla, "linespace", "borde_der")
  percentiles_tabla <- percentiles_tabla[, borde_izq := data.table::shift(borde_der)]
  percentiles_tabla[1,"borde_izq"] <- 0.0
  percentiles_tabla <- percentiles_tabla[, .(borde_izq, borde_der)]
  percentiles_tabla <- percentiles_tabla[, long_int := borde_der - borde_izq]
  percentiles_tabla <- percentiles_tabla[, `:=` (borde_izq = round(borde_izq, digits = 5),
                                                 borde_der = round(borde_der, digits = 5))]
  tabla_share <- tabla_share[, `:=` (share_start = round(share_start, digits = 5),
                                     share_end = round(share_end, digits = 5))]
  
  for (cno_row in 1:nrow(tabla_share)) {
    cno <- as.character(tabla_share[cno_row, 'cno'])
    cno_borde_izq <- as.numeric(tabla_share[cno_row, "share_start"])
    cno_borde_der  <- as.numeric(tabla_share[cno_row, "share_end"])
    
    for (int in 1:nrow(percentiles_tabla)) {
      int_borde_izq <- as.numeric(percentiles_tabla[int, "borde_izq"])
      int_borde_der <- as.numeric(percentiles_tabla[int, "borde_der"])
      
      if (cno_borde_izq <= int_borde_izq & cno_borde_der >= int_borde_der){
        percentiles_tabla[int, "cno"] <- cno
      } else {
        # PASS
      }
    }
  }
  
  percentiles_tabla$share <- lapply(percentiles_tabla$cno, share_function, tabla_share=tabla_share)
  percentiles_tabla$share <- as.numeric(percentiles_tabla$share) 
  percentiles_tabla <- percentiles_tabla[, pondera := long_int / share]
  percentiles_tabla <- percentiles_tabla[, percentil := floor(borde_izq) + 1]
  percentiles_tabla <- percentiles_tabla[, .(percentil, pondera, cno)]
}


weight_percentiles <- function(tabla_percentiles, tabla_share){
  # function that weights the shares so that they are normalized for the base year
  tabla_percentiles <- as.data.table(tabla_percentiles)
  tabla_share <- as.data.table(tabla_share)
  # we eliminate NAs
  tabla_percentiles <- tabla_percentiles[, pondera := ifelse(is.na(pondera), 0, pondera)]
  tabla_share <- tabla_share[, share := ifelse(is.na(share), 0, share)]
  # we use share_function() to retrieve the share of each occupancy
  tabla_percentiles$share <- lapply(tabla_percentiles$cno, share_function, tabla_share=tabla_share)
  # we pass the shares and weightings to numeric
  tabla_percentiles$share <- as.numeric(tabla_percentiles$share)
  tabla_percentiles$pondera <- as.numeric(tabla_percentiles$pondera)
  # we calculate the final share with the weightings
  tabla_percentiles <- tabla_percentiles[, share_w := pondera * share]
  tabla_percentiles <- tabla_percentiles[, share_w := sum(share_w, na.rm = TRUE), by='percentil']
  tabla_percentiles <- tabla_percentiles[, .(percentil, share_w)]
  tabla_percentiles <- unique(tabla_percentiles)
}


wage_calc_percentiles <- function(tabla_share){
  tabla_share <- as.data.table(tabla_share)
  tabla_share <- tabla_share[, `:=` (share_start = cumsum(share) - share,
                                     share_end = cumsum(share))]
  percen <- c(1:99)
  cortes <- tabla_share$share_end
  linespace <- c(percen, cortes)
  linespace <- sort(linespace)
  percentiles_tabla <- as.data.table(linespace)
  setnames(percentiles_tabla, "linespace", "borde_der")
  percentiles_tabla <- percentiles_tabla[, borde_izq := data.table::shift(borde_der)]
  percentiles_tabla[1,"borde_izq"] <- 0.0
  percentiles_tabla <- percentiles_tabla[, .(borde_izq, borde_der)]
  percentiles_tabla <- percentiles_tabla[, long_int := borde_der - borde_izq]
  percentiles_tabla <- percentiles_tabla[, `:=` (borde_izq = round(borde_izq, digits = 5),
                                                 borde_der = round(borde_der, digits = 5))]
  tabla_share <- tabla_share[, `:=` (share_start = round(share_start, digits = 5),
                                     share_end = round(share_end, digits = 5))]
  
  for (cno_row in 1:nrow(tabla_share)) {
    cno <- as.character(tabla_share[cno_row, 'cno'])
    cno_borde_izq <- as.numeric(tabla_share[cno_row, "share_start"])
    cno_borde_der  <- as.numeric(tabla_share[cno_row, "share_end"])
    
    for (int in 1:nrow(percentiles_tabla)) {
      int_borde_izq <- as.numeric(percentiles_tabla[int, "borde_izq"])
      int_borde_der <- as.numeric(percentiles_tabla[int, "borde_der"])
      
      if (cno_borde_izq <= int_borde_izq & cno_borde_der >= int_borde_der){
        percentiles_tabla[int, "cno"] <- cno
      } else {
        # PASS
      }
    }
  }
  
  percentiles_tabla$share <- lapply(percentiles_tabla$cno, share_function, tabla_share=tabla_share)
  percentiles_tabla$share <- as.numeric(percentiles_tabla$share) 
  percentiles_tabla <- percentiles_tabla[, pondera := long_int]
  percentiles_tabla <- percentiles_tabla[, percentil := floor(borde_izq) + 1]
  percentiles_tabla <- percentiles_tabla[, .(percentil, pondera, cno)]
}


wage_weight_percentiles <- function(tabla_percentiles, tabla_share){
  # function that weights the wages so that they are normalized for the base year
  tabla_percentiles <- as.data.table(tabla_percentiles)
  tabla_share <- as.data.table(tabla_share)
  # we eliminate NAs
  tabla_percentiles <- tabla_percentiles[, pondera := ifelse(is.na(pondera), 0, pondera)]
  tabla_share <- tabla_share[, wage := ifelse(is.na(wage), 0, wage)]
  # we use share_function() to retrieve the share of each occupancy
  tabla_percentiles$wage <- lapply(tabla_percentiles$cno, wage_function, tabla_share=tabla_share)
  # we pass the shares and weightings to numeric
  tabla_percentiles$wage <- as.numeric(tabla_percentiles$wage)
  tabla_percentiles$pondera <- as.numeric(tabla_percentiles$pondera)
  # we calculate the final share with the weightings
  tabla_percentiles <- tabla_percentiles[, wage_w := pondera * wage]
  tabla_percentiles <- tabla_percentiles[, wage_w := sum(wage_w, na.rm = TRUE), by='percentil']
  tabla_percentiles <- tabla_percentiles[, .(percentil, wage_w)]
  tabla_percentiles <- unique(tabla_percentiles)
}
