library(readxl)
library(tidyverse)

#Aqui me salte la primera fila "ventas_expandidas" para que el excel se pueda leer mejor 
ventas <- read_excel("ventas_exp.xlsx", skip = 1)

# Calcular totales y comisiones
ventas <- ventas %>%
  mutate(
    costo_total_neto = costo_hotel_neto + costo_vuelo_neto + 
      costo_seguro_neto + costo_excursion_neto,
    
    comision_total = total_pago_cliente - costo_total_neto,
    
    comision_ejecutivo = comision_total * 0.5,
    comision_agencia = comision_total * 0.5,
    
    utilidad_agencia = comision_agencia,
    
    estado_programa = "Pendiente"
  )
#Ahora visualizamos la tabla final
View(ventas)

#Graficando con ggplot para visualizar la comision total por ejecutivo
ventas %>%
  group_by(Ejecutivo) %>%
  summarise(comision_total = sum(comision_total)) %>%
  ggplot(aes(x = reorder(Ejecutivo, -comision_total), y = comision_total, fill = Ejecutivo)) +
  geom_col() +
  labs(
    title = "Comisión Total por Ejecutivo",
    x = "Ejecutivo",
    y = "Comisión Total ($)"
  ) +
  theme_minimal() +
  theme(legend.position = "none")

#Graficando con ggplot para visualizar la ganancia de la agencia
ventas %>%
  ggplot(aes(x = Fecha_de_compra, y = utilidad_agencia)) +
  geom_line(group = 1, color = "steelblue") +
  geom_point(color = "darkorange") +
  labs(
    title = "Utilidad de la Agencia a lo Largo del Tiempo",
    x = "Fecha de Compra",
    y = "Utilidad ($)"
  ) +
  theme_minimal()
#



