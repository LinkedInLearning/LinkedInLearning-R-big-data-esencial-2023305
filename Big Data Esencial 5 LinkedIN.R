##############
# Normalizar #
##############


# Creamos un data.frame simulado de pedidos desnormalizados
set.seed(123)

# Definimos catálogo de productos con precios fijos
productos <- data.frame(
  Producto = c("Ratón", "Teclado", "Monitor", "Auriculares", "Cámara", "Micrófono"),
  Precio = c(20, 45, 150, 30, 60, 80)
)

# Definimos clientes con ciudad fija
clientes <- data.frame(
  Cliente = c("Ana", "Luis", "Carlos", "Lucía", "Mario"),
  Ciudad = c("Madrid", "Sevilla", "Valencia", "Bilbao", "Zaragoza")
)

# Creamos las líneas de pedido
PedidoID <- rep(1:10, each = 2)
Fecha <- rep(seq.Date(as.Date("2023-01-01"), by = "day", length.out = 10), each = 2)

# Asignamos cliente a cada pedido (2 líneas por pedido)
cliente_rep <- rep(clientes$Cliente, length.out = length(PedidoID))
ciudad_rep <- sapply(cliente_rep, function(x) clientes$Ciudad[clientes$Cliente == x])

# Asignamos productos aleatorios
productos_sel <- sample(productos$Producto, length(PedidoID), replace = TRUE)

# Asignamos precio coherente con producto
precio_sel <- sapply(productos_sel, function(p) productos$Precio[productos$Producto == p])

# Cantidad aleatoria
cantidad_sel <- sample(1:3, length(PedidoID), replace = TRUE)

# Creamos el data.frame final
pedido_base <- data.frame(
  PedidoID = PedidoID,
  Fecha = Fecha,
  Cliente = cliente_rep,
  Ciudad = ciudad_rep,
  Producto = productos_sel,
  Precio = precio_sel,
  Cantidad = cantidad_sel
)

# TABLA DE CLIENTES (únicos)
clientes <- unique(pedido_base[c("Cliente", "Ciudad")])
clientes$ClienteID <- seq_len(nrow(clientes))

# Añadimos ClienteID a pedido_base
df <- merge(pedido_base, clientes, by = c("Cliente", "Ciudad"), all.x = TRUE)

# TABLA DE PRODUCTOS (únicos)
productos <- unique(df[c("Producto", "Precio")])
productos$ProductoID <- seq_len(nrow(productos))

# Añadimos ProductoID al dataset principal
df <- merge(df, productos, by = c("Producto", "Precio"), all.x = TRUE)

# TABLA DE PEDIDOS (cabecera)
pedidos <- unique(df[c("PedidoID", "Fecha", "ClienteID")])

# TABLA DE DETALLE DE PEDIDOS (líneas)
lineas_pedido <- df[c("PedidoID", "ProductoID", "Cantidad", "Precio")]

# Resultado: tablas normalizadas
head(clientes)
head(productos)
head(pedidos)
head(lineas_pedido)

#########
# JOINS #
#########

productos <- rbind(productos,c("Café",2,7))

# INNER JOIN: líneas de pedido + productos (solo coincidencias)
merge(lineas_pedido, productos, by = "ProductoID")

# LEFT JOIN: todos los pedidos, incluso si falta el producto
merge(lineas_pedido, productos, by = "ProductoID", all.x = TRUE)

# OUTER JOIN: toda la información
merge(lineas_pedido, productos, by = "ProductoID", all = TRUE)

# Anti join: productos que NO aparecen en líneas de pedido
productos[!(productos$ProductoID %in% lineas_pedido$ProductoID), ]

# INNER JOIN múltiple: líneas de pedido + productos + pedidos + clientes
step1 <- merge(lineas_pedido, productos, by = "ProductoID")
step2 <- merge(step1, pedidos, by = "PedidoID")
vista_final <- merge(step2, clientes, by = "ClienteID")

# Creamos columna de importe
vista_final$Importe <- vista_final$Cantidad * vista_final$Precio.x

# Vemos los 10 pedidos con mayor importe
top_pedidos <- aggregate(Importe ~ PedidoID, data = vista_final, sum)
top_pedidos <- top_pedidos[order(-top_pedidos$Importe), ]
head(top_pedidos, 10)