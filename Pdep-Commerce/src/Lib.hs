type NombreDeProducto = String
type Producto = (NombreDeProducto, Int)

precioTotal :: Producto -> Int -> Int -> Int -> Int
precioTotal unProducto cantidad descuento costoEnvio = aplicarCostoDeEnvio ((* cantidad) . aplicarDescuento unProducto $ descuento) costoEnvio

productoDeElite :: Producto -> Bool
productoDeElite unProducto = productoDeLujo unProducto && productoCodiciado unProducto && not (productoCorriente unProducto)

aplicarDescuento :: Producto -> Int -> Int
aplicarDescuento unProducto descuento = precio unProducto - descuento

precio :: Producto -> Int
precio (_, precio) = precio

entregaSencilla :: String -> Bool
entregaSencilla fecha = (even . length) fecha

descodiciarProducto :: Producto -> String
descodiciarProducto unProducto = take 10 (nombre unProducto)

productoDeLujo :: Producto -> Bool
productoDeLujo unProducto = (elem 'x' (nombre unProducto)) || (elem 'z' (nombre unProducto))

nombre :: Producto -> String
nombre (nombreProducto, _) = nombreProducto

aplicarCostoDeEnvio :: Num a => a -> a -> a
aplicarCostoDeEnvio precio costoEnvio = precio + costoEnvio

productoCodiciado :: Producto -> Bool
productoCodiciado unProducto = esMayorA10 (length . nombre $ unProducto)

esMayorA10 :: Int -> Bool
esMayorA10 longitud = longitud > 10

productoCorriente :: Producto -> Bool
productoCorriente unProducto = (empiezaConVocal . nombre) unProducto

empiezaConVocal :: String -> Bool
empiezaConVocal nombre = (elem . head $ nombre) "aeiouAEIOU"

productoXL :: Producto -> String
productoXL unProducto = ((++) . nombre $ unProducto) "XL"

versionBarata :: Producto -> String
versionBarata unProducto = reverse (descodiciarProducto unProducto)


{-
Tipado de funciones:

take :: Int -> [a] -> [a]

drop :: Int -> [a] -> [a]

head :: [a] -> a

elem :: Eq a => a -> [a] -> Bool

reverse :: [a] -> [a]
-}