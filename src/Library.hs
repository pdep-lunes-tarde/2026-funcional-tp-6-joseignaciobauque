module Library where
import PdePreludat

data Ingrediente =
    Carne | Pan | Panceta | Cheddar | Pollo | Curry | QuesoDeAlmendras
    deriving (Eq, Show)

precioIngrediente Carne = 20
precioIngrediente Pan = 2
precioIngrediente Panceta = 10
precioIngrediente Cheddar = 10
precioIngrediente Pollo =  10
precioIngrediente Curry = 5
precioIngrediente QuesoDeAlmendras = 15

data Hamburguesa = Hamburguesa {
    precioBase :: Number,
    ingredientes :: [Ingrediente]
} deriving (Eq, Show)

-- Parte 1: Hamburgesas

cuartoDeLibra :: Hamburguesa
cuartoDeLibra = Hamburguesa { precioBase = 20, [Pan, Cheddar, Carne, Pan]}

agregarIngrediente :: Ingrediente -> Hamburguesa -> Hamburguesa
agregarIngrediente unIngrediente unaHamburguesa = unaHamburguesa {
    ingredientes = unIngrediente : ingredientes unaHamburguesa
}

filtracionIngrediente :: Ingrediente -> [Ingrediente] -> Bool
filtracionIngrediente ing = any (== ing)

tieneCarne :: Hamburguesa -> Bool
tieneCarne = filtracionIngrediente Carne . ingredientes

tienePollo :: Hamburguesa -> Bool
tienePollo  = filtracionIngrediente Pollo . ingredientes

agrandar :: Hamburguesa -> Hamburguesa
agrandar unaHamburguesa
  | tieneCarne unaHamburguesa = agregarIngrediente Carne unaHamburguesa
  | tienePollo unaHamburguesa = agregarIngrediente Pollo unaHamburguesa

aplicarPorcentaje :: Number -> Number -> Number
aplicarPorcentaje unPorcentaje cienporciento = (cienporciento * unPorcentaje)/100

descuento :: Number -> Hamburguesa -> Hamburguesa
descuento porcentajeEnNumero unaHamburguesa = unaHamburguesa {
    precioBase = precioBase unaHamburguesa - aplicarPorcentaje porcentajeEnNumero (precioBase unaHamburguesa)
}

pdepBurger :: Hamburguesa
pdepBurger = (agrandar.agrandar.agregarIngrediente Cheddar.agregarIngrediente Panceta.aplicarPorcentaje20) cuartoDeLibra 
   