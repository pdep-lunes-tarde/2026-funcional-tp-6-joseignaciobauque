module Library where
import PdePreludat

data Ingrediente =
    Carne | PatiVegano | Pan | PanIntegral | BaconDeTofu | Panceta | Cheddar | Papas | Pollo | Curry | QuesoDeAlmendras
    deriving (Eq, Show)

precioIngrediente Carne = 20
precioIngrediente PatiVegano = 10
precioIngrediente Pan = 2
precioIngrediente PanIntegral = 3
precioIngrediente Panceta = 10
precioIngrediente Cheddar = 10
precioIngrediente Pollo =  10
precioIngrediente Curry = 5
precioIngrediente QuesoDeAlmendras = 15
precioIngrediente Papas = 10
precioIngrediente BaconDeTofu = 10

data Hamburguesa = Hamburguesa {
    precioBase :: Number,
    ingredientes :: [Ingrediente]
} deriving (Eq, Show)

-- Parte 1: Hamburgesas

precioHamburguesa :: Hamburguesa -> Number
precioHamburguesa unaHamburguesa = precioBase unaHamburguesa + sum(map precioIngrediente (ingredientes unaHamburguesa))

cuartoDeLibra :: Hamburguesa
cuartoDeLibra = Hamburguesa { precioBase = 20, ingredientes = [Pan, Cheddar, Carne, Pan]}

agregarIngrediente :: Ingrediente -> Hamburguesa -> Hamburguesa
agregarIngrediente unIngrediente unaHamburguesa = unaHamburguesa {
    ingredientes = unIngrediente : ingredientes unaHamburguesa
}

filtracionIngrediente :: Ingrediente -> [Ingrediente] -> Bool
filtracionIngrediente ing = any (== ing)

tieneCarne :: Hamburguesa -> Bool
tieneCarne = filtracionIngrediente Carne . ingredientes

tienePatiVegano :: Hamburguesa -> Bool
tienePatiVegano = filtracionIngrediente PatiVegano . ingredientes

tienePollo :: Hamburguesa -> Bool
tienePollo  = filtracionIngrediente Pollo . ingredientes

agrandar :: Hamburguesa -> Hamburguesa
agrandar unaHamburguesa
  | tieneCarne unaHamburguesa = agregarIngrediente Carne unaHamburguesa
  | tienePollo unaHamburguesa = agregarIngrediente Pollo unaHamburguesa
  | tienePatiVegano unaHamburguesa = agregarIngrediente PatiVegano unaHamburguesa
  | otherwise = unaHamburguesa

aplicarPorcentaje :: Number -> Number -> Number
aplicarPorcentaje unPorcentaje cienporciento = (cienporciento * unPorcentaje)/100

descuento :: Number -> Hamburguesa -> Hamburguesa
descuento porcentajeEnNumero unaHamburguesa = unaHamburguesa {
    precioBase = precioBase unaHamburguesa - aplicarPorcentaje porcentajeEnNumero (precioBase unaHamburguesa)
}

pdepBurger :: Hamburguesa
pdepBurger =
    (descuento 20 . agrandar . agrandar . agregarIngrediente Cheddar . agregarIngrediente Panceta) cuartoDeLibra

-- Parte 2: Algunas hamburguesas mas

dobleCuarto :: Hamburguesa
dobleCuarto = 
    (agregarIngrediente Cheddar . agrandar) cuartoDeLibra

bigPdep :: Hamburguesa
bigPdep = agregarIngrediente Curry dobleCuarto

delDia :: Hamburguesa -> Hamburguesa
delDia unaHamburguesa = 
    (descuento 30 . agregarIngrediente Papas) unaHamburguesa

-- Parte 3: Algunos Cambios mas

hacerVeggieIngrediente :: Ingrediente -> Ingrediente
hacerVeggieIngrediente Carne = PatiVegano
hacerVeggieIngrediente Pollo = PatiVegano
hacerVeggieIngrediente Cheddar = QuesoDeAlmendras
hacerVeggieIngrediente Panceta = BaconDeTofu

funcionPan :: Ingrediente -> Ingrediente
funcionPan Pan = PanIntegral

hacerVeggie :: Hamburguesa -> Hamburguesa
hacerVeggie unaHamburguesa = unaHamburguesa {
    ingredientes = map hacerVeggieIngrediente (ingredientes unaHamburguesa)
}

cambiarPanDePati :: Hamburguesa -> Hamburguesa
cambiarPanDePati unaHamburguesa = unaHamburguesa {
    ingredientes = map funcionPan (ingredientes unaHamburguesa)
}

dobleCuartoVegano :: Hamburguesa
dobleCuartoVegano =
    (hacerVeggie . cambiarPanDePati) dobleCuarto