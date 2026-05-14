module Spec where
import PdePreludat
import Library
import Test.Hspec
import Control.Exception (evaluate)

correrTests :: IO ()
correrTests = hspec $ do
    describe "TP 5" $ do
        it "cuando se agranda una hamburguesa de carne se le agrega un pedazo de carne" $ do
            agrandar (Hamburguesa 20 [Pan, Carne, Pan]) `shouldBe`  Hamburguesa 20 [Carne, Pan, Carne, Pan]
        
        it "cuando se agranda una hamburguesa de pollo se le agrega un pedazo de pollo" $ do
          agrandar (Hamburguesa 20 [Pan, Pollo, Pan]) `shouldBe`  Hamburguesa 20 [Pollo, Pan, Pollo, Pan]

        it "cuando se agranda una hamburguesa de pati vegano se le agrega un pedazo de pati vegano" $ do
            agrandar (Hamburguesa 20 [Pan, PatiVegano, Pan]) `shouldBe`  Hamburguesa 20 [PatiVegano, Pan, PatiVegano, Pan]
        
        it "recibe un ingrediente y una hamburguesa y le agrega el ingrediente a la hamburguesa" $ do
            agregarIngrediente Pollo cuartoDeLibra `shouldBe` cuartoDeLibra { precioBase = 20, ingredientes = [Pollo, Pan, Cheddar, Carne, Pan]}
        
        it "recibe un % de descuento, y devuelve la hamburguesa con ese descuento aplicado al precio base" $ do
            descuento 10 cuartoDeLibra `shouldBe` cuartoDeLibra { precioBase = 18, ingredientes = [Pan, Cheddar, Carne, Pan]}
        
        it "la pdepBurger debe valer 110" $ do
            precioHamburguesa pdepBurger `shouldBe` 110
        
        it "la dobleCuarto debe valer 84" $ do
            precioHamburguesa dobleCuarto `shouldBe` 84
        
        it "la bigPdep debe valer 89" $ do
            precioHamburguesa bigPdep `shouldBe` 89
        
        it "dada una hamburguesa, le agrega Papas y un descuento del 30%" $ do
            delDia bigPdep `shouldBe` bigPdep { precioBase = 14, ingredientes = [Papas, Curry, Cheddar, Pan, Cheddar, Carne, Pan]}
        
        it "cambia todos los ingredientes base que hayan en la hamburguesa por PatiVegano" $ do
           hacerVeggie cuartoDeLibra `shouldBe` cuartoDeLibra { precioBase = 20, ingredientes = [Pan, QuesoDeAlmendras, PatiVegano, Pan]}
        
        it "cambia el Pan que haya en la hamburguesa por PanIntegral" $ do
            cambiarPanDePati cuartoDeLibra `shouldBe` cuartoDeLibra { precioBase = 20, ingredientes = [PanIntegral, Cheddar, Carne, PanIntegral]}
        
        


