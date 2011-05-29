--
-- Blink a LED on the Arduino Duemilanove using IDE libs from Atom/Haskell.
-- June 2010
-- By Lee Pike
-- BSD3
--

module Main where

import Language.Atom
import Data.Word

atomName = "AtomLED"

-- | Invoke the Atom compiler.
main :: IO ()
main = do
  (sch, _, _, _, _) <- compile atomName defaults {cCode = prePostCode} blink
  putStrLn $ reportSchedule sch

-- | Takes a type and a variable name and initializes it.
varInit :: Type -> String -> String -> String
varInit t var val = cType t ++ " " ++ var ++ " = " ++ val ++ ";"

prePostCode :: [Name] -> [Name] -> [(Name, Type)] -> (String, String)
prePostCode _ _ _ =
    (unlines
    [ (varInit Int16 "ledPin" "13")
    , "void avr_blink(void);"
    ]
    , unlines
    [
      "void setup()   {"
    , "  // initialize the digital pin as an output:"
    , "  pinMode(ledPin, OUTPUT);"
    , "}"
    , ""
    , "// set the LED on or off"
    , "void avr_blink() { digitalWrite(ledPin, state.AtomLED.on); }"
    , ""
    , "void loop() {"
    ,  "  " ++ atomName ++ "();"
    , "}"
    ]
    )

-- How many cycles is a phase?
ph :: Int
ph = 40000

-- Simple Atom to toggle an LED, leaving it on 8 times as long as it's off.
blink :: Atom ()
blink = do
  on <- bool "on" True

  period ph $ phase 0 $ atom "blinkOn" $ do
    call "avr_blink"
    on <== not_ (value on)

  period ph $ phase (quot ph 8) $ atom "blinkOff" $ do
    call "avr_blink"
    on <== not_ (value on)
