module Dynamic.Environment where

import Data.List

type RegisterEntry = (Integer, Maybe String)
type Registers = [RegisterEntry]

nameEqual :: String -> RegisterEntry -> Bool
nameEqual name (_, Just n) = n == name
nameEqual _ (_, Nothing) = False

addReg :: Registers -> Maybe String -> (Registers, Integer)
addReg env val = ((nreg, val) : env, nreg)
    where nreg = case env of
                   ((reg, _) : _) -> reg + 1
                   [] -> 0

regGetAnon :: Registers -> (Registers, Integer)
regGetAnon env = addReg env Nothing

regGetOrAdd :: Registers -> String -> (Registers, Integer)
regGetOrAdd env name =
    case find (nameEqual name) env of
      Just (reg, _) -> (env, reg)
      Nothing -> addReg env $ Just name

regEmpty :: Registers
regEmpty = []
