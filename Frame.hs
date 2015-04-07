-- Stack frames for the Tiger compiler. An instance of the Frame class must
-- provide:
--	* the location of all formal parameters
--	* instructions to shift between frames
--	* number of locals allocated so far (?)
--	* the label of the associated assembly
{-# LANGUAGE TypeFamilies #-}

module Frame where

import AST

class Frame f where
      -- The Access type specifies how a frame variable should be accessed
      -- An instance might provide FrameOffset and Register constructors.
      type Access :: *
      -- Each formal parameter is marked True if it escapes
      newFrame :: Identifier -> [Bool] -> f
      name :: f -> Identifier
      -- For each parameter, specify how it is stored by the callee
      formals :: f -> [Access]
      -- 
      allocLocal :: f -> Bool -> (f,Access)

data MipsFrame = MipsFrame {
     nameMips :: Identifier,
     formalsMips :: [Bool]
} deriving (Eq,Show)

data MipsAccess = FrameOffset Int | Register Temporary


instance Frame MipsFrame where
	 type Access = MipsAccess
	 newFrame n fs = MipsFrame { nameMips = n, formalsMips = fs }
	 name = nameMips
	 formals = formalsMips
	 allocLocal = undefined