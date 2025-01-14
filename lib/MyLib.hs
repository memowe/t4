module MyLib where

type Name = String

welcome :: Name -> String
welcome name = "Welcome, " ++ name ++ "!"
