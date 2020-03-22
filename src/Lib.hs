{-# LANGUAGE DeriveGeneric, OverloadedStrings, OverloadedLabels #-}

module Lib
    ( someFunc
    ) where

import Database.Selda
import Database.Selda.SQLite

data Pet = Dog | Horse | Dragon 
    deriving (Show, Read, Bounded, Enum)
instance SqlType Pet

data Person = Person
    { id :: ID Person
    , name :: Text
    , age :: Int
    , pet :: Maybe Pet
    } deriving Generic
instance SqlRow Person

people :: Table Person
people = table "people" [#id :- autoPrimary]

someFunc :: IO ()
someFunc = withSQLite "people.sqlite" $ do
    tryDropTable people
    createTable people
    insert_ people
        [ Person def "Velvet"    19 (Just Dog)
        , Person def "Kobayashi" 23 (Just Dragon)
        , Person def "Miyu"      10 Nothing
        ]

    adultsAndTheirPets <- query $ do
        person <- select people
        restrict (person ! #age .>= 18)
        return (person ! #name :*: person ! #pet)
    liftIO $ print adultsAndTheirPets