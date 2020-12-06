module Day4
    ( someDay4
    )
where

data Passport m = Passport
    { birthYear :: m String
    , issueYear :: m String
    , expirationYear :: m String
    , height :: m String
    , hairColor :: m String
    , eyeColor :: m String
    , passportId :: m Int
    , countryId :: m Int
    }
