{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}

-- Member, Community, and the Registry
--

type Address = String

data Community a = CommunityRef Address

data Member a = MemberRef Address

class Registry f a where

  isMember :: Community a -> Member a -> f Bool

  load :: Community a -> Member a -> f (Maybe a)

  loadAll :: Community a -> f [a]

-- Influence Distribution

type Influence = Rational

class InfluenceDistribution f a method where
  dist :: Registry f a => Community a -> Member a -> method -> f Influence

-- Democracy

data Democratic = Democratic

instance Functor f => InfluenceDistribution f a Democratic where
  dist community member _ = fmap doAllocation (isMember community member)
    where doAllocation True = 1.0
          doAllocation False = 0.0

-- Totalitarianism

data Totalitarian a = Dictator (Member a)

instance (Applicative f,  Eq (Member a)) => InfluenceDistribution f a (Totalitarian a) where
  dist _ member (Dictator dic) = pure $ if member == dic then 1.0 else 0.0

-- Geolocation

type Location = String

type InverseDistanceWeight = Rational

data GeolocationBased = GeoBased Location

instance Eq GeolocationBased  where
  GeoBased x == GeoBased y =  x == y

instance Applicative f  => InfluenceDistribution f (Location -> InverseDistanceWeight) GeolocationBased where
  dist community member (GeoBased location) = fmap doAllocation (load community member)
    where doAllocation (Just weight) = weight location
          doAllocation Nothing = 0.0



