
The Basic Model
===============

**Keywords** _community, member, registry, decision, event, domain, influence, proposal, vote, ballot, resolution, resolution outcome._

Let us describe informally what it means to make collaborative decision-making. 

First a notion of **community** as a set of **members** is required, all of the decision
power is encapsulated within such community. Secondly, the community must have a 
**registry** to authenticate and authorise members. Following, **decisions** may be triggered
by incoming **events** which are categorised by **domain**. Then a separation is made
between the **influence** that each member has on the decision, and the way in which a
decision is **resolved** into a **resolution outcome** after **votes** over **proposals** have
been collected in a **ballot box**, the voting **ballot** represents not a vote count on a
proposal, but allocated influence of the voting member over the different proposals. 
Proposals might be predefined or previously collected, but we will leave that for a 
future section. Influence distribution and decision resolution are actually just functions, 
called the **influence distribution function** and the **decision resolution function**, and
these together are called the **decision system functions**.

It is desired to have modular parts of the model, which when changed, the community's way of deciding changes, and subsequentially the community changes, these parts must be formalized or semi-formalized mathematically in order to have a full understanding of them, to be able to reduce it to unambiguous code, measure its impact, and proof properties or extend them by providing ways of combination.

This documentation will attempt to do such semi formalization of the parts of the model and will use the programming language Haskell as the chosen tool since it stands in between programmers and mathematicians. With this we would like to achieve some kind of denotational semantics, so to cater to the mathematical rigor as much as to the programming implementation and computational challenges.

The basic technique used in this documentation to be able to describe things without going into unnecessary specifics of the data or the technical implementation, is "tagless final", where Haskell's type classes help us decouple specification from implementation, leaving the former to this documentation, and the latter to the engineers who wish to write a Kratia Engine in any programming language.

The first parts that we will semi formalize, are the concept of a community, a member and a registry, allowing us to continue with the decision system functions, which are of uttermost importance since they stand at the core of Kratia.

## Member, Community, and the Registry

A community may be easily defined as a set of members, and a registry as a mechanism for adding, removing, and authenticating members, also this mechanism is able to authorize these operations; in a future section we will explain that operations on this mechanism should only be triggered by a decision made through the meta-decision system of the community.

In order to better handle communities and members, we will create data models by reference, we use a data type that should represent a unique identifier for such models, in the same form UUIDs work for databases or hash addresses for blockchains. `Community` and `Member` are parametrized to represent the type of the members which are loaded into memory by using an operation on the registry.

```haskell
type Address = String

data Community a = CommunityRef Address

data Member a = MemberRef Address

class Registry f a where

  isMember :: Community a -> Member a -> f Bool

  load :: Community a -> Member a -> f (Maybe a)

  loadAll :: Community a -> f [a]
```

## Influence Distribution

When decisions are to be made, a community might need a notion of influence distribution, so that more capable or more affected individuals have more decision power over others. Given the set of members of a community `M` and a measurement of influence `Infl`, the influence distribution is a function that maps every member with its influence amount. 

```haskell
dist :: M -> Infl
```

This influence distribution function will indeed map every member of the community `M` into some notion of influence, but we need to connect it to our current models and code. To do so we take advantage of the parametrized `a` of `Member` and `Community`

```haskell
type Influence = Rational

class InfluenceDistribution f a method where
  dist :: Registry f a => Community a -> Member a -> method -> f Influence
```

As you can see, `dist` has a stronger restriction now, it creates a relation between `Community`, `Member`, and a data type that parametrizes how the distribution will be made, namely `method`, such data type will carry the context needed to make the influence distribution according to a specific method, and will allow us to model different existing and currently nonexisting ways of distributing influence on a community. Also, it is important to note the relationship created by the polymorphic type `a`, such type is the data of a `Member a` that we can extract from a community `Community a` using registry functions `Registry f a`.

### Democracy

Given the class `InflueceDistribution`, now we have a simple dsl to model different distributions of influence, the simplest ones being democracy and totalitarianism since those are the 2 extremes of the distribution, i.e. all influence to one member or the same influence to each member. 

```haskell
data Democratic = Democratic

instance Functor f => InfluenceDistribution f a Democratic where
  alloc community member _ = fmap doAllocation (isMember community member)
    where doAllocation True = 1.0
          doAllocation False = 0.0
```

This instance is giving the same amount of influence on each member recognized by the `Registry` class, arguably succesfully modeling the notion of pure democracy, where every member of a community has the same amount of saying on each decision. We are not using the `method` argument for much but to distinguish the class instance to be used. In the next example, we will change that.

### Totalitarianism
Modeling totalitarianism is rather straightforward as well. On this implementation we removed the membership check, showing that there can be influence distribution functions which allocate influence to non-members of the community. Also, we are introducing the usage of `method` as a way to add context to the allocation of influence, in this case, to identify the member that will be granted all the influence.

```haskell
data Totalitarian a = Dictator (Member a)

instance Applicative f => InfluenceDistribution f a (Totalitarian a) where
  dist _ member (Dictator dic) = pure $ if member == dic then 1.0 else 0.0
```

### Geolocation Based

We have achieved a good level of polymorphism and generality on our definition, which allows us to be a notch more creative about how we may do the distribution. Depending on the domain, more complex notions of distributions can be done, for example, if the issue related to the decision that has to be done, has a position within its nature, then we can weight the members based on their [inverse distance](https://en.wikipedia.org/wiki/Inverse_distance_weighting) to the issue, i.e. members closer to the issue have more influence than others.

```haskell
type Location = String

type InverseDistanceWeight = Rational

data GeolocationBased = GeoBased Location

instance Applicative f => InfluenceDistribution f (Location -> InverseDistanceWeight) GeolocationBased where
  dist community member (GeoBased location) = fmap doAllocation (load community member)
    where doAllocation (Just weight) = weight location
          doAllocation Nothing = 0.0
```

* EDIT_NOTE: explain the usage of the polymorphic `a`
