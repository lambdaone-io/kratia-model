
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

Note that we made use of the polymorphic `a` in `Member a`, `Community a` and within the `InfluenceDistribution` class, taking advantage of the strong relationship to verify that the operation can only be done with a `Registry` that is able to fetch the location of the members, in this case we used the function `load` from the `Registry` to fetch a function that given a `Location`, it returns the inverse distance of the member, allowing us to use the `Registry` as the only source of truth of the data of the members, and letting the data type `GeolocationBased` carry the context of the distribution and dictate the point of reference for the weighting. 

This final implementation should work as an exceptional example for implementing infinite more influence allocation functions, providing the promised building blocks or exchangeable modules for distributing influence on a community, effectively changing the behaviour of the collaborative decision-making.

## Decision Resolution

Before we work on the notion of vote collection and to complete the notion of a decision system, we need to provide the blocks required to calculate the final outcome of the decision once the voting has ended; we will model the final voting as a mapping between the proposals in the ballot and an aggregation or accumulation of influence `P |-> Infl`, in other words, all the accumulated influence each proposal obtained by summing the votes of the members, and we will call it an `InfluenceAllocation`.

```haskell
data InfluenceAllocation p = InflAlloc (Map p Influence)
```

Given one decision, different InfluenceAllocation instances might come out of the voting process, and the summed influence in the allocation might be less or equal to the total theoretical influence of the whole community (dictated by the influence distribution function), this is so to model the turnout on the voting process (that is the number of members that actually voted), if the voting process ends and if not every member voted, it might be that the summed influence of the InfluenceAllocation is less than the total influence distributed on the community.

To model the decision resolution function we will write something similar as in the influence distribution.

```haskell
class DecisionResolution p method where
	resolve :: InfluenceAllocation p -> Influence -> method -> [p]
```

Notice that we have made a normal type class instead of one that parametrizes over an effect, that is because, in contrast to the influence distribution function, the decision resolution is pure, it does not depend on the current state of the community's registry or in the current state of the members, this allows us to make a function that takes the outcome of a voting session and the total distributed influence over the community, and return 0, 1 or several proposals that resolve, with such list we can model a decision that didn't resolve at all, or that has several outcomes.

It is also valuable to note that we are parametrizing the method of the resolution, allowing us once more to distinguish between different methods and bring context to the computation.

### Majority

The first decision resolution method we will examplify is also probably the most common; Majority is the resolution method where the proposal with the most accumulated influence, passes.

```haskell
data Majority = Majority

instance DecisionResolution p Majority where
  resolve (InflAlloc mapping) _ _ = fst result
    where result = foldlWithKey combine ([], 0.0) mapping
          combine :: ([p], Influence) -> p -> Influence -> ([p], Influence)
          combine (lastProposals, lastInfl) proposal influence
            | lastInfl < influence = ([proposal], influence)
            | lastInfl == influence && lastInfl /= 0.0 = ((proposal : lastProposals), influence)
            | otherwise = ([], 0.0)
```

Notice that this method allows for ties, so if there are two or more proposals with the same influence, they all will be part of the outcome; and also it allows for non-resolution when every proposal had 0 influence accumulated. As you might see, there could be other potential implementations of the majority method, like the one which does not resolve if there are ties, i.e. `if length result > 1 then [] else result`.

### Low Inertia Resolution with Binary Proposals

The decision resolution function can also specialize in the type of proposals, for the next example we will give another basic type, the binary proposals.

```haskell
data BinaryProposal = Yes | No
```

Members can simply vote yes or no, and be used in the notion of low inertia resolution, this means that only the `Yes` proposal passes if it accumulates a percentage of the total distributed influence, that is:

```haskell
                             -- A number in [1, 0)
data LowInertiaResolution = LIR Rational
```

Here the data constructor wraps a rational number between 1 and 0, e.g. if such number is 0.7, it models the required 70% of influence to be allocated on the `Yes` proposal.

```haskell
instance DecisionResolution BinaryProposal LowInertiaResolution where
  resolve (InflAlloc mapping) total (LIR required) =
      if accumulated >= requiredInfluence then [Yes] else [No]
    where requiredInfluence = total * required
          accumulated = foldlWithKey combine 0.0 mapping
          combine :: Influence -> BinaryProposal -> Influence -> Influence
          combine acc Yes x = acc + x
          combine acc No _ = acc
```

If we model the resolution method "Unanimity" as the one that requires everyone to vote `Yes` in order for the decision to pass, one could trivially proof that unanimity is a resolution method with binary proposals and low inertia resolution with `LIR 1.0`. This is the first example of the Kratia model providing equational reasoning to the complex phenomena of collaborative decision making.

### Turnout requirements and other validations

Every resolution method could make turnout requirements by using the total influence on the second argument of the decision resolution function, although when connecting with the rest of the system, one may need to make validation on the congruency of the influence, that is, it is an invariant that the sum of the influence allocated on each proposal, is equal or less than the total distributed influence on the community.
