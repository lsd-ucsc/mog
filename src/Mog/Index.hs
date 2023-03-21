{-# LANGUAGE TypeFamilies, DataKinds #-} -- TypeFamilies implies KindSignatures,ExplictNamespaces
-- FIXME: undicable-instances here lets us return error messages
{-# LANGUAGE UndecidableInstances #-}
module Mog.Index where

import GHC.TypeLits (type Nat, TypeError, type ErrorMessage(..))




-- * Inductive type-nats

data Index = Here | There Index

-- | Exists for documentation.
type family NatIndex (a :: Nat) :: Index where
    NatIndex 0 =                                                                         'Here
    NatIndex 1 =                                                                 ('There 'Here)
    NatIndex 2 =                                                         ('There ('There 'Here))
    NatIndex 3 =                                                 ('There ('There ('There 'Here)))
    NatIndex 4 =                                         ('There ('There ('There ('There 'Here))))
    NatIndex 5 =                                 ('There ('There ('There ('There ('There 'Here)))))
    NatIndex 6 =                         ('There ('There ('There ('There ('There ('There 'Here))))))
    NatIndex 7 =                 ('There ('There ('There ('There ('There ('There ('There 'Here)))))))
    NatIndex 8 =         ('There ('There ('There ('There ('There ('There ('There ('There 'Here))))))))
    NatIndex 9 = ('There ('There ('There ('There ('There ('There ('There ('There ('There 'Here)))))))))
    NatIndex _ = TypeError ('Text "too many") -- UndecidableInstances

-- | @RefIndex n ≡ NatIndex (n - 1)@ so that you can say @1@ to mean the next
-- relation, and @2@ to mean /two/ relations over. Accordingly @0@ isn't here
-- because you can't have a fk to yourself.
type family RefIndex (a :: Nat) :: Index where
    RefIndex 1 =                                                                 'Here
    RefIndex 2 =                                                         ('There 'Here)
    RefIndex 3 =                                                 ('There ('There 'Here))
    RefIndex 4 =                                         ('There ('There ('There 'Here)))
    RefIndex 5 =                                 ('There ('There ('There ('There 'Here))))
    RefIndex 6 =                         ('There ('There ('There ('There ('There 'Here)))))
    RefIndex 7 =                 ('There ('There ('There ('There ('There ('There 'Here))))))
    RefIndex 8 =         ('There ('There ('There ('There ('There ('There ('There 'Here)))))))
    RefIndex 9 = ('There ('There ('There ('There ('There ('There ('There ('There 'Here))))))))
    RefIndex _ = TypeError ('Text "too many") -- UndecidableInstances

