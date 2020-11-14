module Hey.Extra.Row where

import Prelude

import Data.Maybe (Maybe, maybe)
import Data.Symbol (class IsSymbol, SProxy(..))
import Prim.Row as Row
import Prim.RowList (class RowToList, Cons, Nil) as RL
import Prim.RowList (kind RowList)
import Record as Rec
import Record.Builder (Builder)
import Record.Builder as Builder
import Type.Data.Row (RProxy(..))
import Type.Data.RowList (RLProxy(..))
import Type.RowList (class ListToRow) as RL

class SubRowBuilder (rl :: RowList) (sr :: # Type) (r :: # Type) | rl -> sr, r -> sr where
  subRowBuilder :: RLProxy rl -> {|r} -> Builder {} {|sr}

instance subRowBuilderNil :: SubRowBuilder RL.Nil () any where subRowBuilder _ _ = identity
instance subRowBuilderCons ::
  ( IsSymbol sym
  , SubRowBuilder tail out' row
  , RL.ListToRow tail out'          
  , Row.Cons sym ty trash row
  , Row.Cons sym ty out' out
  , Row.Lacks sym out'
  ) => SubRowBuilder (RL.Cons sym ty tail) out row where
  subRowBuilder _ r = Builder.insert sym val <<< subRowBuilder proxy r
    where proxy = RLProxy :: RLProxy tail
          sym = SProxy :: SProxy sym
          val = Rec.get sym r

class SubRow (sr :: # Type) (r :: # Type)
instance subRowImpl :: ( Row.Union sr trash r ) => SubRow sr r

class HasSubRow (sr :: # Type) (r :: # Type) | r -> sr where
  subRow :: RProxy sr -> {|r} -> {|sr}

instance hasSubRowImpl ::
  ( SubRowBuilder rl sr r
  , SubRow sr r
  , RL.RowToList sr rl
  ) => HasSubRow sr r where
  subRow _ = subRowBuilder proxy >>> Builder.build >>> ((#) {})
    where proxy = RLProxy :: RLProxy rl

maybeRow :: forall sr r . HasSubRow sr r => {|sr} -> Maybe {|r} -> {|sr}
maybeRow def = maybe def $ subRow proxy
  where proxy = RProxy :: RProxy sr
