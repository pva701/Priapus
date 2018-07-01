{-# LANGUAGE DuplicateRecordFields #-}

module Buchi
    ( BuchiAutomaton (..)
    , GenBuchiAutomaton (..)

    , intersectBuchiAutomatons
    , gbaToBuchiAutomaton
    ) where

import           Universum

import qualified Data.Map  as M
import qualified Data.Set  as S

-- Non-deterministic buachi automaton
type Transitions alph state = Map state (Map alph (Set state))

createTransitions :: forall alph state . (Ord state, Ord alph) => [(state, alph, state)] -> Transitions alph state
createTransitions = foldl' addEdge mempty
  where
    addEdge :: Transitions alph state -> (state, alph, state) -> Transitions alph state
    addEdge gr (s1, a, s2) = M.alter f s1 gr
      where
        f :: Maybe (Map alph (Set state)) -> Maybe (Map alph (Set state))
        f Nothing     = Just (M.singleton a (S.singleton s2))
        f (Just byAl) = Just $ M.alter f' a byAl

        f' :: Maybe (Set state) -> Maybe (Set state)
        f' Nothing  = Just (S.singleton s2)
        f' (Just s) = Just (S.insert s2 s)

toFlatTransitions :: Transitions alph state -> [(state, alph, state)]
toFlatTransitions gr = do
    (fr, byAl) <- M.toList gr
    (a, toSet) <- M.toList byAl
    to <- S.toList toSet
    pure (fr, a, to)

data BuchiAutomaton alph state = BuchiAutomaton
    { baTransitions :: Transitions alph state
    , baInit        :: Set state
    , baFinal       :: Set state
    }

data GenBuchiAutomaton alph state = GenBuchiAutomaton
    { baTransitions :: Transitions alph state
    , baInit        :: Set state
    , baFinal       :: Set (Set state)
    -- pva701: baFinal shouldn't be empty, if so then GBA can't be converted
    -- to BuchiAutomaton because the GBA accepts any run, but
    -- there is no equalent Buchi automaton which accepts any run
    -- probably we should add new constructor to it
    }

-- GBA to Buchi automaton --

newtype Layer = Layer Int
    deriving (Eq, Ord, Show)

gbaToBuchiAutomaton
    :: forall alph state . (Ord alph, Ord state)
    => GenBuchiAutomaton alph state -> BuchiAutomaton alph (state, Layer)
gbaToBuchiAutomaton (GenBuchiAutomaton t i f) = BuchiAutomaton t1 i1 f1
  where
    headL [] = error "invalid GBA: empty set of sets"
    headL (x:_) = x

    n = length fxs
    fxs = S.toList f

    i1 = S.fromList $ map (, Layer 0) $ S.toList i
    f1 = S.fromList $ map (, Layer 0) $ S.toList $ headL fxs

    t1 = createTransitions $ concatMap constructEdges $ toFlatTransitions t

    constructEdges :: (state, alph, state) -> [((state, Layer), alph, (state, Layer))]
    constructEdges (s1, a, s2) = do
        (idx, final) <- zip [0..] fxs
        pure $
          if s1 `S.member` final then ((s1, Layer idx), a, (s2, Layer $ (idx + 1) `mod` n))
          else ((s1, Layer idx), a, (s2, Layer idx))

-- Intersection --

type InvTransitions alph state = Map alph (Set (state, state))

transitionToInv
    :: forall alph state . (Ord alph, Ord state)
    => Transitions alph state -> InvTransitions alph state
transitionToInv = foldl' f mempty . M.toList
  where
    f :: InvTransitions alph state -> (state, Map alph (Set state)) -> InvTransitions alph state
    f m1 (s, m2) = M.unionWith (<>) m1 (fmap (S.fromList . map (s,) . S.toList) m2)


data TwoLayers = FirstL | SecondL
    deriving (Eq, Ord, Show)

anotherLayer :: TwoLayers -> TwoLayers
anotherLayer FirstL = SecondL
anotherLayer SecondL = FirstL

intersectBuchiAutomatons
    :: forall alph state1 state2 .
       (Ord state1, Ord state2, Ord alph)
    => BuchiAutomaton alph state1
    -> BuchiAutomaton alph state2
    -> BuchiAutomaton alph (state1, state2, TwoLayers)
intersectBuchiAutomatons (BuchiAutomaton t1 i1 f1) (BuchiAutomaton t2 i2 f2) =
    BuchiAutomaton (newTrans1 <> newTrans2) newInit newFinal
  where
    newInit = S.fromList [(x, y, FirstL) | x <- toList i1, y <- toList i2] -- start states of new automaton

    qA = M.keys t1 -- all states of first automaton
    newFinal = S.fromList [(x, y, SecondL) | x <- qA, y <- toList f2]

    t1inv = transitionToInv t1
    t2inv = transitionToInv t2
    newTrans1 = foldl' (addEdges t2inv f1 FirstL identity) mempty (M.toList t1inv)
    newTrans2 = foldl' (addEdges t1inv f2 SecondL (\(x, y, z) -> (y, x, z))) mempty (M.toList t2inv)

    addEdges
        :: (Ord state', Ord state)
        => InvTransitions alph state''
        -> Set state'
        -> TwoLayers
        -> ((state', state'', TwoLayers) -> state)
        -> Transitions alph state
        -> (alph, Set (state', state'))
        -> Transitions alph state
    addEdges anotherTr final layer conv res (al, edges1) = case M.lookup al anotherTr of
        Nothing     -> res
        Just edges2 -> M.unionWith (<>) res $ createTransitions $ do
            (sa1, sa2) <- toList edges1
            let toLayer = if sa1 `S.member` final then anotherLayer layer else layer
            (sb1, sb2) <- toList edges2
            let fromSt = conv (sa1, sb1, layer)
            let toSt = conv (sa2, sb2, toLayer)
            pure (fromSt, al, toSt)

