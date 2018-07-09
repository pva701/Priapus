{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE RankNTypes            #-}

module Buchi
    ( Transitions
    , BuchiAutomaton (..)
    , GenBuchiAutomaton (..)
    , Layer
    , createTransitions

    , intersectBuchiAutomatons
    , gbaToBuchiAutomaton
    , checkEmptiness
    ) where

import Universum

import qualified Data.Map as M
import qualified Data.Set as S

-- Non-deterministic buachi automaton
type Transitions alph state = Map state (Map alph (Set state))

createTransitions :: forall alph state . (Ord state, Ord alph) => [(state, alph, state)] -> Transitions alph state
createTransitions = M.unionsWith (M.unionWith (<>)) . map (\(s1, a, s2) -> M.singleton s1 (M.singleton a (S.singleton s2)))

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
    } deriving (Show)

data GenBuchiAutomaton alph state = GenBuchiAutomaton
    { baTransitions :: Transitions alph state
    , baInit        :: Set state
    , baFinal       :: Set (Set state)
    -- pva701: baFinal shouldn't be empty, if so then GBA can't be converted
    -- to BuchiAutomaton because the GBA accepts any run, but
    -- there is no equalent Buchi automaton which accepts any run
    -- probably we should add new constructor to it
    } deriving (Show)

-- GBA to Buchi automaton --

newtype Layer = Layer Int
    deriving (Eq, Ord, Show)

gbaToBuchiAutomaton
    :: forall alph state . (Ord alph, Ord state)
    => GenBuchiAutomaton alph state -> BuchiAutomaton alph (state, Layer)
gbaToBuchiAutomaton (GenBuchiAutomaton t i f) = BuchiAutomaton t1 i1 f1
  where
    n = length fxs
    fxs = S.toList f

    i1 = S.fromList $ map (, Layer 0) $ S.toList i
    f1 = case fxs of
            []     -> S.fromList $ map (, Layer 0) $ M.keys t ++ S.toList i
            (h:_)  -> S.fromList $ map (, Layer 0) $ S.toList h

    t1 = createTransitions $ concatMap constructEdges $ toFlatTransitions t

    constructEdges :: (state, alph, state) -> [((state, Layer), alph, (state, Layer))]
    constructEdges (s1, a, s2)
        | null fxs  = [((s1, Layer 0), a, (s2, Layer 0))]
        | otherwise = do
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
anotherLayer FirstL  = SecondL
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


-- Check emptiness --

type Used state = Set state
type Graph state = Map state (Set state)
type Components state = Map state Int

checkEmptiness :: (Show state, Show alph, Ord state) => BuchiAutomaton alph state -> Bool
checkEmptiness (BuchiAutomaton tr inp fin) = do
    let gr = transitionsToGraph tr -- graph without letters
        invGr = invGraph gr        -- inverted graph
    let !allNodes = S.toList $ S.fromList $ M.keys tr ++ S.toList inp ++ S.toList fin
    -- Finding SCC
    let !order = dfs1 gr allNodes
    let comp = dfs2 invGr order -- map from node to number of component
    -- Condensing graph
    let condGraph = condenseGraph comp gr
    let getComp v = fromMaybe (error $ "component for " <> show v <> " in condense graph not found") (M.lookup v comp)
        compSize = M.fromListWith (+) $ map (\x -> (x,1::Int)) $ M.elems comp -- map from component to its size
        producesCycle cp = -- component can produce cycle if either it's not trivial or it has a loop
            fromMaybe (0 :: Int) (flip M.lookup compSize cp) > 1 ||
                                cp `S.member` fromMaybe mempty (M.lookup cp condGraph)
        compInps = map getComp $ toList inp -- componets of input nodes
        finProdusingCycle = S.fromList $ filter producesCycle $ map getComp $ toList fin -- final nodes which can produce a cycle
    let !visited = runDfs pass identity (dfs (const pass)) condGraph compInps -- visited nodes from input nodes
    all (flip S.notMember finProdusingCycle) $ toList visited

dfs1 :: Ord state => Graph state -> [state] -> [state]
dfs1 gr nodes = snd $ runDfs pass _1 (dfs (modify . second . (:))) gr nodes

dfs2 :: forall state . Ord state => Graph state -> [state] -> Components state
dfs2 gr nodes = (^. _3) $ (runDfs afterRun _1 (dfs afterDfs) gr nodes)
  where
    afterDfs :: state -> State (Used state, Sum Int, Components state) ()
    afterDfs v = do
        Sum c <- gets (view _2)
        modify (_3 %~ M.insert v c)
    afterRun = modify (_2 %~ (<> Sum 1))

runDfs
    :: (Monoid s, Ord state)
    => State s ()
    -> Lens' s (Used state)
    -> (Lens' s (Used state) -> Graph state -> state -> State s ())
    -> Graph state
    -> [state]
    -> s
runDfs afterRun ln dfsF gr nodes = flip execState mempty $
  forM_ nodes $ \v -> do
      used <- use ln
      unless (v `S.member` used) $ do
          dfsF ln gr v
          afterRun

dfs :: Ord state => (state -> State s ()) -> Lens' s (Used state) -> Graph state -> state -> State s ()
dfs after ln gr v = do
    modify (ln %~ S.insert v)
    used <- use ln
    forM_ (maybe [] toList $ M.lookup v gr) $ \to ->
        unless (to `S.member` used) $ dfs after ln gr to
    after v

condenseGraph :: Ord state => Components state -> Graph state -> Graph Int
condenseGraph comps = foldl' addEdges mempty . M.toList
  where
    addEdges gr (fr, toNodes) = foldl' (addEdge $ getComp fr) gr $ S.toList toNodes

    addEdge fr gr (getComp -> to) = case M.lookup fr gr of
        Nothing -> M.insert fr (S.singleton to) gr
        Just s  -> M.insert fr (to `S.insert` s) gr

    getComp v = fromMaybe (error "vertex not found in components") $ M.lookup v comps

transitionsToGraph :: Ord state => Transitions alph state -> Graph state
transitionsToGraph = fmap (S.fromList . concatMap toList .  toList)

invGraph :: Ord state => Graph state -> Graph state
invGraph = foldl' f mempty . M.toList
  where
    f gr (fr, to) = foldl' (addEdge fr) gr $ toList to
    addEdge to gr fr = case M.lookup fr gr of
        Nothing -> M.insert fr (S.singleton to) gr
        Just s  -> M.insert fr (to `S.insert` s) gr
