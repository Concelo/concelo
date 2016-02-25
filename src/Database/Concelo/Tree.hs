module Database.Concelo.Tree
  ( empty ) where

data Color = Red | Black

data Tree k v = Leaf | Node { getNodeKey :: k
                            , getNodeValue :: v
                            , getNodeRevision :: Integer
                            , getNodeSubTree :: Tree v
                            , getNodeLeft :: Tree v
                            , getNodeRight :: Tree v
                            , getNodeColor :: Color }

empty = Leaf

treeNode f = \case
  Leaf -> Leaf
  node -> f node

maybeNode f = \case
  Leaf -> Nothing
  node -> Just $ f node

sub = treeNode getNodeSubTree

key = maybeNode getNodeKey

value = maybeNode getNodeValue

findNode key = find where
  find = treeNode \node ->
    case compare key $ getNodeKey node of
      LT -> find $ getNodeLeft node
      GT -> find $ getNodeRight node
      EQ -> node

rotate f g = treeNode \node ->
  treeNode
  (\child -> L.set f (L.set g (L.get f child) node) child)
  (L.get g node)

leftRotate = rotate nodeLeft nodeRight

rightRotate = rotate nodeRight nodeLeft

rebalance' f g side nextSide

rebalance = do
  path <- get rebalancerPath
  if null path || getNodeColor (head path) == Black then
    get rebalancerRoot >>= L.set nodeColor Black
    else do
    let (parent:grandparent:_) = path
    if parent `nodeEqual` getNodeLeft grandparent then
      rebalance' nodeLeft nodeRight
      (getNodeRight parent) (getNodeRight grandparent)
      else
      rebalance' nodeRight nodeLeft
      (getNodeLeft parent) (getNodeLeft grandparent)

rebalanceOnInsert revision (path, root, inserted) =
  evalState rebalance $ Rebalancer path root inserted revision

insert key value revision subTree =
  rebalanceOnInsert . ins [] where

  blaze side node = (path', node', inserted) where
    (path', child, inserted) = ins (new:path) $ L.get side node
    node' = L.set getNodeRevision revision $ L.set side child node

  ins path = \case
    Leaf -> (path, new, new) where
      new = Node key value revision subTree Leaf Leaf Red

    node -> case compare key $ getNodeKey node of
      LT -> blaze path nodeLeft node
      GT -> blaze path nodeRight node
      EQ -> ([], node', node') where
        node' = node { getNodeRevision = revision
                     , getNodeValue = value
                     , getNodeColor = Red })
