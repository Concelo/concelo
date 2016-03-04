module Database.Concelo.Serializer
  ( ) where

byACL path value = Path.super (getValueACL value) path

withSerializer constructor f =
  ignisCred >>= \case
    Nothing -> error "cannot encrypt without private key"
    Just cred ->
      with ignisPRNG (f . constructor (getCredPrivate cred))

updateTrees (obsoleteValues, newValues) = do
  with ignisACLTrees \trees ->
    withSerializer ValueSerializer \serializer ->
      foldr update (((T.empty, T.empty, T.empty, T.empty), trees), serializer)
            $ T.union (T.keys obsoleteValues) (T.keys newValues) where

    update acl (((allObsolete, allNew, obsoleteTrees, newTrees),
                 trees), serializer) =

      (((T.union obsolete' allObsolete,
         T.union new' allNew,

         if ST.null (getAnnotatedValue tree) then
           obsoleteTrees else
           T.insert acl tree obsoleteTrees,

         if ST.null tree' then
           newTrees else
           T.insert acl (Annotated tree' acl) newTrees),

        if ST.null tree' then
          T.delete acl trees else
          T.insert acl tree' trees),

       serializer') where

        tree = fromMaybe ST.empty $ T.find (T.key acl) trees

        (tree', serializer', obsolete', new') =
          ST.update (annotatedValue tree) serializer
          (T.sub acl obsoleteValues) (T.sub acl newValues)

whoAmI = fromMaybe $ error "I don't know who I am!"

updateMyTree (allObsolete, allNew, obsoleteTrees, newTrees) = do
  cred <- get ignisCred >>= whoAmI
  challenge <- get ignisChallenge >>= whoAmI
  revision <- getThenUpdate ignisNextRevision (+1)
  time <- get ignisUpdateTime
   -- all trees from other writers using the same keypair we're using
   -- except for the most recent published revision, if applicable:
  obsoleteTrees <- get ignisPeerTrees

  with ignisMyTree \tree ->
    -- NB: the acl serializer will enumerate the key encrypted for all authorized readers
    withSerializer ACLTreeSerializer \serializer ->
      let (tree', serializer', obsolete', new') =
            ST.update (annotatedValue tree) serializer obsoleteTrees
            newTrees
          annotated = (Annotate tree'
                       (Stamp
                        (getIgnisPublic cred) challenge revision time)) in
      (((T.union obsolete' allObsolete,
         T.union new' allNew,

         if ST.isEmpty (getAnnotatedValue tree) then
           obsoleteTrees else
           T.insert key tree obsoleteTrees,

         if ST.isEmpty tree' then
           T.empty else
           T.singleton revision annotated),

        annotated),
       serializer')

updateRootTree (allObsolete, allNew, obsoleteTrees, newTrees) = do
  with ignisRootTree \tree ->
    withSerializer WriterTreeSerializer \serializer ->
      let (tree', serializer', obsolete', new') =
            ST.update tree serializer obsoleteTrees newTrees in
      (((T.union obsolete' allObsolete,
         T.union new' allNew),
        tree'),
       serializer')

maybeUpdateTrees =
  get ignisDiff >>= \case
    Nothing -> return ()
    Just diff ->
      updateACLTrees $ mapPair (T.indexPaths byACL) diff
      >>= updateMyTree
      >>= updateRootTree
      >>= \diff -> with ignisPublisher $ Pub.update diff
