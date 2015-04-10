mapl :: (a -> Bool) -> ([a] -> b) -> [a] -> [b]
mapl p f lst = map f (getBlocks lst)
    where
        skip [] = []
        skip (x:xs) = if p x then x:xs else skip xs
        get [] = ([], [])
        get (x:xs) = let (newBlock, remain) = get xs in (if p x then (x:newBlock, remain) else ([], xs))
        getBlocks [] = []
        getBlocks lst = let (newBlock, remain) = get (skip lst) in appendBlock newBlock (getBlocks remain)
        appendBlock [] blocksLst = blocksLst
        appendBlock newBlock blocksLst = newBlock:blocksLst
