data SymList a = ([a], [a])

-- Convert back from Standard Lists
fromSL :: SymList a -> [a]
fromSL (xs, ys) = xs ++ reverse ys

-- Fundamental Operations

-- snocSL
snocSL :: a -> SymList a -> SymList a
snocSL x (xs, ys) = if null xs then (ys, [x]) else (xs, x: ys)

-- lastSL
lastSL :: SymList a -> a
lastSL (xs, ys) = if null ys then head xs else head ys

-- consSL
consSL :: a → SymList a → SymList a
consSL x (xs, ys) = if null ys then ([x], xs) else (x: xs, ys)

-- headSL
headSL :: SymList a → a
headSL (xs, ys) = if null xs then head ys else head xs

-- tailSL
tailSL :: SymList a → SymList a 
tailSL (xs,ys)
	| null xs = if null ys then ⊥ else nilSL
	| single xs = (reverse vs,us)
	| otherwise = (tail xs, ys)
where (us, vs) = splitAt (length ys div 2) ys

-- initSL
initSL :: SymList a → SymList a
initSL (xs, ys)
	| null ys = if null xs then ⊥ else nilSL
	| single ys = (us, reverse vs)
	| otherwise = (xs, init ys)
where (us, vs) = splitAt (length xs div 2) xs
