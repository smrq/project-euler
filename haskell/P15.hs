module P15 where
import Data.Array
main = print result

lattice n = lattice' (n+1, n+1) where
	memoLimit = 40
	latticeMemo = array ((0, 0), (memoLimit, memoLimit))
		[ ((x, y), lattice' (x, y)) | x <- [0..memoLimit], y <- [0..memoLimit] ]

	lattice' (1, 1) = 1
	lattice' (0, _) = 0
	lattice' (_, 0) = 0
	lattice' (x, y) = (latticeMemo ! (x-1, y)) + (latticeMemo ! (x, y-1))

result = lattice 20