import Control.Lens.Tutorial
import Control.Lens hiding (element)

-- use with
-- stack ghci --package lens-tutorial lens


-- data Point = Point { _x :: Double, _y :: Double } deriving (Show)
-- data Atom = Atom { _element :: String, _point :: Point }
-- data Molecule = Molecule { _atoms :: [Atom] } deriving (Show)- data Point = Point { _x :: Double, _y :: Double }

shiftAtomX :: Atom -> Atom
shiftAtomX = over (point . x) (+ 1)

shiftMoleculeX :: Molecule -> Molecule
shiftMoleculeX = over (atoms . traverse . point . x) (+ 1)

atom1 = Atom { _element = "C", _point = Point { _x = 1.0, _y = 2.0 } }
atom2 = Atom { _element = "O", _point = Point { _x = 3.0, _y = 4.0 } }
molecule = Molecule { _atoms = [atom1, atom2] }

shift lens = over lens (+1)

atomX :: Lens' Atom Double
atomX = point.x

moleculeX :: Traversal' Molecule Double
moleculeX = atoms.traverse.point.x
