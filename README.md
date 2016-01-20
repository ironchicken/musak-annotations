## MuSAK Annotations

This package provides a tool for analysing the annotations produced by the [MuSAK](http://www.transforming-musicology.org/tools/metaMuSAK/) system.

### Installation

1. Ensure GHC and `cabal-install` are installed. Either install the
   [Haskell Platform](https://www.haskell.org/platform/), or for
   Debian and deriatives, try:

        $ sudo apt-get install ghc cabal-install

2. Clone the repository:

        $ git clone git@gitlab.doc.gold.ac.uk:transforming-musicology/musak-shapes.git

3. Create a Cabal sandbox and install the dependencies in it:

        $ cd musak-shapes
        $ cabal sandbox init
        $ cabal install --only-dependencies

4. Build:

        $ cabal build

### Usage

The tools provided in this package are intended to be used with the
annotation data generating by the MuSAK system. Some data from this
system is available to T-Mus project members from the Hearing Wagner
experiment.

1. Ensure `git-annex` is installed.

2. Retrieve the Hearing Wagner data repository:

        $ git clone ssh://tmus@igor.gold.ac.uk/~/data/hw-data.git

3. Retrieve some annotations and page images:

        $ cd hw-data
        $ git annex get Rh/annotations Rh/score

4. To dump all the segmented shapes from every page:

        $ cabal run musak-dump-shapes /path/to/hw-data/Rh/annotations/*.csv

5. To draw the shapes on the score pages:

        $ cabal run musak-highlight-shapes -a "/path/to/hw-data/Rh/annotations/*.csv" -s "/path/to/hw-data/Rh/score/*.jpg"

   If you use globs for the arguments, you need to quote them.

   **However**, this command pairs up the list of files in its two
   arguments and draws the shapes from a page onto the corresponding
   score image. The existing filenames in the score pages are fine
   because their lexicographic ordering is sensible. However the
   annotation CSV files have un-padded numerical components in their
   file names. You could fix this by adding some symlinks with
   sensible names, e.g.:

        $ cd /path/to/hw-data/Rh/annotations/
        $ ghci
        λ: :m Text.Printf
        λ: :m + System.PosixCompat.Files
        λ: sequence_ $ map (\n -> createSymbolicLink ("Rhpage" ++ (show (n+1)) ++ ".csv") ("Rh-" ++ (printf "%04d" n) ++ ".csv")) [1..252]

   And then execute `musak-highlight-shapes` with suitable globs:

        $ cabal run musak-highlight-shapes -a "/path/to/hw-data/Rh/annotations/Rh-*.csv" -s "/path/to/hw-data/Rh/score/Rh-*.jpg"

### Future

The next phase of this software will be to implement some shape
distance measures and then cluster the shapes by similarity.
