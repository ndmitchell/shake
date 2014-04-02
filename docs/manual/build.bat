@mkdir _shake 2> nul
@ghc --make Build.hs -rtsopts -with-rtsopts=-I0 -outputdir=_shake -o _shake/build && _shake\build %*
