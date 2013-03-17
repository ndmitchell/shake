mkdir --parents .hpc/shake
ghc -main-is Development.Make.Main -package transformers --make Development/Make/Main.hs Paths.hs -w -odir .hpc/shake -hidir .hpc/shake -o .hpc/shake/shake
SHAKE=`pwd`/.hpc/shake/shake
cd ../gnumake/tests; ./run_make_tests.pl -make_path $SHAKE \
    features/comments \
    misc/general1 \
    misc/general2 \
    targets/clean \
    variables/flavors \

echo Total: 83 Tests in 39 Categories
