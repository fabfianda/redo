BIN_PATH=.stack-work/install/x*/*/*/bin

redo-ifchange src/Redo.hs

stack purge
stack build
cp $BIN_PATH/redo-exe redo--redoing
