chipset_viewer_test: test chipset_wrapper.o SDLChipsetViewerWrapper.hs *.hs
	ghc -prof -no-hs-main --make chipset_wrapper.o SDLChipsetViewerWrapper.hs -o chipset_viewer_test
SDLChipsetViewerWrapper_stub.h: SDLChipsetViewerWrapper.hs
	ghc -prof -no-hs-main --make SDLChipsetViewerWrapper.hs -c
chipset_wrapper.o: SDLChipsetViewerWrapper_stub.h
	ghc -prof -no-hs-main `sdl-config --cflags` -Wall chipset_wrapper.c -c
test: wrapper.o SDLWrapper.hs *.hs
	ghc -prof -no-hs-main --make wrapper.o SDLWrapper.hs -o test
SDLWrapper_stub.h: SDLWrapper.hs
	ghc -prof -no-hs-main --make SDLWrapper.hs -c
wrapper.o: SDLWrapper_stub.h
	ghc -prof -no-hs-main `sdl-config --cflags` -Wall wrapper.c -c
clean:
	rm -f *.hi *.o *_stub.c *_stub.h test
.PHONY: clean

