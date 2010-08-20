//for explanations see http://www.haskell.org/ghc/docs/latest/html/users_guide/ffi-ghc.html#using-own-main

#include <SDL.h>
#include <HsFFI.h>

#ifdef __GLASGOW_HASKELL__
#include "SDLChipsetViewerWrapper_stub.h"
#endif

#ifdef __GLASGOW_HASKELL__
extern void __stginit_SDLChipsetViewerWrapper ( void );
#endif

int main(int argc, char *argv[])
{
  hs_init(&argc, &argv);
#ifdef __GLASGOW_HASKELL__
  hs_add_root(__stginit_SDLChipsetViewerWrapper);
#endif

  haskell_main();

  hs_exit();
  return 0;
}
