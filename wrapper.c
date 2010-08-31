//for explanations see http://www.haskell.org/ghc/docs/latest/html/users_guide/ffi-ghc.html#using-own-main

#include <SDL/SDL.h>
#include <SDL/SDL_mixer.h>
#include <HsFFI.h>

#ifdef __GLASGOW_HASKELL__
// #include "SDLWrapper_stub.h"
#endif

#ifdef __GLASGOW_HASKELL__
// extern void __stginit_SDLWrapper ( void );
extern void __stginit_Main( void );
#endif

int main(int argc, char *argv[])
{
  hs_init(&argc, &argv);
#ifdef __GLASGOW_HASKELL__
  // hs_add_root(__stginit_SDLWrapper);
  hs_add_root(__stginit_Main);
#endif

  haskell_main();

  hs_exit();
  return 0;
}
