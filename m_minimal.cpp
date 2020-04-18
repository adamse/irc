#include <inspircd.h>
#include "HsFFI.h"
#include "Minimal_stub.h"

class MinimalModule : public Module {
  HsStablePtr state;

  public:
  MinimalModule() {
    int argc = 0;
    char * argv[] = {NULL};
    char ** argp = argv;
    hs_init(&argc, &argp);
    state = hs_module_init();
  }

  ~MinimalModule() {
    hs_module_cleanup(state);
    hs_exit(); // the GHC rts cannot be reloaded currently, hence this is a bit fishy to do :)
  }

  Version GetVersion() {
    return Version("my version!", VF_VENDOR);
  }
};

MODULE_INIT(MinimalModule)