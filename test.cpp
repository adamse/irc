#include <stdio.h>
#include <dlfcn.h>
#include <inspircd.h>

typedef Module* (*init_fun_type)();

int main(int argc, char **argv) {

  void* object = dlopen("./m_minimal.so", RTLD_LAZY);

  if (object == NULL) {
    printf("failed to dlopen: %s\n", dlerror());
    return 1;
  }

  init_fun_type init_fun = (init_fun_type)dlsym(object, "inspircd_module_init");

  if (init_fun == NULL) {
    printf("failed to dlsym: %s\n", dlerror());
    return 2;
  }

  Module* mod = init_fun();
  delete mod;

  return 0;
}