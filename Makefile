# -fno-rtti!
GHC_OPTS=-fforce-recomp -fPIC -rdynamic -dynamic -optc-fno-rtti $(INC)
GHC_LIBDIR = $(shell ghc --print-libdir)
GCC_OPTS=-rdynamic -fPIC -fvisibility=hidden -fvisibility-inlines-hidden -fno-rtti -I$(GHC_LIBDIR)/include $(INC)

%.hs_o: %.hs
	ghc $(GHC_OPTS) -osuf hs_o -c $*.hs

%.o: %.cpp
	ghc $(GHC_OPTS) -c $*.cpp
	# gcc $(GCC_OPTS) -c $*.cpp

m_minimal.so: Minimal.hs_o m_minimal.o
	ghc $(GHC_OPTS) -c $^ -o $@ -shared -lHSrts_thr_l-ghc$(GHC_VERSION)

all: m_minimal.so

test: m_minimal.so .FORCE
	$(RM) -f test
	gcc $(INC) -ldl test.cpp -o test

install: m_minimal.so
	cp m_minimal.so $(INSPIRCD_RUN_PATH)/modules/

clean:
	$(RM) *.hs_o
	$(RM) *.hi
	$(RM) *.o
	$(RM) *.so
	$(RM) *_stub.h
	$(RM) test

.FORCE: