################################################################################
# Configuration
################################################################################

GHC = ghc
GHCI = ghci
# GHC_FLAGS = -O2 -fforce-recomp -ibenchmarks -isrc -itests
GHC_FLAGS = -O2 -fforce-recomp -ibenchmarks -itests -isrc

BENCHMARK_FLAGS = --resamples 10000

benchmark:
	$(GHC) $(GHC_FLAGS) --make -main-is RunHtmlBenchmarks benchmarks/RunHtmlBenchmarks.hs
	./benchmarks/RunHtmlBenchmarks $(BENCHMARK_FLAGS) -o report.html
