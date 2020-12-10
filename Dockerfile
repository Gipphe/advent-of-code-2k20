FROM fpco/stack-build-small:lts-16.24 AS profile
WORKDIR /src/aoc
COPY ./package.yaml ./stack.yaml ./stack.yaml.lock ./advent-of-code-2k20.cabal ./
RUN stack bench --system-ghc --profile --no-run-benchmarks --dependencies-only
COPY ./ ./
CMD stack bench \
	--system-ghc \
	--profile \
	--ghc-options="-O2 -fforce-recomp -fprof-auto -fprof-cafs -with-rtsopts=\"-i0.01 -h -p\"" \
	&& hp2ps -e8in -c advent-of-code-2k20-profile.hp
