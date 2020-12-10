docker build -t aoc/profile .
docker run -it --name aoc_profile aoc/profile
docker cp aoc_profile:/src/aoc/advent-of-code-2k20-profile.prof ./
docker cp aoc_profile:/src/aoc/advent-of-code-2k20-profile.ps ./
docker rm aoc_profile
