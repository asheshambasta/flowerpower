PKG=fht-api

sigint_handler()
{
  kill $(jobs -p)
  exit
}

trap sigint_handler SIGINT

while true; do
  # $@ &
  ghcid -l -c "cabal new-repl $PKG --builddir dist-$PKG" & 
 
  # run_server &
  PID1=$!

  inotifywait \
      -e modify \
      -e move \
      -e create \
      -e delete \
      -e attrib \
      $PKG/$PKG.cabal \
      fht-data/fht-data.cabal -r fht-data/src \
      --exclude ".*flycheck.*|.*\#.*"
  kill $(jobs -p)
  sleep 3
done

