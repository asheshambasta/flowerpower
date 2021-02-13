sigint_handler()
{
  kill $(jobs -p)
  exit
}

PKG=fht-backend

trap sigint_handler SIGINT

while true; do
  # $@ &
  # build executable.
  cabal build $PKG --disable-optimization 
  executable=./dist-newstyle/build/x86_64-linux/ghc-8.6.5/$PKG-0.1.0.0/x/$PKG/noopt/build/$PKG/$PKG
  # start main node
  $executable -D "dbname=flowerpower user=flowerpower" &
  sleep 3 && curl "localhost:3000/api/v1/plants" &

  PID1=$!
  echo Procs: $PID1 

  inotifywait \
      -e modify \
      -e move \
      -e create \
      -e delete \
      -e attrib \
      $PKG/$PKG.cabal \
      -r fht-data \
      -r fht-api \
      --exclude ".*flycheck.*|.*\#.*"
  kill $(jobs -p)
  sleep 3
done

