
set -o errexit

package_dir=$(pwd)
tmp_dir=$(mktemp -d)

cabal sdist
cd $tmp_dir
aunpack $package_dir/dist/kraken*tar.gz
cd kraken*
cabal install --prefix $tmp_dir/install -j1 --ghc-options=-j4
cd $tmp_dir/install

echo port: 21128 >> config
echo krakenUris: [] >> config

./bin/kraken-web --config config &
pid=$!

curl http://localhost:21128/index.html
echo

kill $pid

cd $package_dir
rm -r $tmp_dir
