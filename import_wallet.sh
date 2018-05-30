#!/bin/sh

echo "Renaming modules..."
find ariadne/src/Ariadne/Wallet/Cardano/ -name '*.hs' -exec sed -e 's/Cardano\.Wallet/Ariadne.Wallet.Cardano/g' -i {} \;

echo "Prettyfying with stylish-haskell..."
find ariadne/src/Ariadne/Wallet/Cardano/Kernel -name '*.hs' -exec stylish-haskell -i -v {} \+

echo "Here are all new modules:"
find ariadne/src/Ariadne/Wallet/Cardano/ -name '*.hs' -exec sed -n -e 's/^module \([^ (]\+\).*/\1/p' {} \; | sort
