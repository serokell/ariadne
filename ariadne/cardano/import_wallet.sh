#!/bin/sh

set -e

CARDANO_SL_PATH=${1:-../cardano-sl}
ARIADNE_WALLET_PATH=ariadne/cardano/src/Ariadne/Wallet/Cardano

mkdir -p ${ARIADNE_WALLET_PATH}

echo "Copying code from cardano-sl"
rsync -av --delete \
    "${CARDANO_SL_PATH}"/wallet-new/src/Cardano/Wallet/Kernel* \
    ${ARIADNE_WALLET_PATH}
rsync -av --delete \
    "${CARDANO_SL_PATH}"/wallet-new/src/Cardano/Wallet/{WalletLayer.hs,WalletLayer/Kernel.hs,WalletLayer/Types.hs} \
    ${ARIADNE_WALLET_PATH}/WalletLayer

echo "Renaming modules..."
find ${ARIADNE_WALLET_PATH} \
    -name '*.hs' \
    -exec sed -e 's/\bCardano\.Wallet\./Ariadne.Wallet.Cardano./g' -i {} \;

echo "Prettifying with stylish-haskell..."
find ${ARIADNE_WALLET_PATH}/{Kernel,WalletLayer} \
    -name '*.hs' \
    -exec stylish-haskell -i -v {} \+

echo "Here are all new modules:"
find ${ARIADNE_WALLET_PATH} \
    -name '*.hs' \
    -exec sed -n -e 's/^module \([^ (]\+\).*/\1/p' {} \; | sort
