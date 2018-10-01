#!/bin/sh

set -e

CARDANO_SL_PATH=${1:-../cardano-sl}
ARIADNE_WALLET_PATH=ariadne/cardano/src/Ariadne/Wallet/Cardano
ARIADNE_TEST_PATH=ariadne/cardano/test/backend

mkdir -p ${ARIADNE_WALLET_PATH} ${ARIADNE_TEST_PATH}

echo "Copying code from cardano-sl"
rsync -av --delete \
    "${CARDANO_SL_PATH}"/wallet-new/src/Cardano/Wallet/Kernel* \
    "${CARDANO_SL_PATH}"/wallet-new/src/Cardano/Wallet/WalletLayer.hs \
    ${ARIADNE_WALLET_PATH}
rsync -av --delete \
    "${CARDANO_SL_PATH}"/wallet-new/src/Cardano/Wallet/WalletLayer/{Kernel.hs,Types.hs} \
    ${ARIADNE_WALLET_PATH}/WalletLayer

echo "Copying tests from cardano-sl"
rsync -av --delete \
    "${CARDANO_SL_PATH}"/wallet-new/test/unit/ \
    ${ARIADNE_TEST_PATH}

echo "Renaming modules..."
find ${ARIADNE_WALLET_PATH} ${ARIADNE_TEST_PATH} \
    -name '*.hs' \
    -exec sed -e 's/\bCardano\.Wallet\./Ariadne.Wallet.Cardano./g' -i {} \;

echo "Prettifying with stylish-haskell..."
find ${ARIADNE_WALLET_PATH}/{Kernel,WalletLayer} ${ARIADNE_TEST_PATH} \
    -name '*.hs' \
    -exec stylish-haskell -i -v -c .stylish-haskell.yaml {} \;

echo "Here are all new modules:"
find ${ARIADNE_WALLET_PATH} ${ARIADNE_TEST_PATH} \
    -name '*.hs' \
    -exec sed -n -e 's/^module \([^ (]\+\).*/\1/p' {} \; | sort
