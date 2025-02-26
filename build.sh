#!/bin/bash
npm i -g yarn
curl -L -O https://github.com/clojure/brew-install/releases/latest/download/linux-install.sh
chmod +x linux-install.sh
./linux-install.sh
export PATH=/tmp/clojure/bin:$PATH
yarn install
npx shadow-cljs release app

cp README.md public/app