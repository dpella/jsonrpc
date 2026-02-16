#!/bin/bash


rm -rf dist-newstyle/*-docs.tar.gz
rm -rf dist-newstyle/sdist/*
cabal haddock --haddock-for-hackage all
cabal sdist all
read -p "Username: " username
read -sp "Password: " password

# Upload jsonrpc
cabal upload $1 -u "$username" -p "$password" dist-newstyle/sdist/jsonrpc-*.tar.gz
cabal upload $1 -d -u "$username" -p "$password" dist-newstyle/jsonrpc-*-docs.tar.gz
